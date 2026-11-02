# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily weather at the CIMMYT Toluca office, Estado de Mexico, Mexico. Geographic coverage names Toluca. The files have no coordinates.
# 1975-2009 total flux is MJ/m2 and is stored as srad in kJ/m2. Maximum radiation rate is kW/m2 and is not mean irradiance.
# -9999 is missing. Rainfall also uses -99, which is not a rainfall amount, so that flag is missing too.
# From 2012, SolarRad is W/m2 (irad) and EvapT is evapotranspiration (mm). Older Evap columns are not labeled as evapotranspiration in the metadata sheet.
# Where a date is in both 2010-2012 and the 2012 daily file, the 2012 daily file is kept.

## ISSUES
# One maximum temperature is 215 C. Some maximum humidity values are above 100. Some evapotranspiration values exceed 10 mm. Those values are left as recorded.
# write_files: missing variables: latitude, longitude. unknown variables: evaporation_mm_. out of bounds: tmax (0, 215), rhmx (60.06, 114.9), ETP (0, 21.72).

## NEW VARIABLES
# evaporation_mm_: Evap column (mm) in the 1975-2009 and 2010-2012 files. The metadata unit cell is blank, and the 2001 Agua Fria sister file labels the same name as evaporation.

## NOT PROCESSED
# Hourly workbooks for 2012-2015. Daily files cover those years.
# Rad_rate_Max: maximum radiation rate (kW/m2), not mean irradiance.
# Metadata sheets.


carob_script <- function(path) {

"
CIMMYT Climate Data: Meteorological Station at Toluca CIMMYT Office

CIMMYT has Meteorological Records for each station office in Mexico. Many variables of weather are measured per day (rawdata) from which daily, monthly and yearly summaries are generated. These climate variables are measured by meteorological station and some of them are shown daily or hourly. Some of those variables are: Temperature, Rainfall, Solar Radiation, Relative Humidity, Wind Speed, Evapotranspiration. These files include climate data from 1975 to 2016.
"

	uri <- "hdl:11529/10548092"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.4
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "fini", "NA", "-99", "-99.0", "-999", "-999.0", "-9999", "-9999.0")] <- NA_character_
		as.numeric(x)
	}
	pick <- function(r, nms) {
		n <- nms[nms %in% names(r)]
		if (length(n) == 0) return(rep(NA_character_, nrow(r)))
		as.character(r[[n[1]]])
	}

	read_one <- function(f) {
		r <- carobiner::read.excel(f, sheet=1, col_types="text")
		year <- as.integer(num(pick(r, "Year")))
		month <- as.integer(num(pick(r, "Month")))
		day <- as.integer(num(pick(r, c("Day", "day"))))
		date <- rep(NA_character_, nrow(r))
		dayok <- !is.na(year) & !is.na(month) & !is.na(day)
		date[dayok] <- sprintf("%04d-%02d-%02d", year[dayok], month[dayok], day[dayok])
		srad <- num(pick(r, "Rad_Flux_TOTAL")) * 1000
		data.frame(
			station_id = "Toluca",
			date = date,
			temp = num(pick(r, "Avg. Temp.")),
			tmax = num(pick(r, "Max. Temp.")),
			tmin = num(pick(r, "Min. Temp.")),
			prec = num(pick(r, "Rainfall")),
			rhum = num(pick(r, "RH")),
			rhmx = num(pick(r, "RH_max")),
			rhmn = num(pick(r, "RH_min")),
			ETP = num(pick(r, "EvapT")),
			irad = num(pick(r, "SolarRad")),
			srad = srad,
			evaporation_mm_ = num(pick(r, "Evap"))
		)
	}

	ff <- ff[grepl("daily|1975|2010", basename(ff), ignore.case=TRUE)]
	ff <- ff[!grepl("hourly", basename(ff), ignore.case=TRUE)]
	ff <- ff[order(basename(ff))]
	wth <- do.call(carobiner::bindr, lapply(ff, read_one))
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	meas <- c("temp", "tmax", "tmin", "prec", "rhum", "rhmx", "rhmn", "ETP", "irad", "srad", "evaporation_mm_")
	ok <- rep(FALSE, nrow(wth))
	for (v in meas) ok <- ok | !is.na(wth[[v]])
	wth <- wth[ok, ]
	wth <- wth[!duplicated(wth$date, fromLast=TRUE), ]
	wth <- wth[order(wth$date), ]

	d <- data.frame(
		station_id = "Toluca",
		station_name = "Toluca",
		country = "Mexico",
		adm1 = "Estado de Mexico",
		location = "Toluca",
		geo_from_source = FALSE,
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
