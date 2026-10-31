# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily and monthly weather at the CIMMYT Agua Fria office, Venustiano Carranza, Puebla, Mexico. Geographic coverage names that place. The files have no coordinates.
# 1969-1999 is year and month only, so the date is year-month. Later files are daily.
# Metadata: temperature C, rain mm, relative humidity %, EvapT is evapotranspiration mm, SolarRad is W/m2 (stored as irad). The 2001-2011 column Evap is labeled Evaporation, not evapotranspiration.
# -9999 is the missing-data flag in the metadata. -99 is used the same way for rainfall at the sister Toluca station and is treated as missing here too.

## ISSUES
# One relative humidity in 2013 is -343.9 and one evapotranspiration is 11.9 mm. Those values are left as recorded.
# write_files: missing variables: latitude, longitude. unknown variables: evaporation_mm_. out of bounds: rhum (-343.894, 96.895), ETP (0, 11.9).

## NEW VARIABLES
# evaporation_mm_: pan evaporation (mm) from the 2001-2011 Evap column.

## NOT PROCESSED
# Hourly workbooks for 2012-2016. Daily files cover those years.
# Metadata sheets.


carob_script <- function(path) {

"
CIMMYT Climate Data: Meteorological Station at Agua Fria CIMMYT Office

CIMMYT has Meteorological Records for each station office in Mexico. Many variables of weather are measured per day (rawdata) from which daily, monthly and yearly summaries are generated. These climate variables are measured by meteorological station and some of them are shown daily or hourly. Some of those variables are: Temperature, Rainfall, Solar Radiation, Relative Humidity, Wind Speed, Evapotranspiration. These files include climate data from 1969 to 2017.
"

	uri <- "hdl:11529/10548093"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
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
		mon <- !is.na(year) & !is.na(month) & is.na(day)
		dayok <- !is.na(year) & !is.na(month) & !is.na(day)
		date[mon] <- sprintf("%04d-%02d", year[mon], month[mon])
		date[dayok] <- sprintf("%04d-%02d-%02d", year[dayok], month[dayok], day[dayok])
		# The 2001-2011 workbook labels Evap as evaporation. Later files use EvapT for evapotranspiration.
		data.frame(
			station_id = "Agua Fria",
			date = date,
			temp = num(pick(r, "Avg. Temp.")),
			tmax = num(pick(r, "Max. Temp.")),
			tmin = num(pick(r, "Min. Temp.")),
			prec = num(pick(r, "Rainfall")),
			rhum = num(pick(r, "RH")),
			ETP = num(pick(r, "EvapT")),
			irad = num(pick(r, "SolarRad")),
			evaporation_mm_ = num(pick(r, "Evap"))
		)
	}

	ff <- ff[grepl("daily|DrErnest", basename(ff), ignore.case=TRUE)]
	ff <- ff[!grepl("hourly", basename(ff), ignore.case=TRUE)]
	ff <- ff[order(basename(ff))]
	wth <- do.call(carobiner::bindr, lapply(ff, read_one))
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	meas <- c("temp", "tmax", "tmin", "prec", "rhum", "ETP", "irad", "evaporation_mm_")
	ok <- rep(FALSE, nrow(wth))
	for (v in meas) ok <- ok | !is.na(wth[[v]])
	wth <- wth[ok, ]
	wth <- wth[!duplicated(wth$date, fromLast=TRUE), ]
	wth <- wth[order(wth$date), ]

	d <- data.frame(
		station_id = "Agua Fria",
		station_name = "Agua Fria",
		country = "Mexico",
		adm1 = "Puebla",
		adm2 = "Venustiano Carranza",
		location = "Agua Fria",
		geo_from_source = FALSE,
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
