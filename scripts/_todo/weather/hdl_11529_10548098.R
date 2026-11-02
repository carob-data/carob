# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily weather at the CENEB station. geographicCoverage country is Mexico. The state cell is the placeholder XXXX and is not used.
# The 1986-1992 explanation sheet gives Block 910, Ciudad Obregon: 27.20 N, 109.54 W, 38 m. Those coordinates are stored. geo_from_source is TRUE.
# -99 and -9999 are missing. In 2004-2009, 99 is also missing for temperature and for radiation above 40 MJ/m2. One rainfall value was a station pressure and is left empty. Station pressure outside 900-1100 hPa is left empty.
# From 2012, SolarRad is W/m2 (irad) and EvapT is evapotranspiration (mm), as the metadata sheet states.
# 2004-2009 Rad_Flux is MJ/m2 and is stored as srad in kJ/m2.
# Older Evap is evaporation (mm), not evapotranspiration. Wind is at 0.5 m.
# Where 1989 is in both the multi-year workbook and the 1989 file, the 1989 file is kept.

## ISSUES
# write_files: unknown variables: evaporation_mm_.

## NEW VARIABLES
# evaporation_mm_: Evap column (mm) in the files from 1986 to 2003. The metadata calls it evaporation.

## NOT PROCESSED
# Hourly workbooks for 2012-2016. Daily files cover those years.
# Wet-bulb temperature, environment minimum temperature, vapor tension (mm), insolation (hr.min), and sea-level pressure.
# Radiation in watt-hours, when a daily MJ column is also present.


carob_script <- function(path) {

"
CIMMYT Climate Data: Meteorological Station at CENEB CIMMYT Office

CIMMYT has Meteorological Records for each station office in Mexico. Many variables of weather are measured per day (rawdata) from which daily, monthly and yearly summaries are generated. These climate variables are measured by meteorological station and some of them are shown daily or hourly. Some of those variables are: Temperature, Rainfall, Solar Radiation, Relative Humidity, Wind Speed, Evapotranspiration. These files include climate data from 1986 to 2017.
"

	uri <- "hdl:11529/10548098"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.5
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "-99", "-99.0", "-999", "-999.0", "-9999", "-9999.0")] <- NA_character_
		as.numeric(x)
	}
	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) {
			x[x %in% c(-99, -999, -9999)] <- NA_real_
			out <- rep(NA_character_, length(x))
			# The 1986-1992 workbook stores dates as seconds since 1970-01-01.
			sec <- !is.na(x) & x > 1e8
			if (any(sec)) out[sec] <- as.character(as.Date(x[sec] / 86400, origin="1970-01-01"))
			day <- !is.na(x) & x > 20000 & x < 80000
			if (any(day)) out[day] <- as.character(as.Date(x[day], origin="1899-12-30"))
			return(out)
		}
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
		if (any(iso)) out[iso] <- substr(x[iso], 1, 10)
		out
	}
	pick <- function(r, nms) {
		n <- nms[nms %in% names(r)]
		if (length(n) == 0) return(rep(NA_character_, nrow(r)))
		as.character(r[[n[1]]])
	}
	blank <- function(n) rep(NA_real_, n)

	from_ymd <- function(r) {
		year <- as.integer(num(pick(r, "Year")))
		month <- as.integer(num(pick(r, "Month")))
		day <- as.integer(num(pick(r, "Day")))
		date <- rep(NA_character_, nrow(r))
		ok <- !is.na(year) & !is.na(month) & !is.na(day)
		date[ok] <- sprintf("%04d-%02d-%02d", year[ok], month[ok], day[ok])
		temp <- num(pick(r, c("Avg. Temp.", "Dry_bulb")))
		rad <- num(pick(r, "Rad_Flux_MJ_m2"))
		rad[is.na(rad) | rad < 0 | rad > 40] <- NA_real_
		srad <- rad * 1000
		data.frame(
			station_id = "CENEB",
			date = date,
			temp = temp,
			tmax = num(pick(r, "Max. Temp.")),
			tmin = num(pick(r, "Min. Temp.")),
			prec = num(pick(r, "Rainfall")),
			rhum = num(pick(r, "RH")),
			rhmx = num(pick(r, "RH_max")),
			rhmn = num(pick(r, "RH_min")),
			ETP = num(pick(r, "EvapT")),
			irad = num(pick(r, "SolarRad")),
			srad = srad,
			wspd = num(pick(r, "WIN_SPD_1_2")),
			pressure = num(pick(r, "Pressue_station")),
			evaporation_mm_ = num(pick(r, "Evap"))
		)
	}

	read_named <- function(f, sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, col_types="text")
		from_ymd(r)
	}

	read_position <- function(f, sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE)
		at <- function(i) {
			if (i > ncol(r)) return(blank(nrow(r)))
			num(r[[i]])
		}
		srad <- at(17) * 1000
		data.frame(
			station_id = "CENEB",
			date = as_date(r[[1]]),
			temp = at(3),
			tmax = at(5),
			tmin = at(6),
			prec = at(12),
			rhum = at(9),
			rhmx = blank(nrow(r)),
			rhmn = blank(nrow(r)),
			ETP = blank(nrow(r)),
			irad = blank(nrow(r)),
			srad = srad,
			wspd = at(13),
			pressure = at(10),
			evaporation_mm_ = at(15)
		)
	}

	named <- c(
		"Obegon   daily data 1989.xlsx" = "Obegon_1989",
		"Obregon  daily data 1993_2003.xlsx" = "Obregon1993_2003",
		"Obregon  daily data 2004_2009.xlsx" = "Obregon2004_2009",
		"CENEB daily data 2012.xlsx" = "CENEBDiarios2012",
		"CENEB daily data 2013.xlsx" = "CENEBDiarios2013",
		"CENEB daily data 2014.xlsx" = "CENEBDiarios2014",
		"CENEB daily data 2015.xlsx" = "DiariosCENEB2015",
		"CENEB daily data 2016.xlsx" = "DiariosCENEB2016",
		"CENEB daily data 2017.xlsx" = "Ceneb2017"
	)
	parts <- list()
	old <- ff[basename(ff) == "Obregon  daily data 1992-1986.xls"]
	for (yr in as.character(1992:1986)) {
		parts[[length(parts) + 1]] <- read_position(old, yr)
	}
	for (i in seq_along(named)) {
		f <- ff[basename(ff) == names(named)[i]]
		parts[[length(parts) + 1]] <- read_named(f, named[i])
	}
	wth <- do.call(carobiner::bindr, parts)
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	meas <- c("temp", "tmax", "tmin", "prec", "rhum", "rhmx", "rhmn", "ETP", "irad", "srad", "wspd", "pressure", "evaporation_mm_")
	ok <- rep(FALSE, nrow(wth))
	for (v in meas) ok <- ok | !is.na(wth[[v]])
	wth <- wth[ok, ]
	wth <- wth[!duplicated(wth$date, fromLast=TRUE), ]
	wth <- wth[order(wth$date), ]
	# 99 is a missing-temperature flag in 2004-2009. One rainfall cell is a station pressure. Pressures outside 900-1100 hPa are not station pressure.
	for (v in c("temp", "tmax", "tmin")) wth[[v]][wth[[v]] %in% c(99, -99)] <- NA_real_
	wth$prec[wth$prec > 400] <- NA_real_
	wth$pressure[wth$pressure < 900 | wth$pressure > 1100] <- NA_real_
	wth$srad[wth$srad < 0 | wth$srad > 40000] <- NA_real_

	d <- data.frame(
		station_id = "CENEB",
		station_name = "CENEB",
		country = "Mexico",
		location = "Obregon",
		latitude = 27.20,
		longitude = -109.54,
		elevation = 38,
		geo_from_source = TRUE,
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
