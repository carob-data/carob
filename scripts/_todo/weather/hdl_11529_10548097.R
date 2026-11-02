# R script for "carob"
# license: GPL (>=3)

## NOTES
# Half-hourly records at Centro Experimental Iguala, 2012-2013. Geographic coverage names Mexico. The state and city cells are XXXX, and the files have no coordinates.
# The workbook does not label units. Temperature, rain, humidity, pressure, and solar radiation match the Davis Vantage Pro columns used at the sister stations (C, mm, %, hPa, W/m2).
# Wind speed is 0 to 4.9 and the half-hour maximum is 0 to 9.8, so those columns are stored as m/s.
# SolarEnergy matches langleys: a half hour at the recorded W/m2 converts to the recorded SolarEnergy. One langley is 41.84 kJ/m2.
# ET is the station total for that half hour, not a daily total. -9999 is missing.

## ISSUES
# write_files: missing variables: latitude, longitude. contributor:

## NOT PROCESSED
# Wind run, wind chill, heat index, THW, THSW, rain rate, UV, degree-days, indoor sensors, and station diagnostics.


carob_script <- function(path) {

"
CIMMYT Climate Data: MASAGRO Meteorological Station at Centro Experimental Iguala INIFAP

CIMMYT has Meteorological Records for each station office in Mexico. Many variables of weather are measured per day (rawdata) from which daily, monthly and yearly summaries are generated. These climate variables are measured by meteorological station and some of them are shown daily or hourly. Some of those variables are: Temperature, Rainfall, Solar Radiation, Relative Humidity, Wind Speed, Evapotranspiration. These files include climate data from 2012 to 2013.
"

	uri <- "hdl:11529/10548097"
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
		carob_completion = 70,
		carob_effort = 0.4
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "-99", "-99.0", "-999", "-999.0", "-9999", "-9999.0")] <- NA_character_
		as.numeric(x)
	}
	compass_deg <- function(x) {
		x <- toupper(trimws(as.character(x)))
		x[!nzchar(x) | x %in% c("NA", "NULL", "---")] <- NA_character_
		rose <- c(
			N=0, NNE=22.5, NE=45, ENE=67.5, E=90, ESE=112.5, SE=135, SSE=157.5,
			S=180, SSW=202.5, SW=225, WSW=247.5, W=270, WNW=292.5, NW=315, NNW=337.5
		)
		out <- rep(NA_real_, length(x))
		known <- !is.na(x) & x %in% names(rose)
		out[known] <- unname(rose[x[known]])
		out
	}
	clock <- function(hour, minute, ampm) {
		h <- as.integer(num(hour))
		m <- as.integer(num(minute))
		ap <- toupper(trimws(as.character(ampm)))
		pm <- !is.na(ap) & ap == "PM" & !is.na(h) & h < 12
		am <- !is.na(ap) & ap == "AM" & !is.na(h) & h == 12
		h[pm] <- h[pm] + 12
		h[am] <- 0
		out <- rep(NA_character_, length(h))
		ok <- !is.na(h) & !is.na(m)
		out[ok] <- sprintf("%02d:%02d:00", h[ok], m[ok])
		out
	}

	read_one <- function(f) {
		r <- carobiner::read.excel(f, sheet=1, col_types="text")
		year <- as.integer(num(r$Year))
		month <- as.integer(num(r$Month))
		day <- as.integer(num(r$Day))
		date <- rep(NA_character_, nrow(r))
		ok <- !is.na(year) & !is.na(month) & !is.na(day)
		date[ok] <- sprintf("%04d-%02d-%02d", year[ok], month[ok], day[ok])
		data.frame(
			station_id = "Iguala",
			date = date,
			time = clock(r$Hour, r$Minutes, r$AM_PM),
			temp = num(r$TempOut),
			tmax = num(r$HiTemp),
			tmin = num(r$LowTemp),
			rhum = num(r$OutHum),
			dewp = num(r$DewPt),
			wspd = num(r$WindSpeed),
			wdir = compass_deg(r$WindDir),
			wspdmx = num(r$HiSpeed),
			prec = num(r$Rain),
			irad = num(r$SolarRad),
			# Davis SolarEnergy is langleys. 1 ly = 41.84 kJ/m2.
			srad = num(r$SolarEnergy) * 41.84,
			ETo = num(r$ET),
			pressure = num(r$Bar)
		)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	ff <- ff[order(basename(ff))]
	wth <- do.call(carobiner::bindr, lapply(ff, read_one))
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	meas <- c("temp", "tmax", "tmin", "rhum", "dewp", "wspd", "prec", "irad", "srad", "ETo", "pressure")
	ok <- rep(FALSE, nrow(wth))
	for (v in meas) ok <- ok | !is.na(wth[[v]])
	wth <- wth[ok, ]
	wth <- wth[order(wth$date, wth$time), ]

	d <- data.frame(
		station_id = "Iguala",
		station_name = "Centro Experimental Iguala",
		country = "Mexico",
		location = "Iguala",
		geo_from_source = FALSE,
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
