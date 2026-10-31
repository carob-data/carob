# R script for "carob"
# license: GPL (>=3)

## NOTES
# Half-hourly Davis station record for CEVAMEX in 2015. Geographic coverage is Coatlinchan, Texcoco, Estado de Mexico. The file has no coordinates.
# The metadata sheet gives TempOut, HiTemp, and LowTemp in C, OutHum in percent, DewPt in C, WindSpeed in km/h, SolarRad in W/m2, ET in mm, Bar in hPa, and RainRate in mm/h. The Rain unit cell is blank. Rain reaches 21.4 in one interval, which is a millimetre amount, not inches, so daily prec is the sum of Rain.
# Daily values are the interval mean, max, min, or sum. ETP is the sum of interval ET in mm, stored as mm/day. Wind speed is converted from km/h to m/s. -9999 is missing.

## ISSUES
# write_files: missing variables: latitude, longitude contributor:

## NOT PROCESSED
# Indoor console readings, heat index, wind chill, UV, wind run, and wind direction.
# The metadata sheet.


carob_script <- function(path) {

"
CIMMYT Climate Data: MASAGRO Meteorological Station at Campo Experimental Valles Altos (CEVAMEX)

CIMMYT has Meteorological Records for each station office in Mexico. Many variables of weather are measured per day (rawdata) from which daily, monthly and yearly summaries are generated. These climate variables are measured by meteorological station and some of them are shown daily or hourly. Some of those variables are: Temperature, Rainfall, Solar Radiation, Relative Humidity, Wind Speed, Evapotranspiration. These files include climate data from 2015.
"

	uri <- "hdl:11529/10548095"
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
		x[x %in% c("", "NA", "na", "-99", "-999", "-9999", "-9999.0")] <- NA_character_
		as.numeric(x)
	}
	f <- ff[grepl("CEVAMEX", basename(ff))][1]
	r <- carobiner::read.excel(f, sheet=1)
	year <- as.integer(num(r$Year))
	month <- as.integer(num(r$Month))
	day <- as.integer(num(r$Day))
	hour <- as.integer(num(r$Hour))
	ampm <- toupper(trimws(as.character(r$AM_PM)))
	hour[!is.na(hour) & ampm == "PM" & hour < 12] <- hour[!is.na(hour) & ampm == "PM" & hour < 12] + 12
	hour[!is.na(hour) & ampm == "AM" & hour == 12] <- 0
	ok <- !is.na(year) & !is.na(month) & !is.na(day)
	date <- rep(NA_character_, nrow(r))
	date[ok] <- sprintf("%04d-%02d-%02d", year[ok], month[ok], day[ok])
	tmp <- data.frame(
		date = date,
		temp = num(r$TempOut),
		hi = num(r$HiTemp),
		lo = num(r$LowTemp),
		rhum = num(r$OutHum),
		dewp = num(r$DewPt),
		prec = num(r$Rain),
		irad = num(r$SolarRad),
		ET = num(r$ET),
		wspd = num(r$WindSpeed) / 3.6,
		pressure = num(r$Bar),
		hour = hour
	)
	tmp <- tmp[!is.na(tmp$date), ]
	tmp <- tmp[order(tmp$date, tmp$hour), ]
	dates <- sort(unique(tmp$date))
	agg <- function(x, fun) tapply(x, tmp$date, fun)
	mean_ok <- function(x) {
		z <- as.numeric(agg(x, function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm=TRUE)))
		z
	}
	sum_ok <- function(x) {
		as.numeric(agg(x, function(v) if (all(is.na(v))) NA_real_ else sum(v, na.rm=TRUE)))
	}
	max_ok <- function(x) {
		as.numeric(agg(x, function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm=TRUE)))
	}
	min_ok <- function(x) {
		as.numeric(agg(x, function(v) if (all(is.na(v))) NA_real_ else min(v, na.rm=TRUE)))
	}
	wth <- data.frame(
		station_id = "CEVAMEX",
		date = dates,
		temp = mean_ok(tmp$temp),
		tmax = max_ok(tmp$hi),
		tmin = min_ok(tmp$lo),
		prec = sum_ok(tmp$prec),
		rhum = mean_ok(tmp$rhum),
		dewp = mean_ok(tmp$dewp),
		irad = mean_ok(tmp$irad),
		ETP = sum_ok(tmp$ET),
		wspd = mean_ok(tmp$wspd),
		pressure = mean_ok(tmp$pressure)
	)

	d <- data.frame(
		station_id = "CEVAMEX",
		station_name = "CEVAMEX",
		country = "Mexico",
		adm1 = "Estado de Mexico",
		adm2 = "Texcoco",
		location = "Coatlinchan",
		geo_from_source = FALSE,
		is_survey = FALSE,
		on_farm = FALSE
	)
	carobiner::write_files(path, meta, d, wth=wth)
}
