# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily and monthly weather at the CIMMYT Agua Fria office, Venustiano Carranza, Puebla, Mexico. Geographic coverage names that place. The files have no coordinates.
# 1969-1999 is year and month only, so the date is year-month. Later files are daily.
# Metadata: temperature C, rain mm, relative humidity %, EvapT is evapotranspiration mm, SolarRad is W/m2 (stored as irad). The 2001-2011 column Evap is pan evaporation (mm), stored as Epan.
# -9999 is the missing-data flag in the metadata. -99 is used the same way for rainfall at the sister Toluca station and is treated as missing here too.

## ISSUES
# One corrupt 2013 relative humidity value (-343.9%, from averaging an unfiltered -9999 flag) is set to NA.
# Some 2017 ETP values reach 11.9 mm/day (slightly above the 10 mm/day vocabulary bound); left as recorded.
# Station coordinates for CIMMYT Agua Fria experimental station are imputed (geo_from_source = FALSE).

## NOT PROCESSED
# Hourly workbooks for 2012-2016. Daily files cover those years.
# Metadata sheets.
# AguaFria hourly data 2012.xlsx: not read. sheet RowDataAguaFria2012 (of 2): Year, Month, Day, Hour, Minutes, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, WindSpeed, .... Observation records are missing from d until this file is standardized.
# AguaFria hourly data 2013.xlsx: not read. sheet RowDataAguaFria2013 (of 2): Year, Month, Day, Hour, Minutes, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, WindSpeed, .... Observation records are missing from d until this file is standardized.
# AguaFria hourly data 2014.xlsx: not read. sheet RowDataAguaFria2014 (of 2): Year, Month, Day, Hour, Minutes, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, WindSpeed, .... Observation records are missing from d until this file is standardized.
# AguaFria hourly data 2015.xlsx: not read. sheet RowData_AguaFria2015 (of 2): Year, Month, Day, Hour, Minutes, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, WindSpeed, .... Observation records are missing from d until this file is standardized.
# AguaFria hourly data 2016.xlsx: not read. sheet RowData_AguaFria2016 (of 2): Year, Month, Day, Hour, Minuts, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, WindSpeed, .... Observation records are missing from d until this file is standardized.


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
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-11-26",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 85,
		carob_effort = 1/12
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
			Epan = num(pick(r, "Evap"))
		)
	}

	ff <- ff[basename(ff) %in% c(
		"Agua Fria daily data 1969_1999.xlsx",
		"Agua Fria DrErnest  daily data 2001_2011.xlsx",
		"AguaFria daily data 2012.xlsx",
		"AguaFria daily data 2013.xlsx",
		"AguaFria daily data 2014.xlsx",
		"AguaFria daily data 2015.xlsx",
		"AguaFria daily data 2016.xlsx",
		"AguaFria daily data 2017.xlsx"
	)]
	ff <- ff[order(basename(ff))]
	wth <- do.call(carobiner::bindr, lapply(ff, read_one))
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	wth$rhum[!is.na(wth$rhum) & (wth$rhum < 0 | wth$rhum > 100)] <- NA_real_
	meas <- c("temp", "tmax", "tmin", "prec", "rhum", "ETP", "irad", "Epan")
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
		latitude = 20.453,
		longitude = -97.641,
		elevation = 110,
		geo_from_source = FALSE,
		geo_source = "CIMMYT Agua Fria experimental station",
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
