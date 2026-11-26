# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily weather at the CIMMYT Tlaltizapan office, Morelos, Mexico. Geographic coverage names that place.
# The 1973-1982 cover sheet states TLALTIZAPAN (18°41'N, 99°08'W, written "18.41oN, 99.08oW", 940 masl) = 18.6833 N, 99.1333 W. geo_from_source is TRUE.
# That cover sheet labels radn as MJ/m2, max and min as C, rain as mm, and pan evaporation (Epan) as mm. -99 is missing. Those MJ/m2 values are stored as srad in kJ/m2.
# 1997-2000 workbook sheets use site, day, month, year, tmax, tmin, prec, pan evaporation (Epan), and an unlabeled col 9 whose units change between 1997-1999 and 2000 (not processed).
# 2001-2010 and the file named hourly for 2011 are daily. 2011 Evap is pan evaporation (Epan), not evapotranspiration.
# From 2012, SolarRad is W/m2 and EvapT is evapotranspiration (ETP), as at the Toluca and Agua Fria stations. -9999 is missing.
# Hourly workbooks for 2012-2016 are not used. The daily files cover those years.
# Columns after pan evaporation in the 1973-2000 sheets are not named in the cover sheet.

## ISSUES
# Two corrupt negative daily relative humidity averages (-25.8%, -49.5%, from averaging an unfiltered -9999 flag) are set to NA.
# One negative pan evaporation reading (-8.64 mm in 1996) is set to NA; one 1995 reading of 54.74 mm (likely multi-day accumulation) exceeds the 30 mm/day bound and is left as recorded.

## NOT PROCESSED
# Hourly workbooks for 2012-2016.
# Unnamed columns after pan evaporation in the 1973-2000 workbooks.
# Tlaltizapan hourly data 2012.xlsx: not read. sheet RowDataTlaltizapan2012 (of 2): Year, Month, Day, Hour, Minutes, Minutos, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, .... Observation records are missing from d until this file is standardized.
# Tlaltizapan hourly data 2013.xlsx: not read. sheet RowDataTlaltizapan2013 (of 2): Year, Month, Day, Hour, Minutes, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, WindSpeed, .... Observation records are missing from d until this file is standardized.
# Tlaltizapan hourly data 2014.xlsx: not read. sheet RowDataTlaltizapan2014 (of 2): Year, Month, Day, Hour, Minutes, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, WindSpeed, .... Observation records are missing from d until this file is standardized.
# Tlaltizapan hourly data 2015.xlsx: not read. sheet RowData_Tlaltizapan2015 (of 2): Year, Month, Day, Hour, Minutes, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, WindSpeed, .... Observation records are missing from d until this file is standardized.
# Tlaltizapan hourly data 2016.xlsx: not read. sheet RowData_Tlaltizapan2016 (of 2): Year, Month, day, Hour, Minutes, AM_PM, TempOut, HiTemp, LowTemp, OutHum, DewPt, WindSpeed, .... Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
CIMMYT Climate Data: Meteorological Station at Tlaltizapan CIMMYT Office

CIMMYT has Meteorological Records for each station office in Mexico. Many variables of weather are measured per day (rawdata) from which daily, monthly and yearly summaries are generated. These climate variables are measured by meteorological station and some of them are shown daily or hourly. Some of those variables are: Temperature, Rainfall, Solar Radiation, Relative Humidity, Wind Speed, Evapotranspiration. These files include climate data from 2010 to 2017.
"

	uri <- "hdl:11529/10548096"
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
		carob_completion = 80,
		carob_effort = 1/12
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "-99", "-99.0", "-999", "-999.0", "-9999", "-9999.0")] <- NA_character_
		as.numeric(x)
	}
	pick <- function(r, nms) {
		n <- nms[nms %in% names(r)]
		if (length(n) == 0) return(rep(NA_character_, nrow(r)))
		as.character(r[[n[1]]])
	}
	ymd <- function(year, month, day) {
		date <- rep(NA_character_, length(year))
		ok <- !is.na(year) & !is.na(month) & !is.na(day)
		date[ok] <- sprintf("%04d-%02d-%02d", year[ok], month[ok], day[ok])
		date
	}

	read_old <- function(f) {
		out <- list()
		for (i in 2:15) {
			r <- try(carobiner::read.excel(f, sheet=i, col_names=FALSE), silent=TRUE)
			if (inherits(r, "try-error")) break
			if (ncol(r) < 8) next
			site <- trimws(as.character(r[[1]]))
			if (!any(site == "MXTL", na.rm=TRUE)) next
			year <- as.integer(num(r[[2]]))
			jday <- as.integer(num(r[[3]]))
			date <- rep(NA_character_, nrow(r))
			ok <- !is.na(year) & !is.na(jday) & jday >= 1 & jday <= 366
			for (y in unique(year[ok])) {
				i <- which(ok & year == y)
				origin <- as.Date(sprintf("%04d-01-01", y)) - 1
				date[i] <- as.character(as.Date(jday[i], origin=origin))
			}
			out[[length(out) + 1]] <- data.frame(
				station_id = "Tlaltizapan",
				date = date,
				tmax = num(r[[5]]),
				tmin = num(r[[6]]),
				prec = num(r[[7]]),
				# Cover sheet: radn is MJ/m2.
				srad = num(r[[4]]) * 1000,
				Epan = num(r[[8]])
			)
		}
		if (!length(out)) return(NULL)
		do.call(carobiner::bindr, out)
	}

	read_1997_2000 <- function(f) {
		out <- list()
		for (s in c("1997", "1998", "1999", "2000")) {
			r <- try(carobiner::read.excel(f, sheet=s, col_names=FALSE), silent=TRUE)
			if (inherits(r, "try-error") || ncol(r) < 8) next
			day <- as.integer(num(r[[2]]))
			month <- as.integer(num(r[[3]]))
			year <- as.integer(num(r[[4]]))
			date <- ymd(year, month, day)
			date[!is.na(date) & substr(date, 1, 4) != s] <- NA_character_
			out[[length(out) + 1]] <- data.frame(
				station_id = "Tlaltizapan",
				date = date,
				tmax = num(r[[5]]),
				tmin = num(r[[6]]),
				prec = num(r[[7]]),
				Epan = num(r[[8]])
			)
		}
		if (!length(out)) return(NULL)
		do.call(carobiner::bindr, out)
	}

	read_2001 <- function(f) {
		r <- carobiner::read.excel(f, sheet=1, col_types="text")
		year <- as.integer(num(pick(r, "Year")))
		month <- as.integer(num(pick(r, "Month")))
		day <- as.integer(num(pick(r, "Day")))
		data.frame(
			station_id = "Tlaltizapan",
			date = ymd(year, month, day),
			tmax = num(pick(r, "Max. Temp.")),
			tmin = num(pick(r, "Min. Temp.")),
			rhmx = num(pick(r, "RH_max")),
			rhmn = num(pick(r, "RH_min")),
			prec = num(pick(r, "Rainfall")),
			irad = num(pick(r, "SolarRad")),
			# Rad_Flux is a daily total in MJ/m2, the same quantity as the older radn column.
			srad = num(pick(r, "Rad_Flux")) * 1000
		)
	}

	read_2011 <- function(f) {
		r <- carobiner::read.excel(f, sheet=1, col_types="text")
		year <- as.integer(num(pick(r, "Year")))
		month <- as.integer(num(pick(r, "Month")))
		day <- as.integer(num(pick(r, "Day")))
		data.frame(
			station_id = "Tlaltizapan",
			date = ymd(year, month, day),
			tmax = num(pick(r, "Max. Temp.")),
			tmin = num(pick(r, "Min. Temp.")),
			prec = num(pick(r, "Rainfall")),
			Epan = num(pick(r, "Evap"))
		)
	}

	read_daily <- function(f) {
		r <- carobiner::read.excel(f, sheet=1, col_types="text")
		year <- as.integer(num(pick(r, "Year")))
		month <- as.integer(num(pick(r, "Month")))
		day <- as.integer(num(pick(r, "Day")))
		data.frame(
			station_id = "Tlaltizapan",
			date = ymd(year, month, day),
			temp = num(pick(r, "Avg. Temp.")),
			tmax = num(pick(r, "Max. Temp.")),
			tmin = num(pick(r, "Min. Temp.")),
			prec = num(pick(r, "Rainfall")),
			rhum = num(pick(r, "RH")),
			ETP = num(pick(r, "EvapT")),
			irad = num(pick(r, "SolarRad"))
		)
	}

	bn <- basename(ff)
	parts <- list()
	f1 <- ff[basename(ff) %in% c(
		"Tlaltizapan 1982 - 1973.xls",
		"Tlaltizapan 1992 - 1983.xls",
		"Tlaltizapan 1996 - 1993.xls"
	)]
	if (length(f1)) parts <- c(parts, lapply(f1, read_old))
	f1b <- ff[basename(ff) == "Tlaltizapan 2000 - 1997.xls"]
	if (length(f1b)) parts <- c(parts, lapply(f1b, read_1997_2000))
	f2 <- ff[basename(ff) == "Tlaltizapan hourly data 2001_2010.xlsx"]
	if (length(f2)) parts <- c(parts, lapply(f2, read_2001))
	f3 <- ff[basename(ff) == "Tlaltizapan hourly data 2011.xlsx"]
	if (length(f3)) parts <- c(parts, lapply(f3, read_2011))
	f4 <- ff[basename(ff) %in% c(
		"Tlaltizapan daily data 2012.xlsx",
		"Tlaltizapan daily data 2013.xlsx",
		"Tlaltizapan daily data 2014.xlsx",
		"Tlaltizapan daily data 2015.xlsx",
		"Tlaltizapan daily data 2016.xlsx",
		"Tlaltizapan daily data 2017.xlsx"
	)]
	if (length(f4)) parts <- c(parts, lapply(f4, read_daily))
	parts <- parts[!vapply(parts, is.null, TRUE)]
	wth <- do.call(carobiner::bindr, parts)
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	wth$rhum[!is.na(wth$rhum) & (wth$rhum < 0 | wth$rhum > 100)] <- NA_real_
	wth$Epan[!is.na(wth$Epan) & wth$Epan < 0] <- NA_real_
	meas <- c("temp", "tmax", "tmin", "prec", "rhum", "rhmx", "rhmn", "ETP", "irad", "srad", "Epan")
	ok <- rep(FALSE, nrow(wth))
	for (v in meas) if (v %in% names(wth)) ok <- ok | !is.na(wth[[v]])
	wth <- wth[ok, ]
	wth <- wth[!duplicated(paste(wth$date, wth$station_id), fromLast=TRUE), ]
	wth <- wth[order(wth$date), ]

	d <- data.frame(
		station_id = "Tlaltizapan",
		station_name = "Tlaltizapan",
		country = "Mexico",
		adm1 = "Morelos",
		location = "Tlaltizapan",
		# Cover sheet of the 1973-1982 workbook: 18°41'N, 99°08'W (written "18.41oN, 99.08oW"), 940 masl.
		latitude = round(18 + 41/60, 4),
		longitude = -round(99 + 8/60, 4),
		elevation = 940,
		geo_from_source = TRUE,
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
