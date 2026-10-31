# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily weather at the CIMMYT Tlaltizapan office, Morelos, Mexico. Geographic coverage names that place.
# The 1973-1982 cover sheet states TLALTIZAPAN (18.41 N, 99.08 W, 940 masl). Longitude is west, so it is stored negative. geo_from_source is TRUE.
# That cover sheet labels radn as MJ/m2, max and min as C, rain as mm, and pan evaporation as mm. -99 is missing. Those MJ/m2 values are stored as srad in kJ/m2.
# 2001-2010 and the file named hourly for 2011 are daily. 2011 Evap is pan evaporation, not evapotranspiration.
# From 2012, SolarRad is W/m2 and EvapT is evapotranspiration, as at the Toluca and Agua Fria stations. -9999 is missing.
# Hourly workbooks for 2012-2016 are not used. The daily files cover those years.
# Columns after pan evaporation in the 1973-2000 sheets are not named in the cover sheet.

## ISSUES
# Relative humidity includes values below 0. They were left as recorded.
# write_files: unknown variables: evaporation_mm_. out of bounds: rhum (-49.493, 96.682). contributor:

## NEW VARIABLES
# evaporation_mm_: pan evaporation (mm) from the 1973-2000 and 2011 files.

## NOT PROCESSED
# Hourly workbooks for 2012-2016.
# Unnamed columns after pan evaporation in the 1973-2000 workbooks.


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
				evaporation_mm_ = num(r[[8]])
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
			evaporation_mm_ = num(pick(r, "Evap"))
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
	old <- ff[grepl("^Tlaltizapan (19|20)", bn) & grepl("\\.xls$", bn, ignore.case=TRUE)]
	if (length(old)) parts <- c(parts, lapply(old, read_old))
	f2001 <- ff[grepl("2001_2010", bn)]
	if (length(f2001)) parts <- c(parts, lapply(f2001, read_2001))
	f2011 <- ff[grepl("hourly data 2011", bn)]
	if (length(f2011)) parts <- c(parts, lapply(f2011, read_2011))
	daily <- ff[grepl("daily data", bn)]
	if (length(daily)) parts <- c(parts, lapply(daily, read_daily))
	parts <- parts[!vapply(parts, is.null, TRUE)]
	wth <- do.call(carobiner::bindr, parts)
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	meas <- c("temp", "tmax", "tmin", "prec", "rhum", "rhmx", "rhmn", "ETP", "irad", "srad", "evaporation_mm_")
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
		# Cover sheet of the 1973-1982 workbook: 18.41 N, 99.08 W, 940 masl.
		latitude = 18.41,
		longitude = -99.08,
		elevation = 940,
		geo_from_source = TRUE,
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
