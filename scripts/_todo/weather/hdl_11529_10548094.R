# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily weather at El Batan, Estado de Mexico. The explanation sheets give BATAN (19.31 N, 98.50 W, 2249 masl). Longitude is west. geo_from_source is TRUE.
# 2012-2017 workbooks use temperature C, rainfall mm, RH percent, EvapT mm, and SolarRad W/m2.
# 1997-1998 sheets are site, day, month, year, max temp, min temp, rainfall mm, and pan evaporation. -99 is missing.
# 1977-1996 sheets are site, year, day-of-year, radiation MJ/m2, max temp, min temp, rain mm, pan evaporation, then month and day. Radiation is stored as srad in kJ/m2. Wind run is km/day and is stored as m/s.
# 1973-1976 sheets use an Excel date, day-of-year, radiation MJ/m2, max and min temperature, rain mm, and pan evaporation.
# 1999-2011 sheets use an Excel date or day-of-year, max and min temperature, max and min RH, daily radiation MJ/m2, and rain mm. The date is the sheet year plus day-of-year when that day is present, because some Excel serials are one year early. A sheet only keeps dates in its own year.
# -99 and -9999 are missing. Hourly workbooks are not used.

## ISSUES
# write_files: unknown variables: evaporation_mm_ contributor:

## NEW VARIABLES
# evaporation_mm_: pan evaporation (mm) on the 1973-1998 sheets. The 1986 explanation names Pan evap beside rainfall in mm. The 1997 explanation names Pan_Evap and does not print a unit.

## NOT PROCESSED
# Hourly and Campbell hourly workbooks.
# Maximum radiation rate on the 1999-2011 sheets. The 1999 header says W/m2 and the 2011 header says kW/m2, and it is a maximum rate rather than mean irradiance.
# Radiation on the 1997-1998 sheets, which has no unit.
# Wet-bulb and dry-bulb temperature, vapour-pressure deficit, the unused wind-run height, and sun hours.


carob_script <- function(path) {

"
CIMMYT Climate Data: Meteorological Station at El Batan CIMMYT Office

CIMMYT has Meteorological Records for each station office in Mexico. Many variables of weather are measured per day (rawdata) from which daily, monthly and yearly summaries are generated. These climate variables are measured by meteorological station and some of them are shown daily or hourly. Some of those variables are: Temperature, Rainfall, Solar Radiation, Relative Humidity, Wind Speed, Evapotranspiration. These files include climate data from 1973 to 2017.
"

	uri <- "hdl:11529/10548094"
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
		if (inherits(x, "Date")) x <- as.character(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "na", "-99", "-99.0", "-999", "-9999", "-9999.0")] <- NA_character_
		x[grepl("[[:alpha:]]", x)] <- NA_character_
		as.numeric(x)
	}
	pick <- function(r, nms) {
		n <- nms[nms %in% names(r)]
		if (length(n) == 0) return(rep(NA_character_, nrow(r)))
		as.character(r[[n[1]]])
	}
	na_n <- function(n) rep(NA_real_, n)
	excel_date <- function(x) {
		if (inherits(x, "Date")) return(as.character(x))
		if (inherits(x, "POSIXt")) return(as.character(as.Date(x)))
		n <- num(x)
		out <- rep(NA_character_, length(n))
		ok <- !is.na(n) & n > 20000 & n < 60000
		if (any(ok)) out[ok] <- as.character(as.Date(n[ok], origin="1899-12-30"))
		out
	}
	ymd <- function(year, month, day) {
		year <- as.integer(year)
		month <- as.integer(month)
		day <- as.integer(day)
		out <- rep(NA_character_, length(year))
		md <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
		ok <- !is.na(year) & !is.na(month) & !is.na(day) & month >= 1 & month <= 12
		leap <- ok & month == 2 & ((year %% 400 == 0) | (year %% 4 == 0 & year %% 100 != 0))
		maxd <- rep(NA_integer_, length(year))
		maxd[ok] <- md[month[ok]]
		maxd[leap] <- 29
		ok <- ok & day >= 1 & day <= maxd
		if (any(ok)) out[ok] <- sprintf("%04d-%02d-%02d", year[ok], month[ok], day[ok])
		out
	}
	jday_date <- function(year, jday) {
		year <- as.integer(year)
		jday <- as.integer(jday)
		out <- rep(NA_character_, length(jday))
		leap <- !is.na(year) & ((year %% 400 == 0) | (year %% 4 == 0 & year %% 100 != 0))
		ok <- !is.na(year) & !is.na(jday) & jday >= 1 & (jday <= 365 | (jday == 366 & leap))
		if (any(ok)) {
			origin <- as.Date(sprintf("%04d-01-01", year[ok]))
			out[ok] <- as.character(origin + (jday[ok] - 1))
		}
		out
	}
	wind_ms <- function(km_day) num(km_day) / 86.4
	prefer_wind <- function(near, far) {
		w <- wind_ms(near)
		alt <- wind_ms(far)
		w[is.na(w)] <- alt[is.na(w)]
		w
	}
	pack <- function(date, temp, tmax, tmin, prec, rhum, rhmx, rhmn, ETP, irad, srad, wspd, evaporation_mm_) {
		keep <- !is.na(date)
		if (!any(keep)) return(NULL)
		data.frame(
			date = date[keep],
			temp = temp[keep],
			tmax = tmax[keep],
			tmin = tmin[keep],
			prec = prec[keep],
			rhum = rhum[keep],
			rhmx = rhmx[keep],
			rhmn = rhmn[keep],
			ETP = ETP[keep],
			irad = irad[keep],
			srad = srad[keep],
			wspd = wspd[keep],
			evaporation_mm_ = evaporation_mm_[keep]
		)
	}
	read_new <- function(f) {
		r <- carobiner::read.excel(f, sheet=1, col_types="text")
		year <- num(pick(r, "Year"))
		month <- num(pick(r, "Month"))
		day <- num(pick(r, "Day"))
		n <- nrow(r)
		pack(
			ymd(year, month, day),
			num(pick(r, "Avg. Temp.")),
			num(pick(r, "Max. Temp.")),
			num(pick(r, "Min. Temp.")),
			num(pick(r, "Rainfall")),
			num(pick(r, "RH")),
			na_n(n), na_n(n),
			num(pick(r, "EvapT")),
			num(pick(r, "SolarRad")),
			na_n(n), na_n(n), na_n(n)
		)
	}
	sheets_for <- function(bn) {
		if (grepl("1976 - 1973", bn)) return(as.character(1976:1973))
		if (grepl("1986 - 1977", bn)) return(as.character(1986:1977))
		if (grepl("1996 - 1987", bn)) return(as.character(1996:1987))
		if (grepl("1998 - 1997", bn)) return(c("1998", "1997"))
		if (grepl("2011 - 1999", bn)) return(as.character(2011:1999))
		NULL
	}
	in_sheet_year <- function(date, sheet) {
		date[!is.na(date) & substr(date, 1, 4) != sheet] <- NA_character_
		date
	}
	read_manual <- function(r, sheet) {
		n <- nrow(r)
		date <- in_sheet_year(ymd(num(r[[4]]), num(r[[3]]), num(r[[2]])), sheet)
		pack(date, na_n(n), num(r[[5]]), num(r[[6]]), num(r[[7]]), na_n(n), na_n(n), na_n(n), na_n(n), na_n(n), na_n(n), na_n(n), num(r[[8]]))
	}
	read_mid <- function(r, sheet) {
		n <- nrow(r)
		date <- in_sheet_year(ymd(num(r[[2]]), num(r[[15]]), num(r[[16]])), sheet)
		pack(date, na_n(n), num(r[[5]]), num(r[[6]]), num(r[[7]]), na_n(n), na_n(n), na_n(n), na_n(n), na_n(n), num(r[[4]]) * 1000, prefer_wind(r[[13]], r[[12]]), num(r[[8]]))
	}
	read_early <- function(r, sheet) {
		n <- nrow(r)
		date <- in_sheet_year(excel_date(r[[1]]), sheet)
		pack(date, na_n(n), num(r[[4]]), num(r[[5]]), num(r[[6]]), na_n(n), na_n(n), na_n(n), na_n(n), na_n(n), num(r[[3]]) * 1000, prefer_wind(r[[12]], r[[11]]), num(r[[7]]))
	}
	read_flux <- function(r, sheet) {
		n <- nrow(r)
		date <- jday_date(rep(as.integer(sheet), n), num(r[[2]]))
		miss <- is.na(date)
		date[miss] <- excel_date(r[[1]])[miss]
		date <- in_sheet_year(date, sheet)
		pack(date, na_n(n), num(r[[3]]), num(r[[4]]), num(r[[9]]), na_n(n), num(r[[5]]), num(r[[6]]), na_n(n), na_n(n), num(r[[8]]) * 1000, na_n(n), na_n(n))
	}
	read_old_sheet <- function(f, sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE)
		bn <- basename(f)
		if (grepl("1998 - 1997", bn)) return(read_manual(r, sheet))
		if (grepl("1986 - 1977", bn) || grepl("1996 - 1987", bn)) return(read_mid(r, sheet))
		if (grepl("1976 - 1973", bn)) return(read_early(r, sheet))
		read_flux(r, sheet)
	}
	read_old <- function(f) {
		sh <- sheets_for(basename(f))
		parts <- lapply(sh, function(s) read_old_sheet(f, s))
		parts <- parts[!vapply(parts, is.null, logical(1))]
		if (!length(parts)) return(NULL)
		do.call(carobiner::bindr, parts)
	}

	daily <- ff[grepl("daily", basename(ff), ignore.case=TRUE)]
	daily <- daily[!grepl("hourly", basename(daily), ignore.case=TRUE)]
	daily <- daily[order(basename(daily))]
	parts <- lapply(daily, function(f) {
		if (grepl("\\.xlsx$", basename(f), ignore.case=TRUE)) read_new(f) else read_old(f)
	})
	parts <- parts[!vapply(parts, is.null, logical(1))]
	wth <- do.call(carobiner::bindr, parts)
	wth$station_id <- "El Batan"
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	meas <- c("temp", "tmax", "tmin", "prec", "rhum", "rhmx", "rhmn", "ETP", "irad", "srad", "wspd", "evaporation_mm_")
	ok <- rep(FALSE, nrow(wth))
	for (v in meas) ok <- ok | !is.na(wth[[v]])
	wth <- wth[ok, ]
	wth <- wth[!duplicated(wth$date, fromLast=TRUE), ]
	wth <- wth[order(wth$date), ]

	d <- data.frame(
		station_id = "El Batan",
		station_name = "El Batan",
		country = "Mexico",
		adm1 = "Estado de Mexico",
		adm2 = "Texcoco",
		location = "El Batan",
		latitude = 19.31,
		longitude = -98.50,
		elevation = 2249,
		geo_from_source = TRUE,
		is_survey = FALSE,
		on_farm = FALSE
	)
	carobiner::write_files(path, meta, d, wth=wth)
}
