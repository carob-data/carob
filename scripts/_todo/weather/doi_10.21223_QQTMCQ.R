# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sub-hourly record at Vista Florida, Pataz, 13 July 2022 to 31 October 2023.
# Early dates are month/day/year text. From 13 June 2023 the date is an Excel serial (origin 1899-12-30). Time is an Excel fraction of a day.
# Dictionary: temperatures C, RH %, wind speed m/s, solar radiation W/m2, rain and Eo mm, pressure hPa, VPD kPa.
# Tmax and Tmin are the extremes during that interval, not daily extremes.
# Wind direction is a 16-point compass label. It is stored as wdir at the center of that wedge (N = 0, NE = 45, E = 90).
# Solar radiation is interval mean intensity, stored as irad.
# The Dataverse bounding box stores westLongitude 77.61 and southLatitude 7.82 without a sign. The coverage is Peru, so both are stored as negative. The point is about 20 km from the El Progreso-Chugay station in the same province.

## ISSUES
# Confirm the sign correction on the bounding box.
# VPD in the file is kPa. It is stored as vpd in hPa.
# write_files: missing variables: carob_contributor.
# Eo is reference evapotranspiration, in mm for the recording interval, stored as ETo.
# Sites need a check that Vista Florida in this file is the Pataz site named in the title, which is what the coordinates support.

## NOT PROCESSED
# 02_Data_dictionary.xlsx: column definitions, already used.


carob_script <- function(path) {

"
Dataset for: Meteorological data at Vista Florida locality, Pataz - La Libertad from 2022 to 2023

In the present dataset, meteorological data at Vista Florida locality - Pataz, La Libertad during July 13th 2022 to October 31st 2023 are available. The meteorological variables are the following variables: Air temperature (°C), relative humidity (%), vapor pressure deficit (kPa), wind speed (m s^-1), wind direction (cardinals direction of compass rose), solar radiation (W m^-2), atmospheric pressure (hPa), precipitation (mm) and potential evapotranspiration (mm).
"

	uri <- "doi:10.21223/QQTMCQ"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "01_Meteorological data_Vista Florida_2022-2023.xlsx"]
	# Date is month/day/year text in early rows and an Excel serial later. Read every column as text.
	r <- carobiner::read.excel(f, na=c("NAN", "NaN", "NA"), col_types=rep("text", 14))

	num <- function(x) {
		s <- trimws(as.character(x))
		s[!nzchar(s)] <- NA_character_
		as.numeric(s)
	}

	raw_date <- trimws(as.character(r$Date))
	raw_date[!nzchar(raw_date)] <- NA_character_
	date <- rep(NA_character_, length(raw_date))
	mdy <- !is.na(raw_date) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", raw_date)
	date[mdy] <- as.character(as.Date(raw_date[mdy], format="%m/%d/%Y"))
	iso <- !is.na(raw_date) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", raw_date)
	date[iso] <- substr(raw_date[iso], 1, 10)
	ser <- !is.na(raw_date) & grepl("^[0-9]{4,6}$", raw_date)
	date[ser] <- as.character(as.Date(as.numeric(raw_date[ser]), origin="1899-12-30"))
	if (any(is.na(date) & !is.na(raw_date))) {
		stop("unparsed date")
	}

	# Time is an Excel fraction of a day (0.5 = 12:00).
	tv <- num(r$Time)
	mins <- round(tv * 1440)
	hh <- mins %/% 60
	mm <- mins %% 60
	hh[hh == 24] <- 0
	time <- rep(NA_character_, length(tv))
	ok <- !is.na(tv)
	time[ok] <- sprintf("%02d:%02d", hh[ok], mm[ok])

	compass_deg <- function(x) {
		x <- toupper(trimws(as.character(x)))
		x[!nzchar(x) | x %in% c("NA", "NULL", "NAN", "---", "------")] <- NA_character_
		rose <- c(
			N=0, NNE=22.5, NE=45, ENE=67.5, E=90, ESE=112.5, SE=135, SSE=157.5,
			S=180, SSW=202.5, SW=225, WSW=247.5, W=270, WNW=292.5, NW=315, NNW=337.5
		)
		out <- rep(NA_real_, length(x))
		known <- !is.na(x) & x %in% names(rose)
		out[known] <- unname(rose[x[known]])
		bad <- unique(x[!is.na(x) & !known])
		if (length(bad) > 0) stop("unmapped wind direction: ", paste(utils::head(bad, 30), collapse=", "))
		out
	}

	wth <- data.frame(
		station_id = "VistaFlorida_Pataz",
		date = date,
		time = time,
		temp = num(r$Ta),
		tmax = num(r$Tmax),
		tmin = num(r$Tmin),
		rhum = num(r$RH),
		vpd = num(r$VPD) * 10,
		wspd = num(r$Ws),
		wdir = compass_deg(r$Dir),
		pressure = num(r$Pa),
		prec = num(r$P),
		irad = num(r$SR),
		ETo = num(r$Eo)
	)
	wth <- wth[!is.na(wth$date), ]
	wth <- wth[order(wth$date, wth$time), ]
	rownames(wth) <- NULL

	d <- data.frame(
		station_id = "VistaFlorida_Pataz",
		station_name = "Vista Florida",
		country = "Peru",
		adm1 = "La Libertad",
		adm2 = "Pataz",
		location = "Vista Florida",
		# Dataverse westLongitude 77.61 and southLatitude 7.82, unsigned. Peru is west and south of 0.
		longitude = -77.61,
		latitude = -7.82,
		geo_from_source = TRUE,
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
