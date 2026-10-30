# R script for "carob"
# license: GPL (>=3)

## NOTES
# Hourly record from the CIP La Molina station, 15 April 2013 to 6 April 2025.
# Dictionary units: air temperature C, relative humidity %, wind speed m/s, wind direction degrees, solar radiation W/m2, rain mm, pressure hPa.
# Solar radiation is the hourly mean intensity, stored as irad (W/m2 = J s-1 m-2).
# Eo is the ASCE standardized reference evapotranspiration for that hour, in mm. The vocabulary unit for ETo is mm/day.
# Vapor pressure deficit is kPa. Atmospheric pressure is hPa. PAR is umol s-1 m-2. None of those three names is in the weather vocabulary.

## ISSUES
# The Dataverse record has no bounding box, so this station is not georeferenced.
# Eo is an hourly amount in mm, not a daily rate.
# Some Eo cells are the text NAN. Those are missing.
# Time is an integer hour on some rows and an Excel fraction of a day on others. Both are stored as hh:mm.
# Eo includes a few small negative values. They are kept.
# write_files: missing variables: carob_contributor, latitude, longitude. unknown variables: vpd_, par_umol_, pressure_hpa_. out of bounds: ETo (-0.049, 1.142).

## NEW VARIABLES
# vpd_: vapor pressure deficit, kPa
# pressure_hpa_: atmospheric pressure, hPa
# par_umol_: photosynthetically active radiation, umol s-1 m-2

## NOT PROCESSED
# 02_Data_dictionary.xlsx: column definitions, already used.
# 03_Photo_La Molina CIP Station.jpg: station photograph.


carob_script <- function(path) {

"
Dataset for: Meteorological data at La Molina CIP experimental station from 2013 to 2025

In the present dataset, hourly meteorological data at La Molina CIP Experimental Station during April 15th 2013 to April 6th 2025 are available. The meteorological variables are the following variables: Air temperature (°C), relative humidity (%), vapor pressure deficit (kPa), wind speed (m s^-1), wind direction (°), solar radiation (W m^-2), PAR radiation (umol s^-1 m^-2), rain (mm), atmospheric pressure (hPa) and potential evapotranspiration (mm). Some problems in the registration were presented, due to sensor errors, low battery, station maintenance and others, in some cases were completed with data from the Alexander von Humboldt meteorological station of the National Agrarian University-La Molina (highlighted in blue).
"

	uri <- "doi:10.21223/0GJCYR"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Meteorology data_La Molina CIP station 2013-2025.xlsx"]
	# Time mixes integer hours and Excel day-fractions. Eo is blank at the start, so a guessed type drops the numbers.
	types <- c("guess", "guess", "text", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "text")
	r <- carobiner::read.excel(f, na=c("NAN", "NaN", "NA"), col_types=types)

	num <- function(x) {
		if (is.numeric(x)) {
			x[is.nan(x)] <- NA_real_
			return(x)
		}
		s <- trimws(as.character(x))
		s[!nzchar(s)] <- NA_character_
		as.numeric(s)
	}
	# Integer 0-23 is an hour. A fraction of 1 is an Excel time (0.75 = 18:00).
	clock <- function(x) {
		s <- trimws(as.character(x))
		s[!nzchar(s)] <- NA_character_
		v <- as.numeric(s)
		out <- rep(NA_character_, length(s))
		is_hour <- !is.na(s) & grepl("^[0-9]{1,2}$", s) & v >= 0 & v <= 23
		out[is_hour] <- sprintf("%02d:00", as.integer(v[is_hour]))
		is_frac <- !is_hour & !is.na(v) & v >= 0 & v < 1
		mins <- round(v[is_frac] * 1440)
		hh <- mins %/% 60
		mm <- mins %% 60
		hh[hh == 24] <- 0
		out[is_frac] <- sprintf("%02d:%02d", hh, mm)
		out
	}
	time <- clock(r$Time)

	wth <- data.frame(
		station_id = "LaMolina_CIP",
		date = as.character(as.Date(r$Date)),
		time = time,
		temp = num(r[["Ta (°C)"]]),
		rhum = num(r[["RH (%)"]]),
		vpd_ = num(r[["VPD (kPa)"]]),
		wspd = num(r[["WS (m/s)"]]),
		wdir = num(r[["Dir (°)"]]),
		irad = num(r[["SR (W/m2)"]]),
		par_umol_ = num(r[["PAR (umol s-1 m-2)"]]),
		prec = num(r[["P (mm)"]]),
		pressure_hpa_ = num(r[["Pa (hPa)"]]),
		ETo = num(r[["Eo (mm)"]]),
		stringsAsFactors = FALSE
	)
	wth <- wth[!is.na(wth$date), ]
	wth <- wth[order(wth$date, wth$time), ]
	rownames(wth) <- NULL

	d <- data.frame(
		station_id = "LaMolina_CIP",
		station_name = "La Molina CIP",
		country = "Peru",
		location = "La Molina",
		adm1 = "Lima",
		geo_from_source = FALSE,
		is_survey = FALSE,
		on_farm = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
