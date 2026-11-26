# R script for "carob"
# license: GPL (>=3)

## NOTES
# Hourly record from the CIP La Molina station, 15 April 2013 to 6 April 2025.
# Dictionary units: air temperature C, relative humidity %, wind speed m/s, wind direction degrees, solar radiation W/m2, rain mm, pressure hPa.
# Solar radiation is the hourly mean intensity, stored as irad (W/m2 = J s-1 m-2).
# Eo is the ASCE standardized reference evapotranspiration for that hour, in mm, stored as ETo.
# Vapor pressure deficit in the file is kPa and is stored as vpd in hPa. Atmospheric pressure is stored as pressure.
# Hourly PAR is in umol s-1 m-2 and is converted to hourly kJ m-2 using 4.57 umol J-1 (McCree 1972) * 3.6 (3600 s / 1000).

## ISSUES
# Coordinates and elevation are taken from the sister CIP La Molina station workbook (doi:10.21223/0X6T54; geo_from_source = FALSE).
# Eo is an hourly amount in mm. Small negative nighttime values (-0.049 to 0) are clamped to 0.
# Some Eo cells are the text NAN. Those are missing.
# Time is an integer hour on some rows and an Excel fraction of a day on others. Both are stored as hh:mm.

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
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-11-26",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 85,
		carob_effort = 1/12
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

	eto <- num(r[["Eo (mm)"]])
	# Small negative nighttime ASCE hourly ETo values (-0.049 to 0) are clamped to 0
	eto[!is.na(eto) & eto < 0 & eto >= -0.1] <- 0

	wth <- data.frame(
		station_id = "LaMolina_CIP",
		date = as.character(as.Date(r$Date)),
		time = time,
		temp = num(r[["Ta (°C)"]]),
		rhum = num(r[["RH (%)"]]),
		vpd = num(r[["VPD (kPa)"]]) * 10,
		wspd = num(r[["WS (m/s)"]]),
		wdir = num(r[["Dir (°)"]]),
		irad = num(r[["SR (W/m2)"]]),
		par = num(r[["PAR (umol s-1 m-2)"]]) * 3.6 / 4.57,
		prec = num(r[["P (mm)"]]),
		pressure = num(r[["Pa (hPa)"]]),
		ETo = eto
	)
	wth <- wth[!is.na(wth$date), ]
	wth <- wth[order(wth$date, wth$time), ]
	rownames(wth) <- NULL

	d <- data.frame(
		station_id = "LaMolina_CIP",
		station_name = "La Molina CIP",
		country = "Peru",
		adm1 = "Lima",
		adm2 = "Lima",
		adm3 = "La Molina",
		location = "La Molina",
		latitude = -12.07776,
		longitude = -76.94875,
		elevation = 239,
		geo_from_source = FALSE,
		geo_source = "CIP La Molina station workbook (doi:10.21223/0X6T54)",
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
