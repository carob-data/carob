# R script for "carob"
# license: GPL (>=3)

## NOTES
# Half-hourly (and some hourly) record at El Progreso, Chugay district, 14 November 2023 to 14 August 2024.
# Dictionary: Ta, Tmax and Tmin are C for the recording interval. RH is %. Wind speed is m/s. Solar radiation is W/m2. Rain and Eo are mm. Pressure is hPa. VPD is kPa.
# Tmax and Tmin are the extremes during that interval, not daily extremes.
# Wind direction is a 16-point compass label. It is stored as wdir at the center of that wedge (N = 0, NE = 45, E = 90).
# The Dataverse bounding box publishes one west longitude (-77.82) and one south latitude (-7.77). That point is used.
# Solar radiation is interval mean intensity, stored as irad.

## ISSUES
# Eo is reference evapotranspiration, in mm for the recording interval, stored as ETo.
# Precipitation in this file is 0 on every row.
# VPD in the file is kPa. It is stored as vpd in hPa.

## NOT PROCESSED
# 02_Data_dictionary.xlsx: column definitions, already used.
# 03_El Progreso-Chugay.jpg: site photograph.


carob_script <- function(path) {

"
Dataset for: Meteorological data at El Progreso locality, Chugay - La Libertad from 2023 to 2024

In the present dataset, meteorological data at El progreso locality - Chugay district, La Libertad during November 14th 2023 to August 14th 2024 are available. The meteorological variables are the following variables: Air temperature (°C), relative humidity (%), vapor pressure deficit (kPa), wind speed (m s^-1), wind direction (cardinals direction of compass rose), solar radiation (W m^-2), atmospheric pressure (hPa), precipitation (mm) and potential evapotranspiration (mm).
"

	uri <- "doi:10.21223/8CEEVY"
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

	f <- ff[basename(ff) == "01_Meteorological data_El Progreso-Chugay_2023-2024.xlsx"]
	r <- carobiner::read.excel(f, na=c("NAN", "NaN", "NA"))

	num <- function(x) {
		if (is.numeric(x)) {
			x[is.nan(x)] <- NA_real_
			return(x)
		}
		s <- trimws(as.character(x))
		s[!nzchar(s)] <- NA_character_
		as.numeric(s)
	}

	tm <- r$Time
	if (inherits(tm, "POSIXt")) {
		time <- format(tm, "%H:%M")
	} else {
		time <- trimws(as.character(tm))
		time[!nzchar(time)] <- NA_character_
	}

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
		station_id = "ElProgreso_Chugay",
		date = as.character(as.Date(r$Date)),
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
		station_id = "ElProgreso_Chugay",
		station_name = "El Progreso",
		country = "Peru",
		adm1 = "La Libertad",
		adm2 = "Sanchez Carrion",
		adm3 = "Chugay",
		location = "El Progreso",
		longitude = -77.82,
		latitude = -7.77,
		geo_from_source = TRUE,
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
