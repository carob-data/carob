# R script for "carob"
# license: GPL (>=3)

## NOTES
# La Soledad, Chugay, La Libertad, Peru. Vantage Pro2 records about every 10 minutes from 2022-07-11 to 2024-03-03.
# Date and Time are Excel date and time. Time is stored as HH:MM. Coordinates are the Dataverse bounding box, which publishes only a south latitude and a west longitude.
# Dictionary: Ta, Tmax and Tmin are C, RH is %, wind speed is m/s, rain is mm, solar radiation is W/m2, pressure is hPa, VPD is kPa. Eo is reference evapotranspiration, in mm for the recording interval, stored as ETo. Wind direction is a 16-point compass label, stored as wdir at the center of that wedge (N = 0, NE = 45, E = 90).

## ISSUES
# No fertilizer or crop records. This is a weather station series.
# VPD in the file is kPa and is stored as vpd in hPa.
# write_files: missing carob_contributor.

## NOT PROCESSED
# ID: row number.
# None of the measurement columns were dropped.

carob_script <- function(path) {

"
Dataset for: Meteorological data at La Soledad locality, Chugay - La Libertad from 2022 to 2024

In the present dataset, meteorological data at La Soledad locality - Chugay, La Libertad during July 11th 2022 to March 3th 2024 are available. The meteorological variables are the following variables: Air temperature (°C), relative humidity (%), wind speed (m s^-1), wind direction (cardinals direction of compass rose), solar radiation (W m^-2), atmospheric pressure (hPa), rain (mm) and potential evapotranspiration (mm).
"

	uri <- "doi:10.21223/BWELO8"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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

	f <- ff[basename(ff) == "01_Meteorological data_La Soledad-Chugay 2022-2024.xlsx"]
	nms <- names(carobiner::read.excel(f, n_max=0))
	types <- rep("text", length(nms))
	types[nms %in% c("Date", "Time")] <- "guess"
	r <- carobiner::read.excel(f, col_types=types)

	d <- data.frame(
		station_id = "La Soledad",
		station_name = "La Soledad",
		country = "Peru",
		adm1 = "La Libertad",
		adm2 = "Sánchez Carrión",
		adm3 = "Chugay",
		location = "La Soledad",
		latitude = -7.77,
		longitude = -77.83,
		geo_from_source = TRUE,
		is_survey = FALSE,
		on_farm = NA
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("NAN", "NA", "---", "")] <- NA_character_
		as.numeric(x)
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
		station_id = "La Soledad",
		date = as.character(as.Date(r$Date)),
		time = format(r$Time, "%H:%M"),
		temp = num(r$Ta),
		tmax = num(r$Tmax),
		tmin = num(r$Tmin),
		rhum = num(r$RH),
		wspd = num(r$Ws),
		wdir = compass_deg(r$Dir),
		prec = num(r$P),
		irad = num(r$SR),
		vpd = num(r$VPD) * 10,
		pressure = num(r$Pa),
		ETo = num(r$Eo)
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
