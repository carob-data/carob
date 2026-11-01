# R script for "carob"
# license: GPL (>=3)

## NOTES
# Santa Fe, Chugay, La Libertad, Peru. Same 10-minute Vantage Pro2 layout as La Soledad, from 2023-11-15 to 2024-08-14.
# Coordinates are the Dataverse bounding box (south latitude and west longitude only).
# Units follow the dictionary: temperature C, humidity %, wind m/s, rain mm, solar radiation W/m2, pressure hPa, VPD kPa, evapotranspiration mm per record. Wind direction is a 16-point compass label, stored as wdir at the center of that wedge (N = 0, NE = 45, E = 90).

## ISSUES
# Eo is reference evapotranspiration, in mm for the recording interval, stored as ETo. VPD in the file is kPa and is stored as vpd in hPa.
# The file uses NAN and --- as missing values.
# write_files: missing carob_contributor.

## NOT PROCESSED
# ID: row number.

carob_script <- function(path) {

"
Dataset for: Meteorological data at Santa Fe, Chugay district - La Libertad from 2023 to 2024

In the present dataset, meteorological data from the station located at Santa Fe zone - Chugay district, La Libertad during November 15th 2023 to August 14th 2024 are available. The meteorological variables are the following variables: Air temperature (°C), relative humidity (%), vapor pressure deficit (kPa), wind speed (m s^-1), wind direction (cardinals direction of compass rose), solar radiation (W m^-2), atmospheric pressure (hPa), precipitation (mm) and potential evapotranspiration (mm).
"

	uri <- "doi:10.21223/VFVAZK"
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

	f <- ff[basename(ff) == "01_Meteorological data_Santa Fe-Chugay_2023-2024.xlsx"]
	nms <- names(carobiner::read.excel(f, n_max=0))
	types <- rep("text", length(nms))
	types[nms %in% c("Date", "Time")] <- "guess"
	r <- carobiner::read.excel(f, col_types=types)

	d <- data.frame(
		station_id = "Santa Fe",
		station_name = "Santa Fe",
		country = "Peru",
		adm1 = "La Libertad",
		adm2 = "Sánchez Carrión",
		adm3 = "Chugay",
		location = "Santa Fe",
		latitude = -7.78,
		longitude = -77.87,
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
		station_id = "Santa Fe",
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
