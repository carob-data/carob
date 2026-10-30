# R script for "carob"
# license: GPL (>=3)

## NOTES
# Hourly series from sheet Hourly. Dictionary units: Ta °C, RH %, WS m/s, SR W/m2, PP mm, Eo mm, VPD kPa, Pa hPa.
# Wind direction is either a compass label or a degree value. Compass labels are stored as the center of that wedge (N = 0, NE = 45, E = 90). "---" and "------" are missing.
# Coordinates and elevation are the dataset characteristicOfSources text: "South Latitude 11.1 / West Longitude 75.4 / Elevation 848".

## ISSUES
# Eo is reference evapotranspiration, in mm for the recording interval, stored as ETo.
# VPD in the file is kPa. It is stored as vpd in hPa.
# The published coordinates are to 0.1 degree.
# write_files: missing variables: carob_contributor.

## NOT PROCESSED
# 03_Photo_San Ramon CIP Station.jpg.

carob_script <- function(path) {

"
Dataset for: Meteorological data at San Ramon CIP experimental station from 2019 to 2026

In the present dataset, hourly meteorological data at San Ramon CIP Experimental Station during December 7 2018 to February 23 2026 are available. The meteorological variables are the following variables: Air temperature (Ta, °C), relative humidity (RH, %), vapor pressure deficit (VPD, kPa), wind speed (WS, m s^-1), wind direction (cardinals direction of compass rose), solar radiation (SR, W m^-2), atmospheric pressure (Pa, hPa), precipitation (PP, mm) and potential evapotranspiration (Eo, mm). The global average values for this period reached 23.4 ± 0.17 °C, 82.7 ± 0.66 %, 0.29 ± 0.02 m s^⁻¹, 32.9 ± 1.07 MJ m^⁻² day^⁻¹, and 6.1 ± 0.23 mm day^⁻¹ for Ta, RH, WS, SR, and Eo, respectively. Finaly, the annual average PP reached 1224.5 ± 336.0 mm.
"

	uri <- "doi:10.21223/269Z3N"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "01_Meteorology data_San Ramon CIP station 2019-2026.xlsx"]
	r <- carobiner::read.excel(f, sheet="Hourly", na=c("---", "------"))

	blank <- function(x) {
		y <- trimws(as.character(x))
		y[y %in% c("", "---", "------", "NA")] <- NA_character_
		y
	}
	num <- function(x) {
		y <- blank(x)
		as.numeric(y)
	}

	compass <- c(
		N=0, NNE=22.5, NE=45, ENE=67.5, E=90, ESE=112.5, SE=135, SSE=157.5,
		S=180, SSW=202.5, SW=225, WSW=247.5, W=270, WNW=292.5, NW=315, NNW=337.5
	)
	dir <- blank(r$Dir)
	is_num <- !is.na(dir) & grepl("^[0-9]+(\\.[0-9]+)?$", dir)
	wdir <- rep(NA_real_, length(dir))
	wdir[is_num] <- as.numeric(dir[is_num])
	lab <- dir[!is_num & !is.na(dir)]
	unknown <- setdiff(unique(lab), names(compass))
	if (length(unknown) > 0) {
		stop("unmapped wind direction: ", paste(utils::head(unknown, 30), collapse=", "))
	}
	wdir[!is_num] <- unname(compass[dir[!is_num]])

	wth <- data.frame(
		station_id = "CIP-SanRamon",
		date = as.character(as.Date(r$Date)),
		time = format(as.POSIXct(r$Time), "%H:%M:%S", tz="UTC"),
		temp = num(r$Ta),
		rhum = num(r$RH),
		vpd = num(r$VPD) * 10,
		wspd = num(r$WS),
		wdir = wdir,
		pressure = num(r$Pa),
		irad = num(r$SR),
		prec = num(r$PP),
		ETo = num(r$Eo)
	)
	wth <- wth[!is.na(wth$date), ]

	d <- data.frame(
		station_id = "CIP-SanRamon",
		station_name = "San Ramon CIP experimental station",
		country = "Peru",
		location = "San Ramon",
		adm1 = "Junin",
		latitude = -11.1,
		longitude = -75.4,
		elevation = 848,
		geo_from_source = TRUE,
		geo_uncertainty = 12000,
		geo_source = "Dataverse characteristicOfSources: South Latitude 11.1, West Longitude 75.4, Elevation 848",
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
