# R script for "carob"
# license: GPL (>=3)

## NOTES
# Records are the station interval (often 30 minutes, sometimes one hour). Temperature, humidity, wind, rain, solar radiation, and evapotranspiration are as recorded for that interval.
# Wind direction is a 16-point compass label. It is stored as wdir at the center of that wedge (N = 0, NE = 45, E = 90).
# Vapor pressure deficit in the file is kPa. It is stored as vpd in hPa. Atmospheric pressure is stored as pressure, in hPa. Solar radiation is W/m2. Precipitation is mm. Eo is reference evapotranspiration, in mm for the recording interval, stored as ETo.
# Latitude -7.81 and longitude -77.87 are the Dataverse bounding box (south latitude and west longitude only, two decimals). geo_uncertainty is 2000 m.
# Country Peru and department La Libertad are the geographic coverage. Pishauli and Chugay district are in the dataset title.

## ISSUES
# The province is not in the file.
# write_files: missing carob_contributor.

## NOT PROCESSED
# 02_Data_dictionary.xlsx: column definitions.
# 03_Photo MS Pishauli-Chugay.jpg: station photograph.

carob_script <- function(path) {

"
Dataset for: Meteorological data at Pishauli, Chugay district - La Libertad from 2023 to 2024

In the present dataset, meteorological data in the Pishauli locality - Chugay district, La Libertad during November 16th 2023 to July 18th 2024 are available. The meteorological variables are the following variables: Air temperature (°C), relative humidity (%), vapor pressure deficit (kPa), wind speed (m s^-1), wind direction (cardinals direction of compass rose), solar radiation (W m^-2), atmospheric pressure (hPa), precipitation (mm) and potential evapotranspiration (mm).
"

	uri <- "doi:10.21223/LK29AX"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
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

	f <- ff[basename(ff) == "01_Meteorological data_Pishauli-Chugay_2023-2024.xlsx"]
	r <- carobiner::read.excel(f, na=c("NULL", "null", ""))

	num <- function(x) {
		if (is.character(x)) {
			x <- trimws(x)
			x[!nzchar(x)] <- NA_character_
		}
		as.numeric(x)
	}
	clock <- function(x) {
		if (inherits(x, "POSIXt")) return(format(x, "%H:%M:%S", tz="UTC"))
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		x
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
	date <- r$Date
	if (inherits(date, "POSIXt") || inherits(date, "Date")) date <- as.character(as.Date(date))
	date <- as.character(date)

	wth <- data.frame(
		station_id = "Pishauli",
		date = date,
		time = clock(r$Time),
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
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	rownames(wth) <- NULL

	d <- data.frame(
		station_id = "Pishauli",
		station_name = "Pishauli",
		country = "Peru",
		adm1 = "La Libertad",
		adm3 = "Chugay",
		location = "Pishauli",
		on_farm = NA,
		is_survey = FALSE,
		latitude = -7.81,
		longitude = -77.87,
		geo_from_source = TRUE,
		geo_uncertainty = 2000,
		geo_source = "CIP Dataverse bounding box"
)

	carobiner::write_files(path, meta, d, wth=wth)
}
