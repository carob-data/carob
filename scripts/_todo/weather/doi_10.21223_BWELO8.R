# R script for "carob"
# license: GPL (>=3)

## NOTES
# La Soledad, Chugay, La Libertad, Peru. Vantage Pro2 records about every 10 minutes from 2022-07-11 to 2024-03-03.
# Date and Time are Excel date and time. Time is stored as HH:MM. Coordinates are the Dataverse bounding box, which publishes only a south latitude and a west longitude.
# Dictionary: Ta, Tmax and Tmin are C, RH is %, wind speed is m/s, rain is mm, solar radiation is W/m2, pressure is hPa, VPD is kPa, and potential evapotranspiration is mm for the recording interval. Wind direction is a compass cardinal, not degrees.

## ISSUES
# No fertilizer or crop records. This is a weather station series.
# Potential evapotranspiration and VPD are for each 10-minute record, so they are not stored as daily ETP or as vapour pressure.
# write_files: missing carob_contributor. unknown variables: etp_mm_, wind_dir_, vpd_kpa_, pressure_hpa_.

## NOT PROCESSED
# ID: row number.
# None of the measurement columns were dropped.

## NEW VARIABLES
# wind_dir_: compass cardinal.
# vpd_kpa_: vapour-pressure deficit (kPa).
# pressure_hpa_: atmospheric pressure (hPa).
# etp_mm_: potential evapotranspiration during the recording interval (mm).


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
		on_farm = NA,
		stringsAsFactors = FALSE
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("NAN", "NA", "---", "")] <- NA_character_
		as.numeric(x)
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
		wind_dir_ = as.character(r$Dir),
		prec = num(r$P),
		irad = num(r$SR),
		vpd_kpa_ = num(r$VPD),
		pressure_hpa_ = num(r$Pa),
		etp_mm_ = num(r$Eo),
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
