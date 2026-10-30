# R script for "carob"
# license: GPL (>=3)

## NOTES
# Santa Fe, Chugay, La Libertad, Peru. Same 10-minute Vantage Pro2 layout as La Soledad, from 2023-11-15 to 2024-08-14.
# Coordinates are the Dataverse bounding box (south latitude and west longitude only).
# Units follow the dictionary: temperature C, humidity %, wind m/s, rain mm, solar radiation W/m2, pressure hPa, VPD kPa, evapotranspiration mm per record. Wind direction is a compass cardinal.

## ISSUES
# Potential evapotranspiration and VPD are interval values, not daily totals or vapour pressure.
# The file uses NAN and --- as missing values.
# write_files: missing carob_contributor. unknown variables: etp_mm_, wind_dir_, vpd_kpa_, pressure_hpa_.

## NOT PROCESSED
# ID: row number.

## NEW VARIABLES
# wind_dir_: compass cardinal.
# vpd_kpa_: vapour-pressure deficit (kPa).
# pressure_hpa_: atmospheric pressure (hPa).
# etp_mm_: potential evapotranspiration during the recording interval (mm).


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
		on_farm = NA,
		stringsAsFactors = FALSE
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("NAN", "NA", "---", "")] <- NA_character_
		as.numeric(x)
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
