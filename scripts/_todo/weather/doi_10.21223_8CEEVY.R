# R script for "carob"
# license: GPL (>=3)

## NOTES
# Half-hourly (and some hourly) record at El Progreso, Chugay district, 14 November 2023 to 14 August 2024.
# Dictionary: Ta, Tmax and Tmin are C for the recording interval. RH is %. Wind speed is m/s. Solar radiation is W/m2. Rain and Eo are mm. Pressure is hPa. VPD is kPa.
# Tmax and Tmin are the extremes during that interval, not daily extremes.
# Wind direction is a compass-rose code (N, NE, E, ...), not degrees.
# The Dataverse bounding box publishes one west longitude (-77.82) and one south latitude (-7.77). That point is used.
# Solar radiation is interval mean intensity, stored as irad.

## ISSUES
# Eo is the potential evapotranspiration for the recording interval, in mm. The vocabulary unit for ETo is mm/day.
# Precipitation in this file is 0 on every row.
# write_files: missing variables: carob_contributor. unknown variables: vpd_, wind_dir_, pressure_hpa_.

## NEW VARIABLES
# vpd_: vapor pressure deficit, kPa
# pressure_hpa_: atmospheric pressure, hPa
# wind_dir_: compass-rose wind direction (N, NE, E, and the other 16-point codes)

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

	wd <- trimws(as.character(r$Dir))
	wd[!nzchar(wd)] <- NA_character_

	wth <- data.frame(
		station_id = "ElProgreso_Chugay",
		date = as.character(as.Date(r$Date)),
		time = time,
		temp = num(r$Ta),
		tmax = num(r$Tmax),
		tmin = num(r$Tmin),
		rhum = num(r$RH),
		vpd_ = num(r$VPD),
		wspd = num(r$Ws),
		wind_dir_ = wd,
		pressure_hpa_ = num(r$Pa),
		prec = num(r$P),
		irad = num(r$SR),
		ETo = num(r$Eo),
		stringsAsFactors = FALSE
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
		on_farm = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
