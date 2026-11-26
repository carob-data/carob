# R script for "carob"
# license: GPL (>=3)

## NOTES
# Hourly Vantage Pro Plus records. Temperature, humidity, dew point, wind speed, rain, solar radiation, and evapotranspiration are the station columns.
# Wind direction is a 16-point compass label. It is stored as wdir at the center of that wedge (N = 0, NE = 45, E = 90). The text NULL is missing.
# Solar energy is labeled Wh/m2 in the dictionary, but Solar Energy / Solar Rad = 0.0143 (10-min) and 0.0860 (60-min), which is Langleys (1 ly = 41.84 kJ/m2). Multiplied by 41.84 to kJ/m2.
# ET is the mm recorded for that hour, stored as ETo. Barometric pressure is hPa.
# Coordinates, admin names, and elevation are columns in the workbook.
# The description lists vapor pressure deficit. This workbook has dew point, not vapor-pressure deficit.

## ISSUES
# Comfort indices, UV, indoor temperature, rain rate, and wind run are not mapped.
# Soil Temp 1, 2, 3 are a 3-depth profile (undocumented depths); averaged to soil_temp.
# Soil 1 Moist. is labeled % in the dictionary, but values range 2-200 (Davis Watermark 200SS soil water tension in centibars/kPa, 0 = wet, 200 = dry); stored as soil_water_tension.

## NOT PROCESSED
# PAR: column is completely empty (all NULL).
# Wind Run, Wind Chill, Heat Index, THW Index, THSW Index, Rain Rate, Hi Solar Rad., UV Index, UV Dose, Hi UV, Heat D-D, Cool D-D, In Temp, In Hum, Wind Samp, ISS Recept, Arc. Int. Derived indices or station diagnostics.
# 02_Data_Dictionary.xlsx: column definitions.

carob_script <- function(path) {

"
Dataset for: Meteorological data at La Victoria CIP experimental station from 2004 to 2012

In the present dataset, hourly meteorological data at La Victoria CIP Experimental Station from July 2004 to May 2012 is available. The meteorological variables are the following variables: Air temperature (°C), relative humidity (%), vapor pressure deficit (kPa), wind speed (m s^-1), wind direction (cardinals direction of compass rose), solar radiation (W m^-2), atmospheric pressure (hPa), rain (mm) and potential evapotranspiration (mm). Data from an automatic weather station Vantage Pro Plus.
"

	uri <- "doi:10.21223/BJOIJL"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		notes = NA,
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-11-26",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 1/12
	)

	f <- ff[basename(ff) == "01_LaVictoria_Junin.xlsx"]
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

	adm1 <- carobiner::fix_name(as.character(r$Adm1), "title")
	adm2 <- carobiner::fix_name(as.character(r$Adm2), "title")
	adm3 <- carobiner::fix_name(as.character(r$Adm3), "title")
	location <- carobiner::fix_name(as.character(r$Locality), "title")
	latitude <- num(r$Latitude)
	longitude <- num(r$Longitude)
	elevation <- num(r$Altitude)
	key <- paste(adm1, adm2, adm3, location, latitude, longitude, elevation)
	if (length(unique(key)) != 1) stop("more than one station location")

	date <- as.character(r$Date)
	if (inherits(r$Date, "POSIXt") || inherits(r$Date, "Date")) {
		date <- as.character(as.Date(r$Date))
	}
	soil_temp <- rowMeans(cbind(num(r[["Soil  Temp 1"]]), num(r[["Soil  Temp 2"]]), num(r[["Soil  Temp 3"]])), na.rm=TRUE)
	soil_temp[is.nan(soil_temp)] <- NA_real_

	wth <- data.frame(
		station_id = location[1],
		date = date,
		time = clock(r$Time),
		temp = num(r[["Temp Out"]]),
		tmax = num(r[["Hi Temp"]]),
		tmin = num(r[["Low Temp"]]),
		rhum = num(r[["Out Hum"]]),
		dewp = num(r[["Dew Pt."]]),
		wspd = num(r[["Wind Speed"]]),
		wdir = compass_deg(r[["Wind Dir"]]),
		wspdmx = num(r[["Hi Speed"]]),
		wind_dir_max = compass_deg(r[["Hi Dir"]]),
		prec = num(r$Rain),
		irad = num(r[["Solar Rad."]]),
		srad = num(r[["Solar Energy"]]) * 41.84,
		ETo = num(r$ET),
		pressure = num(r$Bar),
		soil_water_tension = num(r[["Soil 1 Moist."]]),
		soil_temp = soil_temp,
		leaf_wetness = num(r[["Leaf Wet 1"]])
	)
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	agg_hourly <- carobiner::get_function("aggregate_hourly", path, group)
	wth <- agg_hourly(wth)

	d <- data.frame(
		station_id = location[1],
		station_name = location[1],
		country = "Peru",
		adm1 = adm1[1],
		adm2 = adm2[1],
		adm3 = adm3[1],
		location = location[1],
		on_farm = FALSE,
		is_survey = FALSE,
		latitude = latitude[1],
		longitude = longitude[1],
		elevation = elevation[1],
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
