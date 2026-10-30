# R script for "carob"
# license: GPL (>=3)

## NOTES
# Hourly Vantage Pro Plus records. Temperature, humidity, dew point, wind speed, rain, solar radiation, and evapotranspiration are the station columns.
# Wind direction is a compass-rose cardinal, not degrees. Stored as wind_dir_. The text NULL is missing.
# Solar energy is Wh/m2 in the dictionary (vatios-hora/m2). Multiplied by 3.6 to kJ/m2.
# ET is mm per hourly interval, not mm/day. Barometric pressure is hPa.
# Coordinates, admin names, and elevation are columns in the workbook.
# The description lists vapor pressure deficit. This workbook has dew point, not vapor-pressure deficit.

## ISSUES
# Comfort indices, UV, indoor temperature, rain rate, and wind run are not mapped.
# write_files: missing carob_contributor. unknown variables: wind_dir_, wind_dir_max_, pressure_, par_, soil_moist_, soil_temp1_, soil_temp2_, soil_temp3_, leaf_wet_.

## NEW VARIABLES
# wind_dir_: compass-rose wind direction (N, NE, ...). Not degrees.
# wind_dir_max_: compass direction of the maximum wind speed in the interval.
# pressure_: barometric pressure, hPa.
# par_: photosynthetically active radiation, as recorded.
# soil_moist_: soil moisture sensor, percent.
# soil_temp1_, soil_temp2_, soil_temp3_: soil temperature sensors, C.
# leaf_wet_: leaf wetness sensor, 0 to 15.

## NOT PROCESSED
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
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.45
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
	card <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		x
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
		wind_dir_ = card(r[["Wind Dir"]]),
		wspdmx = num(r[["Hi Speed"]]),
		wind_dir_max_ = card(r[["Hi Dir"]]),
		prec = num(r$Rain),
		irad = num(r[["Solar Rad."]]),
		srad = num(r[["Solar Energy"]]) * 3.6,
		ETo = num(r$ET),
		pressure_ = num(r$Bar),
		par_ = num(r$PAR),
		soil_moist_ = num(r[["Soil 1 Moist."]]),
		soil_temp1_ = num(r[["Soil  Temp 1"]]),
		soil_temp2_ = num(r[["Soil  Temp 2"]]),
		soil_temp3_ = num(r[["Soil  Temp 3"]]),
		leaf_wet_ = num(r[["Leaf Wet 1"]]),
		stringsAsFactors = FALSE
	)
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	rownames(wth) <- NULL

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
		geo_from_source = TRUE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
