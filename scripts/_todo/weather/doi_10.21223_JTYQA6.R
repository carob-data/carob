# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily weather for localities in Peru and one locality in Uganda (Kalengyere), 1974-2012.
# NULL is the missing-value flag. Coordinates and altitude are in the data file (decimal degrees, m).
# Dictionary: temperatures C, rain mm/day, relative humidity %, leaf wetness on a 0-15 scale.
# Daily solar radiation values (1.5-34, median 20.6) are clearly MJ/m2/day despite the dictionary's "w/m2" label; stored as srad in kJ/m2 (* 1000).
# Daily PAR values (1263-15237, median 7063, Hobo/Watchdog) are daily sums of hourly umol m-2 s-1; converted to kJ/m2 (* 3.6 / 4.57).
# Rain uses the tokens T. and NJL. Those are not amounts, so they are missing in prec.
# -999 in rain and humidity is a missing code, not a measurement. Relative humidity drift slightly above 100% (up to 104%) is clamped to 100%.
# Adm2 "Cocepcion" is stored as Concepcion. "oxapampa" is stored as Oxapampa.
# Weather type is recorded as "Hobo / Watchdog" or "Cambell".

## ISSUES
# Independencia (79 rows, Peru) has no admin names, elevation, or coordinates in the file and is ambiguous across Peruvian districts (NA latitude/longitude).
# Kabale is stored in adm1 as recorded. It is a district, not a Ugandan region.

## NEW VARIABLES
# weather_type_: instrument or station type as recorded (Hobo / Watchdog, Cambell)
# rh90_: relative humidity above 90%, dictionary unit %

## NOT PROCESSED
# data_dictionary.xlsx: column definitions, already used.
# Month, Day, Year: the same day as Date.
# HOURW: hour of observation. Every value is missing.


carob_script <- function(path) {

"
Dataset for: Compilation of Daily Data

This dataset consists of daily weather data collected from diverse locations in Peru, spanning from 1974 to 2012. It includes the following variables: weather type, temperature metrics, rainfall, solar radiation, humidity metrics and leaf wetness.
"

	uri <- "doi:10.21223/JTYQA6"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "01_daily_data.xlsx"]
	nms <- names(carobiner::read.excel(f, n_max=0))
	r <- carobiner::read.excel(f, na="NULL", col_types=rep("text", length(nms)))

	blank <- function(x) {
		x <- trimws(x)
		x[!nzchar(x)] <- NA_character_
		x
	}
	num <- function(x) {
		s <- blank(x)
		as.numeric(s)
	}
	# -999 is the missing code used in rain and humidity. It is not a measured value.
	meas <- function(x) {
		v <- num(x)
		v[!is.na(v) & v <= -99] <- NA_real_
		v
	}

	rain <- blank(r$Rain)
	rain[rain %in% c("T.", "NJL")] <- NA_character_
	date <- blank(r$Date)
	iso <- !is.na(date) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", date)
	out_date <- rep(NA_character_, length(date))
	out_date[iso] <- substr(date[iso], 1, 10)
	if (any(is.na(out_date) & !is.na(date))) {
		stop("unparsed date")
	}

	adm2 <- carobiner::fix_name(r$Adm2, "title")
	adm2[adm2 == "Cocepcion"] <- "Concepcion"
	loc <- carobiner::fix_name(r$Locality, "title")

	rh_clean <- function(x) {
		v <- meas(x)
		v[!is.na(v) & v > 100 & v <= 105] <- 100
		v
	}

	wth <- data.frame(
		country = blank(r$COUNTRY),
		adm1 = carobiner::fix_name(r$Adm1, "title"),
		adm2 = adm2,
		adm3 = carobiner::fix_name(r$Adm3, "title"),
		location = loc,
		latitude = num(r$Latitude),
		longitude = num(r$Longitude),
		elevation = num(r$Altitude),
		date = out_date,
		weather_type_ = blank(r$WeatherType),
		temp = meas(r$Tmean),
		tmax = meas(r$Tmax),
		tmin = meas(r$Tmin),
		prec = meas(rain),
		srad = meas(r[["Solar Rad"]]) * 1000,
		rhum = rh_clean(r$RHmean),
		rhmx = rh_clean(r$Rhmax),
		rhmn = rh_clean(r$Rhmin),
		rh90_ = meas(r$RH90),
		par = meas(r$PAR) * 3.6 / 4.57,
		leaf_wetness = meas(r[["Leaf Wet"]])
	)
	wth <- wth[!is.na(wth$date), ]

	place <- paste(wth$country, wth$adm1, wth$adm2, wth$adm3, wth$location, wth$latitude, wth$longitude)
	wth$station_id <- paste0("s", as.integer(factor(place, levels=unique(place))))

	d <- wth[!duplicated(wth$station_id), c("station_id", "country", "adm1", "adm2", "adm3", "location", "latitude", "longitude", "elevation")]
	d$station_name <- d$location
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$is_survey <- FALSE
	d$on_farm <- NA
	rownames(d) <- NULL

	keep <- c("station_id", "date", "weather_type_", "temp", "tmax", "tmin", "prec", "srad", "rhum", "rhmx", "rhmn", "rh90_", "par", "leaf_wetness")
	wth <- wth[, keep]
	wth <- wth[order(wth$station_id, wth$date), ]
	rownames(wth) <- NULL

	carobiner::write_files(path, meta, d, wth=wth)
}
