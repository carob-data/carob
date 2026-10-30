# R script for "carob"
# license: GPL (>=3)

## NOTES
# Hourly Davis Vantage Pro records from Peru, 2004-2012. Place and coordinates are columns on each row. Country is the dataset description (Peru).
# NULL is missing. Temperature is C, humidity is %, wind speed is m/s, rain is mm, solar radiation and PAR are W/m2, barometric pressure is hPa, dew point is C.
# HOURW mixes Excel day-fractions and clock strings such as 15:59:41.0. Both are stored as HH:MM.

## ISSUES
# Evapotranspiration is mm per recording interval, not mm/day. Wind direction is a compass code, not degrees.
# Wind Dir and Hi Dir are read as text. Guessing the type treats them as logical and drops codes such as SSE.
# Indoor temperature, UV, degree-days and wind-chill indexes are not primary outdoor observations.
# Some temperature readings are -67.8 C and wind speed reaches 70 m/s. Those values are left as recorded.
# write_files: missing carob_contributor. unknown variables: wind_dir_, hi_dir_, par_wm2_, pressure_hpa_, rain_rate_mm_h_, et_mm_, soil_moist_pct_, soil_temp_1_, soil_temp_2_, soil_temp_3_. out of bounds: temp (-67.8, 38.6), tmax (-67.8, 38.6), tmin (-67.8, 36.4), dewp (-67.8, 38.4), wspd (0, 70.2), wspdmx (0, 70.2).

## NOT PROCESSED
# Month, Day, Year: the same day as Date.
# WeatherType, Wind Run, Wind Chill, Heat Index, THW Index, THSW Index, Hi Solar Rad, UV Index, UV Dose, Hi UV, Heat D-D, Cool D-D, In Temp, In Hum, Leaf Wet, Wind Samp, ISS Recept, Arc. Int.

## NEW VARIABLES
# wind_dir_: wind direction as recorded.
# hi_dir_: direction at maximum wind speed, as recorded.
# pressure_hpa_: barometric pressure (hPa).
# rain_rate_mm_h_: rain rate (mm/h).
# et_mm_: evapotranspiration in the recording interval (mm).
# par_wm2_: photosynthetically active radiation (W/m2).
# soil_moist_pct_: soil moisture (%).
# soil_temp_1_, soil_temp_2_, soil_temp_3_: soil temperature (C).


carob_script <- function(path) {

"
Dataset for: Compilation of Hourly Data

This dataset comprises hourly data collected from diverse locations from Peru (from 2004 to 2012), detailing a comprehensive range of meteorological, environmental, and indoor parameters. It includes variables grouped under the following categories: weather conditions, temperature metrics, humidity, wind data, solar and UV metrics, rainfall and barometric data, evapotranspiration and more.
"

	uri <- "doi:10.21223/1RALF3"
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
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "01_hourly_data.xlsx"]
	# Wind direction is a compass cardinal. Guessing the type warns on SSE, SE, and similar.
	nms <- names(carobiner::read.excel(f, n_max=0))
	types <- rep("text", length(nms))
	r <- carobiner::read.excel(f, na=c("NULL", "NA"), col_types=types)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c("NA", "NULL", "NAN", "---")] <- NA_character_
		as.numeric(x)
	}
	clock <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		out <- rep(NA_character_, length(x))
		hm <- !is.na(x) & grepl("^[0-9]{1,2}:[0-9]{2}", x)
		if (any(hm)) {
			parts <- strsplit(x[hm], ":")
			hh <- as.integer(vapply(parts, `[`, "", 1))
			mm <- as.integer(vapply(parts, `[`, "", 2))
			out[hm] <- sprintf("%02d:%02d", hh, mm)
		}
		ser <- !hm & !is.na(x) & grepl("^-?[0-9.]+([eE][+-]?[0-9]+)?$", x)
		if (any(ser)) {
			mins <- round(as.numeric(x[ser]) * 24 * 60)
			out[ser] <- sprintf("%02d:%02d", mins %/% 60, mins %% 60)
		}
		out
	}
	date_of <- function(x) {
		x <- trimws(as.character(x))
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
		if (any(iso)) out[iso] <- as.character(as.Date(substr(x[iso], 1, 10)))
		ser <- !iso & !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
		if (any(ser)) out[ser] <- as.character(as.Date(as.numeric(x[ser]), origin="1899-12-30"))
		out
	}

	loc <- trimws(as.character(r$Locality))
	loc[!nzchar(loc)] <- NA_character_
	lat <- num(r$Latitude)
	lon <- num(r$Longitude)
	sid <- paste(loc, lat, lon, sep="|")

	d <- data.frame(
		station_id = sid,
		station_name = loc,
		country = "Peru",
		adm1 = carobiner::fix_name(as.character(r$Adm1), "title"),
		adm2 = carobiner::fix_name(as.character(r$Adm2), "title"),
		adm3 = carobiner::fix_name(as.character(r$Adm3), "title"),
		location = loc,
		latitude = lat,
		longitude = lon,
		elevation = num(r$Altitude),
		stringsAsFactors = FALSE
	)
	d <- d[!duplicated(d$station_id), ]
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$is_survey <- FALSE
	d$on_farm <- NA

	wth <- data.frame(
		station_id = sid,
		date = date_of(r$Date),
		time = clock(r$HOURW),
		temp = num(r$Tmean),
		tmax = num(r$Tmax),
		tmin = num(r$Tmin),
		rhum = num(r$Rhum),
		dewp = num(r[["Dew Pt"]]),
		wspd = num(r[["Wind Speed"]]),
		wspdmx = num(r[["Hi Speed"]]),
		wind_dir_ = trimws(as.character(r[["Wind Dir"]])),
		hi_dir_ = trimws(as.character(r[["Hi Dir"]])),
		prec = num(r$Rain),
		irad = num(r[["Solar Rad"]]),
		par_wm2_ = num(r$PAR),
		pressure_hpa_ = num(r$Bar),
		rain_rate_mm_h_ = num(r[["Rain Rate"]]),
		et_mm_ = num(r$ET),
		soil_moist_pct_ = num(r[["Soil 1 Moist"]]),
		soil_temp_1_ = num(r[["Soil  Temp 1"]]),
		soil_temp_2_ = num(r[["Soil  Temp 2"]]),
		soil_temp_3_ = num(r[["Soil  Temp 3"]]),
		stringsAsFactors = FALSE
	)
	wth$wind_dir_[!nzchar(wth$wind_dir_)] <- NA_character_
	wth$hi_dir_[!nzchar(wth$hi_dir_)] <- NA_character_
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
