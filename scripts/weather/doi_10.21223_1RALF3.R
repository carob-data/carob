# R script for "carob"
# license: GPL (>=3)

## NOTES
# Hourly Davis Vantage Pro records from Peru, 2004-2012. Place and coordinates are columns on each row. Country is the dataset description (Peru).
# NULL is missing. Temperature is C, humidity is %, wind speed is m/s, rain is mm, solar radiation and PAR are W/m2, barometric pressure is hPa, dew point is C.
# HOURW mixes Excel day-fractions and clock strings such as 15:59:41.0. Both are stored as HH:MM.

## ISSUES
# Evapotranspiration is the mm recorded for that hour, stored as ETo. Wind direction is a 16-point compass label, stored as wdir at the center of that wedge (N = 0, NE = 45, E = 90).
# Wind Dir and Hi Dir are read as text. Guessing the type treats them as logical and drops codes such as SSE.
# Indoor temperature, UV, degree-days and wind-chill indexes are not primary outdoor observations.
# Davis sensor disconnect codes (temperatures below -50 C, down to -67.8 C = -90 F, including 3,690 rows in Soil Temp 2), wind-speed spikes (> 25 m/s mean / > 50 m/s max, 70.2 m/s), and leaf_wetness readings > 15 (158-188, outside the 0-15 Davis sensor scale) are set to NA.
# PAR (Hobo/Watchdog rows) is instantaneous PPFD (umol m-2 s-1, up to 2554) logged at mixed sub-hourly intervals (1, 10, 15 min); converted to iPAR (J s-1 m-2 = W m-2) via / 4.57 (McCree 1972).
# Soil Temp 1, 2, 3 are a 3-depth profile (undocumented depths); averaged to soil_temp after filtering sensor disconnects.
# Soil 1 Moist is labeled % in the dictionary, but values range 0-200 (Davis Watermark 200SS soil water tension in centibars/kPa, 0 = wet, 200 = dry); stored as soil_water_tension.

## NOT PROCESSED
# Month, Day, Year: the same day as Date.
# WeatherType, Rain Rate, Wind Run, Wind Chill, Heat Index, THW Index, THSW Index, Hi Solar Rad, UV Index, UV Dose, Hi UV, Heat D-D, Cool D-D, In Temp, In Hum, Wind Samp, ISS Recept, Arc. Int.
# data_dictionary.xlsx: documentation or a codebook, not an observation table.


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
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-11-26",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 1/12
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
			mins <- round((as.numeric(x[ser]) %% 1) * 24 * 60)
			hh <- (mins %/% 60) %% 24
			out[ser] <- sprintf("%02d:%02d", hh, mins %% 60)
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
		elevation = num(r$Altitude)
	)
	d <- d[!duplicated(d$station_id), ]
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$is_survey <- FALSE
	d$on_farm <- NA

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

	clean_temp <- function(x) {
		v <- num(x)
		v[!is.na(v) & v < -50] <- NA_real_
		v
	}
	wspd <- num(r[["Wind Speed"]])
	wspd[!is.na(wspd) & wspd > 25] <- NA_real_
	wspdmx <- num(r[["Hi Speed"]])
	wspdmx[!is.na(wspdmx) & wspdmx > 50] <- NA_real_
	lw <- num(r[["Leaf Wet"]])
	lw[!is.na(lw) & lw > 15] <- NA_real_
	soil_temp <- rowMeans(cbind(clean_temp(r[["Soil  Temp 1"]]), clean_temp(r[["Soil  Temp 2"]]), clean_temp(r[["Soil  Temp 3"]])), na.rm=TRUE)
	soil_temp[is.nan(soil_temp)] <- NA_real_

	wth <- data.frame(
		station_id = sid,
		date = date_of(r$Date),
		time = clock(r$HOURW),
		temp = clean_temp(r$Tmean),
		tmax = clean_temp(r$Tmax),
		tmin = clean_temp(r$Tmin),
		rhum = num(r$Rhum),
		dewp = clean_temp(r[["Dew Pt"]]),
		wspd = wspd,
		wspdmx = wspdmx,
		wdir = compass_deg(r[["Wind Dir"]]),
		wind_dir_max = compass_deg(r[["Hi Dir"]]),
		prec = num(r$Rain),
		irad = num(r[["Solar Rad"]]),
		srad = num(r[["Solar Energy"]]) * 41.84,
		iPAR = num(r$PAR) / 4.57,
		pressure = num(r$Bar),
		ETo = num(r$ET),
		leaf_wetness = lw,
		soil_water_tension = num(r[["Soil 1 Moist"]]),
		soil_temp = soil_temp
	)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
