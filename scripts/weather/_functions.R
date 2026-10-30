

africa_rice_weather <- function(ff) {

## NOTES
# Shared reader for the AfricaRice weather-station workbooks.
# Daily station weather from the AfricaRice Dataverse. The Data sheet is used. Dictionary.xls is the column list.
# Actual vapour pressure is kPa in the column name and in the dictionary. Multiplied by 10 to hPa.
# Solar radiation is already kJ/m2. Temperatures are C, rain is mm, humidity is %, wind is m/s, and wind direction is degrees from north.
# The Dataverse bounding box is used as the station coordinate only when it publishes one latitude and one longitude.
	# -99, -99.99, -100, and the text NULL are missing values. None of these is a possible value for these weather variables.

## ISSUES
# Quality-flag codes are kept as recorded. Dictionary.xls does not define what the codes mean.
# Where Weather_Date is an Excel serial rather than a date, it is read with the spreadsheet origin 1899-12-30.
# A bounding box with a north-south or east-west extent is not converted to a station coordinate.

## NOT PROCESSED
# Dictionary.xls: column definitions. The codes for the flag columns are not in this file.
# Data, Weather_Year and DOY: the same day as Weather_Date.

	.africa_rice_weather_country <- function(x) {
		x <- trimws(x)
		x[x %in% c("Tanzania, United Republic of", "United Republic of Tanzania")] <- "Tanzania"
		x[x %in% c("Congo, the Democratic Republic of the", "Congo, The Democratic Republic of the")] <- "Democratic Republic of the Congo"
		x[x %in% c("Cote d'Ivoire", "Cote d’Ivoire")] <- "Côte d'Ivoire"
		x[x == "Swaziland"] <- "Eswatini"
		x
	}

	.africa_rice_weather_place <- function(js) {
		fields <- js$data$latestVersion$metadataBlocks$geospatial$fields
		if (is.null(fields)) return(list(country=NA_character_, location=NA_character_, latitude=NA_real_, longitude=NA_real_, geo_from_source=FALSE))
		cov <- Filter(function(f) identical(f$typeName, "geographicCoverage"), fields)
		box <- Filter(function(f) identical(f$typeName, "geographicBoundingBox"), fields)
		country <- location <- NA_character_
		if (length(cov) == 1 && length(cov[[1]]$value) == 1) {
			v <- cov[[1]]$value[[1]]
			country <- .africa_rice_weather_country(v$country$value)
			location <- trimws(v$city$value)
			if (!nzchar(location)) location <- NA_character_
		}
		lat <- lon <- NA_real_
		geo <- FALSE
		if (length(box) == 1 && length(box[[1]]$value) == 1) {
			b <- box[[1]]$value[[1]]
			# A published coordinate can contain a space, as in "- 6.30".
			num <- function(z) {
				if (is.null(z) || is.null(z$value)) return(NA_real_)
				s <- gsub("\\s+", "", z$value)
				if (!nzchar(s) || !grepl("^-?[0-9]+(\\.[0-9]+)?$", s)) return(NA_real_)
				as.numeric(s)
			}
			north <- num(b$northLatitude)
			south <- num(b$southLatitude)
			east <- num(b$eastLongitude)
			west <- num(b$westLongitude)
			point <- !is.na(north) && !is.na(east) &&
				(is.na(south) || south == north) && (is.na(west) || west == east)
			if (point) {
				lat <- north
				lon <- east
				geo <- TRUE
			}
		}
		list(country=country, location=location, latitude=lat, longitude=lon, geo_from_source=geo)
	}

	.africa_rice_weather_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		x <- trimws(as.character(x))
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
		if (any(iso)) out[iso] <- as.character(as.Date(x[iso]))
		num <- !iso & !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
		if (any(num)) out[num] <- as.character(as.Date(as.numeric(x[num]), origin="1899-12-30"))
		out
	}

	xls <- ff[grepl("\\.xls[x]?$", ff, ignore.case=TRUE)]
	xls <- xls[!grepl("dictionary", basename(xls), ignore.case=TRUE)]
	if (length(xls) == 0) stop("no weather workbook")
	known <- c(
		"airtemp_avg-degc"="temp",
		"airtemp_min-degc"="tmin",
		"airtemp_max-degc"="tmax",
		"humidity_avg-%"="rhum",
		"humidity_min-%"="rhmn",
		"humidity_max-%"="rhmx",
		"winddir_avg-deg"="wdir",
		"windspd_avg-m.s-1"="wspd",
		"windspd_min-m.s-1"="wspd_min",
		"windspd_max-m.s-1"="wspdmx",
		"radiation-kj-m-2"="srad",
		"rainfall-mm"="prec",
		"actualvapourpressure_avg-kpa"="vapr",
		"dewpointtemp-degc"="dewp",
		"airtemp_avg_flag"="temp_flag",
		"airtemp_min_flag"="tmin_flag",
		"airtemp_max_flag"="tmax_flag",
		"humidity_avg_flag"="rhum_flag",
		"humidity_min_flag"="rhmn_flag",
		"humidity_max_flag"="rhmx_flag",
		"winddir_avg_flag"="wdir_flag",
		"windspd_avg_flag"="wspd_flag",
		"windspd_min_flag"="wspd_min_flag",
		"windspd_max_flag"="wspdmx_flag",
		"radiation_flag"="srad_flag",
		"rainfall_flag"="prec_flag",
		"actualvapourpressure_avg_flag"="vapr_flag",
		"dewpointtemp_flag"="dewp_flag"
	)
	parts <- lapply(xls, function(f) {
		nms <- names(carobiner::read.excel(f, sheet=1, n_max=0, .name_repair="minimal"))
		types <- rep("guess", length(nms))
		# Later rows can be text dates. Guessing a date type drops those rows and warns.
		types[tolower(trimws(nms)) == "weather_date"] <- "text"
		x <- carobiner::read.excel(f, sheet=1, .name_repair="minimal", na=c("-99", "-99.99", "NULL"), col_types=types)
		# Some sheets repeat the 32-column header across empty columns. Keep the first block.
		starts <- which(tolower(trimws(names(x))) == "station_shortname")
		if (length(starts) > 1) x <- x[, starts[1]:(starts[2] - 1), drop=FALSE]
		key <- tolower(trimws(names(x)))
		if (!"station_shortname" %in% key || !"weather_date" %in% key) {
			stop("unexpected columns in ", basename(f), ": ", paste(names(x), collapse=", "))
		}
		extra <- setdiff(key, c("station_shortname", "weather_date", "weather_year", "doy", names(known)))
		if (length(extra)) stop("unmapped columns in ", basename(f), ": ", paste(extra, collapse=", "))
		raw_date <- x[[which(key == "weather_date")[1]]]
		date <- .africa_rice_weather_date(raw_date)
		w <- data.frame(
			station_id = as.character(x[[which(key == "station_shortname")[1]]]),
			date = date
		)
		for (nm in names(known)) {
			j <- match(nm, key)
			if (is.na(j)) next
			outnm <- known[[nm]]
			v <- x[[j]]
			if (grepl("_flag$", outnm)) {
				w[[outnm]] <- as.character(v)
			} else {
				v <- as.numeric(v)
				# -99.99 is stored as a number, and wind direction uses -100. Both are below -99.
				v[!is.na(v) & v <= -99] <- NA_real_
				w[[outnm]] <- v
			}
		}
		if ("vapr" %in% names(w)) w$vapr <- w$vapr * 10
		w
	})
	wth <- do.call(rbind, parts)
	wth <- wth[!is.na(wth$date) & nzchar(wth$station_id), ]
	jsf <- list.files(unique(dirname(xls)), pattern="\\.json$", full.names=TRUE)
	jsf <- jsf[!grepl("_files\\.json$", jsf)]
	place <- if (length(jsf)) .africa_rice_weather_place(jsonlite::fromJSON(jsf[1], simplifyVector=FALSE)) else {
		list(country=NA_character_, location=NA_character_, latitude=NA_real_, longitude=NA_real_, geo_from_source=FALSE)
	}
	# The station row holds the place. Weather rows keep station_id as the link.
	d <- data.frame(
		station_id = unique(wth$station_id),
		station_name = place$location,
		country = place$country,
		location = place$location,
		latitude = place$latitude,
		longitude = place$longitude,
		geo_from_source = place$geo_from_source,
		is_survey = FALSE,
		on_farm = NA
	)
	list(d=d, wth=wth)
}
