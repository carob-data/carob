# R script for "carob"
# license: GPL (>=3)

## NOTES
# Entry means from the 2005 CHTHTZIY hybrid trial. Grain yield is already kg/ha.
# Days pollen is days to anthesis. Plant height is cm. Moisture is percent.
# Coordinates, elevation, and dates are from 05CHTHTZIY-Location.xls, joined on the set number in the file name.
# Latitude and longitude are degrees and minutes. Excel serial dates use the 1899-12-30 origin.
# Set 1 (San Jose Iturbide) has no coordinates. Coordinates are copied only when the set number and the place name agree.
# 05CHTHTZIY18.xls says Bako (2) and the location sheet set 18 says Ambo, so that trial is not given Ambo's coordinates or country.
# Cuedo and Jiquipilco are both recorded at 18°17' N, 98°37' W. Jiquipilco elevation is 0 in the location sheet.
# Vietnam is the spelling in the location sheet and on the Danphuong trial sheet.

## ISSUES
# missing variables: irrigated, K_fertilizer, N_fertilizer, P_fertilizer
# duplicate coordinates: fewer coordinates than locations
# NA detected: country, latitude, longitude

## NOT PROCESSED
# The combined workbook.
# 05CHTHTZIY21.xls (Dhaulakuan, India). That sheet has flowering, height, and lodging, and no grain-yield column.
# Percent of the best check, and the number of plants harvested, because plot area is not given.
# Summary rows (mean, maximum, minimum, LSD, CV).


carob_script <- function(path) {

"
Summary results and individual trial results from the International Intermediate Yellow Hybrid - IIYH, (Transition Zone Intermediate Yellow Hybrids - CHTHTZIY) conducted in 2005.
"

	uri <- "hdl:11529/10601"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Intermediate Yellow Hybrid Trial - IIYH0516",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.6
	)

	blank <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "- - -")] <- NA_character_
		dash <- !is.na(x) & grepl("-", x)
		x[dash] <- NA_character_
		x
	}
	num <- function(x) as.numeric(blank(x))
	excel_date <- function(x) {
		n <- num(x)
		as.character(as.Date(n, origin="1899-12-30"))
	}

	f <- ff[basename(ff) == "05CHTHTZIY-Location.xls"]
	loc <- as.data.frame(carobiner::read.excel(f, col_types="text"))
	# Header row is Country, Location, Set No., Latitude, Longitude, Masl, planting date, harvest date.
	sites <- loc[10:nrow(loc), ]
	sites <- data.frame(
		set_no = num(sites[[4]]),
		country = trimws(as.character(sites[[2]])),
		location = trimws(as.character(sites[[3]])),
		latitude = num(gsub("[^0-9.]", "", blank(sites[[5]]))) + num(gsub("[^0-9.]", "", blank(sites[[6]]))) / 60,
		longitude = num(gsub("[^0-9.]", "", blank(sites[[8]]))) + num(gsub("[^0-9.]", "", blank(sites[[9]]))) / 60,
		lat_hem = trimws(as.character(sites[[7]])),
		lon_hem = trimws(as.character(sites[[10]])),
		elevation = num(sites[[11]]),
		planting_date = excel_date(sites[[12]]),
		harvest_date = excel_date(sites[[13]])
	)
	south <- !is.na(sites$lat_hem) & sites$lat_hem == "S"
	west <- !is.na(sites$lon_hem) & sites$lon_hem == "W"
	sites$latitude[south] <- -sites$latitude[south]
	sites$longitude[west] <- -sites$longitude[west]
	sites$geo_from_source <- !is.na(sites$latitude) & !is.na(sites$longitude)

	one <- function(f) {
		raw <- as.data.frame(carobiner::read.excel(f, sheet="Results", col_types="text"))
		location <- trimws(sub(".*Location:\\s*", "", as.character(raw[1, 1])))
		country <- trimws(sub(".*Country:\\s*", "", as.character(raw[1, 3])))
		lab <- apply(raw[1:8, ], 2, function(z) paste(z, collapse=" "))
		ycol <- grep("Yield|Rend", lab)[1]
		if (is.na(ycol)) return(NULL)
		acol <- grep("Pollen|Flor", lab)[1]
		hcol <- grep("Plnt|Altur", lab)[1]
		mcol <- grep("Moist|Hum", lab)[1]
		hdr <- grep("Kg/ha|t/ha|Kg / ha", as.character(raw[[ycol]]))
		start <- if (length(hdr)) max(hdr) + 1 else 1
		raw <- raw[start:nrow(raw), ]
		ent <- trimws(as.character(raw[[1]]))
		ent[grepl("Location:|Agroecological|^Ent\\.$|^No\\.$", ent)] <- NA_character_
		ent_n <- num(ent)
		ped <- trimws(as.character(raw[[2]]))
		keep <- !is.na(ent_n) & !is.na(num(raw[[ycol]]))
		if (!any(keep)) return(NULL)
		variety <- ped[keep]
		miss <- is.na(variety) | variety %in% c("", "NA")
		variety[miss] <- ent[keep][miss]
		n <- sum(keep)
		data.frame(
			trial_id = sub("\\.xls$", "", basename(f)),
			set_no = num(sub(".*CHTHTZIY([0-9]+)\\.xls$", "\\1", basename(f))),
			country = rep(country, n),
			location = rep(location, n),
			variety = variety,
			treatment = variety,
			yield = num(raw[[ycol]][keep]),
			anthesis_days = if (!is.na(acol)) num(raw[[acol]][keep]) else rep(NA_real_, n),
			plant_height = if (!is.na(hcol)) num(raw[[hcol]][keep]) else rep(NA_real_, n),
			yield_moisture = if (!is.na(mcol)) num(raw[[mcol]][keep]) else rep(NA_real_, n)
		)
	}

	files <- ff[grepl("CHTHTZIY[0-9]+\\.xls$", basename(ff))]
	blocks <- lapply(files, one)
	blocks <- blocks[!vapply(blocks, is.null, TRUE)]
	d <- do.call(carobiner::bindr, blocks)
	d$country[d$country %in% c("", "NA")] <- NA_character_
	d$latitude <- NA_real_
	d$longitude <- NA_real_
	d$elevation <- NA_real_
	d$planting_date <- NA_character_
	d$harvest_date <- NA_character_
	d$geo_from_source <- FALSE
	place_key <- function(x) substr(tolower(gsub("[^a-z]", "", x)), 1, 4)
	for (i in seq_len(nrow(sites))) {
		hit <- !is.na(d$set_no) & d$set_no == sites$set_no[i] & place_key(d$location) == place_key(sites$location[i])
		if (!any(hit)) next
		d$latitude[hit] <- sites$latitude[i]
		d$longitude[hit] <- sites$longitude[i]
		d$elevation[hit] <- sites$elevation[i]
		d$planting_date[hit] <- sites$planting_date[i]
		d$harvest_date[hit] <- sites$harvest_date[i]
		d$geo_from_source[hit] <- isTRUE(sites$geo_from_source[i])
		need <- hit & is.na(d$country)
		d$country[need] <- sites$country[i]
	}
	d$set_no <- NULL
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$record_id <- as.integer(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
