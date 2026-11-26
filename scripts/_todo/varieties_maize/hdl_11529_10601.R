# R script for "carob"
# license: GPL (>=3)

## NOTES
# Entry means from the 2005 CHTHTZIY hybrid trial. Grain yield is already kg/ha.
# Days pollen is days to anthesis. Plant height is cm. Moisture is percent.
# Coordinates, elevation, and dates are from 05CHTHTZIY-Location.xls, joined on the set number in the file name.
# Latitude and longitude are degrees and minutes. Excel serial dates use the 1899-12-30 origin.
# Set 1 (San Jose Iturbide, Gto., Mexico) has no coordinates in the location sheet; assigned 21.000 N, -100.383 W (geo_from_source = FALSE).
# 05CHTHTZIY18.xls says Location: Bako (2), Country: Ethiopia (while location sheet set 18 lists Ambo); extracted Country from row 2 of the trial workbook and assigned Bako station coordinates (9.117 N, 37.050 E, geo_from_source = FALSE).
# 05CHTHTZIY21.xls (Dhaulakuan, India) has flowering, height, and stalk rot without a grain-yield column; included with yield = NA and Dhaulakuan coordinates (30.494 N, 77.479 E, geo_from_source = FALSE).
# Cuedo and Jiquipilco are both recorded at 18°17' N, 98°37' W. Jiquipilco elevation is 0 in the location sheet.
# Vietnam is the spelling in the location sheet and on the Danphuong trial sheet.

## ISSUES
# missing variables: irrigated, K_fertilizer, N_fertilizer, P_fertilizer
# duplicate coordinates: fewer coordinates than locations
# NA detected: yield (Dhaulakuan set 21 has no yield column)

## NOT PROCESSED
# The combined workbook.
# Percent of the best check, and the number of plants harvested, because plot area is not given.
# Summary rows (mean, maximum, minimum, LSD, CV).
# 05CHTHTZIY-Combined.xls: not read. sheet 05CHTHTZIY-Combined: Mean for different traits across environments, 05CHTHTZIY-ACDATA.xls. Observation records are missing from d until this file is standardized.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 68,
		carob_effort = 0
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
	r1 <- as.data.frame(carobiner::read.excel(f, col_types="text"))
	# Header row is Country, Location, Set No., Latitude, Longitude, Masl, planting date, harvest date.
	sites <- r1[10:nrow(r1), ]
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
		r2 <- as.data.frame(carobiner::read.excel(f, sheet="Results", col_types="text"))
		row1 <- as.character(unlist(r2[1, ]))
		loc_hit <- row1[grepl("Location:", row1)][1]
		cty_hit <- row1[grepl("Country:", row1)][1]
		location <- if (!is.na(loc_hit)) trimws(sub(".*Location:\\s*", "", loc_hit)) else NA_character_
		country <- if (!is.na(cty_hit)) trimws(sub(".*Country:\\s*", "", cty_hit)) else NA_character_
		lab <- apply(r2[1:8, ], 2, function(z) paste(z, collapse=" "))
		ycol <- grep("Yield|Rend", lab)[1]
		acol <- grep("Pollen|Flor", lab)[1]
		scol <- grep("Silk", lab)[1]
		hcol <- grep("Plnt|Altur", lab)[1]
		ecol <- grep("Ear\\s*ht", lab)[1]
		mcol <- grep("Moist|Hum", lab)[1]
		ref_col <- if (!is.na(ycol)) ycol else acol
		if (is.na(ref_col)) return(NULL)
		hdr <- grep("Kg/ha|t/ha|Kg / ha|\\(d\\)", as.character(r2[[ref_col]]))
		start <- if (length(hdr)) max(hdr) + 1 else 1
		r2 <- r2[start:nrow(r2), ]
		ent <- trimws(as.character(r2[[1]]))
		ent[grepl("Location:|Agroecological|^Ent\\.$|^No\\.$", ent)] <- NA_character_
		ent_n <- num(ent)
		ped <- trimws(as.character(r2[[2]]))
		keep <- !is.na(ent_n) & !is.na(num(r2[[ref_col]]))
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
			yield = if (!is.na(ycol)) num(r2[[ycol]][keep]) else rep(NA_real_, n),
			anthesis_days = if (!is.na(acol)) num(r2[[acol]][keep]) else rep(NA_real_, n),
			silking_days = if (!is.na(scol)) num(r2[[scol]][keep]) else rep(NA_real_, n),
			plant_height = if (!is.na(hcol)) num(r2[[hcol]][keep]) else rep(NA_real_, n),
			ear_height = if (!is.na(ecol)) num(r2[[ecol]][keep]) else rep(NA_real_, n),
			yield_moisture = if (!is.na(mcol)) num(r2[[mcol]][keep]) else rep(NA_real_, n)
		)
	}

	f1 <- ff[basename(ff) %in% c(
		"05CHTHTZIY1.xls",
		"05CHTHTZIY16.xls",
		"05CHTHTZIY17.xls",
		"05CHTHTZIY18.xls",
		"05CHTHTZIY21.xls",
		"05CHTHTZIY4.xls",
		"05CHTHTZIY5.xls",
		"05CHTHTZIY7.xls"
	)]
	blocks <- lapply(f1, one)
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
	# Fill missing coordinates for San Jose Iturbide (Mexico), Bako (2) (Ethiopia), and Dhaulakuan (India)
	i_sji <- is.na(d$latitude) & grepl("San Jose Iturbide", d$location, ignore.case = TRUE)
	d$latitude[i_sji] <- 21.000
	d$longitude[i_sji] <- -100.383
	d$geo_from_source[i_sji] <- FALSE
	i_bako <- is.na(d$latitude) & grepl("Bako", d$location, ignore.case = TRUE)
	d$latitude[i_bako] <- 9.117
	d$longitude[i_bako] <- 37.050
	d$geo_from_source[i_bako] <- FALSE
	i_dh <- is.na(d$latitude) & grepl("Dhaulakuan", d$location, ignore.case = TRUE)
	d$latitude[i_dh] <- 30.494
	d$longitude[i_dh] <- 77.479
	d$geo_from_source[i_dh] <- FALSE
	d$set_no <- NULL
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$record_id <- as.integer(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
