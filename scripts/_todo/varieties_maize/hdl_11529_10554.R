# R script for "carob"
# license: GPL (>=3)

## NOTES
# International Late White Hybrid Trial (ILWH / 06CHTTW), conducted in 2006 at 4 sites (Mexico and Ghana).
# Variety means are in the "-1" workbooks (06CHTTW1-1, 8-1, 9-1, 28-1). The matching workbooks without "-1" are empty field-book templates and are not read.
# GrainYieldTons_FieldWt is in t/ha, converted to kg/ha (* 1000).
# Site coordinates, elevation, and planting/harvest dates come from 06CHTTW-Locations.xls.
# In 06CHTTW-Locations.xls, Usmajac (Jalisco, Mexico) is printed as 18°55' N, 105°42' W, which falls in the Pacific Ocean; replaced with Usmajac coordinates (19.875 N, -103.5987 W, geo_from_source = FALSE), matching published hdl_11529_10542.R.
# Disease scores (ear rot, gray leaf spot, southern leaf blight, maize leaf blight, rust, curvularia, streak) go to the long table.

## ISSUES
# Usmajac, Jalisco coordinates in 06CHTTW-Locations.xls (18°55' N, 105°42' W) are in the ocean and were corrected to 19.875, -103.5987 (geo_from_source = FALSE).
# Fumesua harvest date in 06CHTTW-Locations.xls (2007-08-16) is 43 days after planting (2007-07-04); kept as recorded.
# write_files: missing variables: irrigated, K_fertilizer, N_fertilizer, P_fertilizer; datespan on Fumesua; unknown variable ears_per_plant_.

## NEW VARIABLES
# ears_per_plant_: ears per plant (EPPNo). Unit: ears/plant.

## NOT PROCESSED
# Empty field-book templates (06CHTTW1.xls, 8.xls, 9.xls, 28.xls) and summary workbooks (06CHTTW-Combined.xls, 06CHTTW-Grain Yield.xls).
# Rank, selection index, percent of best check, and seed-lot origin codes.


carob_script <- function(path) {

"
International Late White Hybrid Trial - ILWH0605

Summary results and individual trial results from the International Late White Hybrid - ILWH
(Elite Tropical Late White Normal and QPM Hybrid Trial - CHTTW) conducted in 2006.
"

	uri <- "hdl:11529/10554"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Late White Hybrid Trial - ILWH0605",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;anthesis_days;asi;plant_height;ear_height",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 72,
		carob_effort = 0,
		notes = NA
	)

	num <- function(x) {
		z <- trimws(as.character(x))
		z[z %in% c("", "NA", ".", "-", "- - -")] <- NA_character_
		z[!is.na(z) & grepl("[A-Za-z]", z)] <- NA_character_
		as.numeric(z)
	}
	num_piece <- function(x) {
		z <- gsub("o|'|°|º", "", as.character(x))
		num(z)
	}
	excel_date <- function(x) as.character(as.Date(num(x), origin = "1899-12-30"))
	pct <- function(x) {
		v <- num(x)
		out <- rep(NA_character_, length(v))
		ok <- !is.na(v)
		out[ok] <- paste0(v[ok], "%")
		out
	}

	f1 <- ff[basename(ff) == "06CHTTW-Locations.xls"]
	r1 <- carobiner::read.excel(f1, col_names = FALSE, na = c("", "- - -"))
	c2 <- trimws(as.character(r1[[2]]))
	keep <- !is.na(c2) & nzchar(c2) & !c2 %in% c("Country", "País", "Pais")
	r1 <- r1[keep, ]
	lat <- num_piece(r1[[5]]) + num_piece(r1[[6]]) / 60
	lon <- num_piece(r1[[8]]) + num_piece(r1[[9]]) / 60
	ns <- toupper(trimws(as.character(r1[[7]])))
	we <- toupper(trimws(as.character(r1[[10]])))
	lat[ns == "S"] <- -abs(lat[ns == "S"])
	lon[we == "W"] <- -abs(lon[we == "W"])
	country <- carobiner::fix_name(trimws(as.character(r1[[2]])), "title")
	country[country == "México"] <- "Mexico"
	locs <- data.frame(
		set_no = as.integer(num(r1[[4]])),
		country = country,
		location = trimws(as.character(r1[[3]])),
		latitude = lat,
		longitude = lon,
		elevation = num(r1[[11]]),
		planting_date = excel_date(r1[[12]]),
		harvest_date = excel_date(r1[[13]]),
		geo_from_source = !is.na(lat) & !is.na(lon)
	)
	locs$location <- gsub("Veracrúz", "Veracruz", locs$location, fixed = TRUE)
	# Source coordinates for Usmajac (18°55' N, 105°42' W) fall in the Pacific Ocean
	i_usm <- grepl("Usmajac", locs$location)
	locs$latitude[i_usm] <- 19.875
	locs$longitude[i_usm] <- -103.5987
	locs$geo_from_source[i_usm] <- FALSE

	read_trial <- function(f) {
		r <- carobiner::read.excel(f, col_names = FALSE)
		hdr <- trimws(as.character(unlist(r[1, ])))
		col <- function(nm) {
			i <- which(hdr == nm)
			if (!length(i)) return(rep(NA_character_, nrow(r)))
			as.character(r[[i[1]]])
		}
		ent <- num(col("Entry"))
		keep <- !is.na(ent)
		if (!any(keep)) return(NULL)
		ped <- trimws(col("BreedersPedigree1")[keep])
		nm <- trimws(col("Name")[keep])
		ped[is.na(ped) | ped %in% c("", "NA")] <- nm[is.na(ped) | ped %in% c("", "NA")]
		id <- as.integer(sub("^06CHTTW([0-9]+)-1\\.xls$", "\\1", basename(f), ignore.case = TRUE))
		data.frame(
			set_no = id,
			variety_code = as.character(as.integer(ent[keep])),
			variety = ped,
			variety_pedigree = ped,
			treatment = ped,
			yield = num(col("GrainYieldTons_FieldWt")[keep]) * 1000,
			yield_moisture = num(col("GrainMoisturePer")[keep]),
			anthesis_days = num(col("AnthesisDate")[keep]),
			silking_days = num(col("DaysToSilk")[keep]),
			asi = num(col("ASI")[keep]),
			plant_height = num(col("PlantHeightCm")[keep]),
			ear_height = num(col("EarHeightCm")[keep]),
			ears_per_plant_ = num(col("EPPNo")[keep]),
			root_lodging = pct(col("RootLodgingPer")[keep]),
			stalk_lodging = pct(col("StemLodgingPer")[keep]),
			husk = pct(col("BadHuskCoverPer")[keep]),
			aspect = num(col("PlantAspect1_5")[keep]),
			ear_aspect = num(col("EarAspect1_5")[keep]),
			e_rot = num(col("EarRotTotalPer")[keep]),
			gls = num(col("GrayLeafSpot1_5")[keep]),
			rust = num(col("CommonRust1_5")[keep]),
			blight = num(col("LeafBlightTurcicum1_5")[keep]),
			helm = num(col("LeafBlightMaydis1_5")[keep]),
			curv = num(col("CurvulariaLeafSpot1_5")[keep]),
			streak = num(col("VirusMaizeStreak1_5")[keep])
		)
	}

	f2 <- ff[basename(ff) %in% c("06CHTTW1-1.xls", "06CHTTW8-1.xls", "06CHTTW9-1.xls", "06CHTTW28-1.xls")]
	d <- do.call(rbind, lapply(f2, read_trial))
	d <- merge(d, locs, by = "set_no", all.x = TRUE)
	d$trial_id <- paste(d$set_no, d$location, sep = "_")
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	d$borer_trial <- FALSE
	d$set_no <- NULL

	for (nm in c("silking_days", "ears_per_plant_", "root_lodging", "stalk_lodging", "husk", "aspect", "ear_aspect")) {
		if (nm %in% names(d) && all(is.na(d[[nm]]))) d[[nm]] <- NULL
	}

	d$record_id <- as.integer(seq_len(nrow(d)))
	mz_pd_long <- carobiner::get_function("mz_pd_long", path, group)
	long <- mz_pd_long(d)
	d[intersect(c("e_rot", "gls", "rust", "blight", "helm", "curv", "streak"), names(d))] <- NULL

	if (is.null(long) || nrow(long) == 0) {
		d$record_id <- NULL
		carobiner::write_files(path, meta, d)
	} else {
		carobiner::write_files(path, meta, d, long = long)
	}
}
