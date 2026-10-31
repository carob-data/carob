# R script for "carob"
# license: GPL (>=3)

## NOTES
# Site files ending in -1 are filled field books. The other site workbooks are empty templates and are not read.
# Yield is the field-weight column, labeled t/ha, multiplied by 1000. The grain-weight column is empty.
# Anthesis date in these rows is days after planting. Grain moisture is percent.
# Planting and harvest dates in the data rows are Excel serials (origin 1899-12-30).
# Coordinates and elevation come from 07CHTTY-Locations.xls, joined on the set number.
# Latitude and longitude in that sheet are degrees and minutes with a hemisphere letter.
# Country and the place name come from the field book. The title does not name one country.

## ISSUES
# The separate grain-yield workbook is a mean across environments, so it is not joined to these site rows.
# La Vizcaina has no coordinates in the locations sheet.
# write_files: missing variables: irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: ears_per_plant_. NA detected: latitude, longitude. contributor:

## NEW VARIABLES
# ears_per_plant_: ears per plant.

## NOT PROCESSED
# Empty field-book templates and the PDF reports.
# 07CHTTY-Combined.xls and 07CHTTY-Grain Yield.xls. Those repeat means across environments.
# Rank, percent of the best check, selection index, and the seed-lot origin code.
# Ear and plant aspect scores, senescence, and columns that are empty in the entry rows.
# Collaborator names and the management label Optimal.


carob_script <- function(path) {

"
International Late Yellow Hybrid Trial - ILYH0704

Summary results and individual trial results from the International Late Yellow Hybrid - ILYH, (Elite Tropical Late Yellow Normal and QPM Hybrid Trial - CHTTY) conducted in 2007.
"

	uri <- "hdl:11529/10540"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Late Yellow Hybrid Trial - ILYH0704",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.6
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "-", "+", ".")] <- NA_character_
		bad <- !is.na(x) & (grepl("[A-Za-z*%#/]", x) | (grepl("-", x) & !grepl("^-[0-9.]+$", x)))
		x[bad] <- NA_character_
		as.numeric(x)
	}
	excel_date <- function(x) {
		if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.character(as.Date(x)))
		n <- num(x)
		as.character(as.Date(n, origin="1899-12-30"))
	}
	num_piece <- function(x) {
		x <- gsub("o|'", "", as.character(x))
		num(x)
	}
	pct <- function(x) {
		x <- num(x)
		out <- rep(NA_character_, length(x))
		ok <- !is.na(x)
		out[ok] <- paste0(x[ok], "%")
		out
	}
	score <- function(x, scale) {
		x <- num(x)
		out <- rep(NA_character_, length(x))
		ok <- !is.na(x)
		out[ok] <- paste0(x[ok], " (", scale, ")")
		out
	}

	floc <- ff[grepl("Locations\\.xls$", basename(ff))]
	loc0 <- carobiner::read.excel(floc, col_names=FALSE, na=c("", "- - -"))
	c2 <- trimws(as.character(loc0[[2]]))
	keep <- !is.na(c2) & !c2 %in% c("Country", "País", "Pais") & nzchar(c2)
	loc0 <- loc0[keep, ]
	lat <- num_piece(loc0[[5]]) + num_piece(loc0[[6]]) / 60
	lon <- num_piece(loc0[[8]]) + num_piece(loc0[[9]]) / 60
	ns <- toupper(trimws(as.character(loc0[[7]])))
	we <- toupper(trimws(as.character(loc0[[10]])))
	i <- which(ns == "S")
	lat[i] <- -abs(lat[i])
	i <- which(we == "W")
	lon[i] <- -abs(lon[i])
	locs <- data.frame(
		set_no = as.integer(num(loc0[[4]])),
		latitude = lat,
		longitude = lon,
		elevation = num(loc0[[11]])
	)
	locs <- locs[!is.na(locs$set_no), ]

	read_trial <- function(f) {
		raw <- carobiner::read.excel(f, sheet="Results", col_names=FALSE)
		hdr <- trimws(as.character(unlist(raw[1, ])))
		col <- function(nm) {
			i <- which(hdr == nm)
			if (length(i) == 0) return(NA_integer_)
			i[1]
		}
		pull <- function(nm) {
			i <- col(nm)
			if (is.na(i)) return(rep(NA_real_, nrow(raw)))
			num(raw[[i]])
		}
		text <- function(nm) {
			i <- col(nm)
			if (is.na(i)) return(rep(NA_character_, nrow(raw)))
			v <- trimws(gsub("\\s+", " ", as.character(raw[[i]])))
			v[v %in% c("", "NA")] <- NA_character_
			v
		}
		entry <- pull("Entry")
		keep <- !is.na(entry)
		if (!any(keep)) return(NULL)
		n <- sum(keep)
		entry <- entry[keep]
		variety <- text("BreedersPedigree1")[keep]
		name <- text("Name")[keep]
		miss <- is.na(variety) | !nzchar(variety)
		variety[miss] <- name[miss]
		y <- pull("GrainYieldTons_FieldWt")[keep]
		if (is.finite(stats::median(y, na.rm=TRUE)) && stats::median(y, na.rm=TRUE) < 40) y <- y * 1000
		stand <- pull("PlantStand_NumPerPlot")[keep]
		area <- pull("PlotSize")[keep]
		density <- rep(NA_real_, n)
		ok <- !is.na(stand) & !is.na(area) & area > 0
		density[ok] <- stand[ok] * 10000 / area[ok]
		data.frame(
			set_no = as.integer(pull("Site")[keep]),
			trial_id = text("NameOfExperiment")[keep],
			variety_code = as.character(entry),
			variety = variety,
			variety_pedigree = variety,
			yield = y,
			yield_moisture = pull("GrainMoisturePer")[keep],
			anthesis_days = pull("AnthesisDate")[keep],
			silking_days = pull("DaysToSilk")[keep],
			asi = pull("ASI")[keep],
			plant_height = pull("PlantHeightCm")[keep],
			ear_height = pull("EarHeightCm")[keep],
			plot_area = area,
			plant_density = density,
			ears_per_plant_ = pull("EPPNo")[keep],
			root_lodging = pct(text("RootLodgingPer")[keep]),
			stalk_lodging = pct(text("StemLodgingPer")[keep]),
			husk = pct(text("BadHuskCoverPer")[keep]),
			grain_texture = score(text("GrainTexture1_5")[keep], "1-5"),
			country = carobiner::fix_name(text("Country")[keep], "title"),
			location = carobiner::fix_name(text("NameOfLocation")[keep], "title"),
			planting_date = excel_date(text("PlantingDate")[keep]),
			harvest_date = excel_date(text("HarvestDate")[keep]),
			ear_rot = pull("EarRotTotalPer")[keep],
			gray_leaf = pull("GrayLeafSpot1_5")[keep],
			south_blight = pull("LeafBlightMaydis1_5")[keep],
			north_blight = pull("LeafBlightTurcicum1_5")[keep],
			streak = pull("VirusMaizeStreak1_5")[keep],
			curvularia = pull("CurvulariaLeafSpot1_5")[keep],
			rust = pull("CommonRust1_5")[keep],
			smut = pull("CommonSmutPer")[keep],
			head_smut = pull("HeadSmutPer")[keep]
		)
	}

	files <- ff[grepl("-1\\.xls$", basename(ff))]
	parts <- lapply(files, read_trial)
	parts <- parts[!vapply(parts, is.null, TRUE)]
	d <- do.call(rbind, parts)
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]
	hit <- match(d$set_no, locs$set_no)
	d$latitude <- locs$latitude[hit]
	d$longitude <- locs$longitude[hit]
	d$elevation <- locs$elevation[hit]
	d$crop <- "maize"
	d$treatment <- d$variety
	d$yield_part <- "grain"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$country[d$country %in% c("", "Na")] <- NA_character_
	d$location[d$location %in% c("", "Na")] <- NA_character_
	d$record_id <- seq_len(nrow(d))

	one_dis <- function(x, disease, percent) {
		keep <- !is.na(x)
		if (!any(keep)) return(NULL)
		val <- x[keep]
		lab <- if (percent) paste0(val, "%") else paste0(val, " (1-5)")
		data.frame(
			record_id = d$record_id[keep],
			disease = disease,
			disease_incidence = if (percent) lab else NA_character_,
			disease_severity = if (percent) NA_character_ else lab
		)
	}
	long <- rbind(
		one_dis(d$ear_rot, "ear rot", TRUE),
		one_dis(d$gray_leaf, "gray leaf spot", FALSE),
		one_dis(d$south_blight, "southern leaf blight", FALSE),
		one_dis(d$north_blight, "maize leaf blight", FALSE),
		one_dis(d$streak, "maize streak virus", FALSE),
		one_dis(d$curvularia, "curvularia leaf spot", FALSE),
		one_dis(d$rust, "rust", FALSE),
		one_dis(d$smut, "smut", TRUE),
		one_dis(d$head_smut, "head smut", TRUE)
	)
	drop <- c("ear_rot", "gray_leaf", "south_blight", "north_blight", "streak", "curvularia", "rust", "smut", "head_smut", "set_no")
	d[drop] <- NULL
	if (all(is.na(d$root_lodging))) d$root_lodging <- NULL
	if (all(is.na(d$stalk_lodging))) d$stalk_lodging <- NULL
	if (all(is.na(d$husk))) d$husk <- NULL
	if (all(is.na(d$grain_texture))) d$grain_texture <- NULL
	if (all(is.na(d$ears_per_plant_))) d$ears_per_plant_ <- NULL
	if (all(is.na(d$silking_days))) d$silking_days <- NULL
	if (all(is.na(d$plant_height))) d$plant_height <- NULL
	if (all(is.na(d$ear_height))) d$ear_height <- NULL
	if (all(is.na(d$yield_moisture))) d$yield_moisture <- NULL
	if (all(is.na(d$plant_density))) d$plant_density <- NULL
	if (is.null(long) || nrow(long) == 0) {
		d$record_id <- NULL
		carobiner::write_files(path, meta, d)
	} else {
		carobiner::write_files(path, meta, d, long=long)
	}
}
