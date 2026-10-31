# R script for "carob"
# license: GPL (>=3)

## NOTES
# Each location workbook except 05CHTTY8.xls is a variety-mean table. Grain yield is labeled kg/ha.
# 05CHTTY8.xls has no grain-yield column. The locations comment for that set says without grain yield. Flowering, height, lodging, and scores from that sheet are kept, with yield missing.
# The English header Fluorescence on that sheet is the Spanish field mEarRotStenoc1_5, stored as stenocarpella ear rot.
# Days to pollen is stored as anthesis_days. Moisture is percent.
# Site coordinates, elevation, and dates come from 05CHTTY-Locations.xls.
# Latitude and longitude are degrees and minutes with a hemisphere letter.
# Planting and harvest dates in that sheet are Excel serials (origin 1899-12-30).
# Lodging, husk cover, and disease scores keep the scale printed in the unit row.
# Helm. turcicum is stored as maize leaf blight. Bipolaris maydis is stored as southern leaf blight.
# Plant aspect uses the 1-5 or 1-9 score in aspect. Ears per plant is a ratio.

## ISSUES
# Binh Duong has degrees but the minutes cells are blank, so that site has no coordinates.
# Neiva-Huila and Neiva-Huila (2) share one published coordinate. The two Aragua sets do too.
# El Progreso, Pejibaye is published at 9°8' N, 84°32' W, which does not fall on land. The coordinate is left as published.
# El Progreso and one Aragua set have no planting or harvest date. Samaru has no harvest date. Agua Fria set 8 has no grain yield.
# A dot in the aspect columns of 05CHTTY7.xls is read as missing.
# write_files: missing variables: K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: ears_per_plant_, endosperm_hardness_. coordinates not on land: Costa Rica. duplicate coordinates: fewer coordinates than locations. NA detected: yield, latitude, longitude. invalid terms: disease: aspergillus flavus, fusarium, leaf diseases, leaf spot, rhizoctonia, smut, southern rust, stunt, tar spot. invalid terms: pest_species: earworm, maize weevil. contributor:

## NEW VARIABLES
# ears_per_plant_: ears per plant, ratio, as printed.
# endosperm_hardness_: endosperm hardness score. The unit row is 1-5. No scale is applied to the number.

## NOT PROCESSED
# Percent of the best check. It is computed from yield.
# Plants germinated and plants harvested, counts with no plot area.
# Health Plant and Health Mz scores in 05CHTTY27.xls. The scale is not defined beyond the 1-5 unit cell.
# Ear-position ratio in 05CHTTY8.xls. It is ear height divided by plant height.
# Origin codes in 05CHTTY8.xls. They are seed-lot identifiers.
# 05CHTTY-Combined.xls, which repeats the location means.
# Cooperator names.


carob_script <- function(path) {

"
International Late Yellow Hybrid Trial - ILYH0506

Summary results and individual trial results from the International Late Yellow Hybrid - ILYH, (Elite Tropical Late Yellow Normal and QPM Hybrid Trial - CHTTY) conducted in 2005.
"

	uri <- "hdl:11529/10583"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Late Yellow Hybrid Trial - ILYH0506",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.7
	)

	excel_date <- function(x) {
		if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.character(as.Date(x)))
		x <- trimws(as.character(x))
		x[x %in% c("", "- - -", "NA")] <- NA_character_
		as.character(as.Date(as.numeric(x), origin="1899-12-30"))
	}
	num_piece <- function(x) {
		x <- gsub("o|'", "", as.character(x))
		x <- trimws(x)
		x[x %in% c("", "- - -", "NA")] <- NA_character_
		as.numeric(x)
	}
	empty_long <- function() {
		data.frame(
			set_no = integer(),
			row_i = integer(),
			disease = character(),
			disease_incidence = character(),
			disease_severity = character(),
			pest_species = character(),
			pest_severity = character()
		)
	}
	add_score <- function(long, set_no, row_i, txt, kind, name, is_pct) {
		keep <- !is.na(txt) & nzchar(txt)
		if (!any(keep)) return(long)
		piece <- data.frame(
			set_no = set_no[keep],
			row_i = row_i[keep],
			disease = NA_character_,
			disease_incidence = NA_character_,
			disease_severity = NA_character_,
			pest_species = NA_character_,
			pest_severity = NA_character_
		)
		inc <- txt[keep]
		pct <- is_pct[keep]
		if (kind == "disease") {
			piece$disease <- name
			piece$disease_incidence <- ifelse(pct, inc, NA_character_)
			piece$disease_severity <- ifelse(pct, NA_character_, inc)
		} else {
			piece$pest_species <- name
			piece$pest_severity <- inc
		}
		rbind(long, piece)
	}

	floc <- ff[basename(ff) == "05CHTTY-Locations.xls"]
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
	country <- carobiner::fix_name(trimws(as.character(loc0[[2]])), "title")
	country[country == "México"] <- "Mexico"
	country[country == "Viet Nam"] <- "Vietnam"
	locs <- data.frame(
		set_no = as.integer(loc0[[4]]),
		country = country,
		location = carobiner::fix_name(trimws(as.character(loc0[[3]])), "title"),
		latitude = lat,
		longitude = lon,
		elevation = as.numeric(loc0[[11]]),
		planting_date = excel_date(loc0[[12]]),
		harvest_date = excel_date(loc0[[13]])
	)

	read_standard <- function(f) {
		r <- carobiner::read.excel(f, col_names=FALSE, na=c("", "- - -", "."))
		blank <- function(x) {
			x <- as.character(x)
			x[is.na(x)] <- ""
			trimws(x)
		}
		nm <- trimws(paste(blank(unlist(r[5, ])), blank(unlist(r[6, ]))))
		unit <- blank(unlist(r[7, ]))
		sub <- r[9:nrow(r), ]
		lab <- trimws(as.character(sub[[2]]))
		lab[is.na(lab)] <- ""
		end <- which(lab %in% c("Mean", "Maximum", "Minimum"))[1]
		if (!is.na(end)) sub <- sub[seq_len(end - 1), ]
		lab <- trimws(as.character(sub[[2]]))
		lab[is.na(lab)] <- ""
		sub <- sub[nzchar(lab) & !lab %in% c("Checks (Testigos)", "Check Means"), ]
		n <- nrow(sub)
		if (n == 0) return(NULL)
		find <- function(pat) {
			i <- grep(pat, nm, ignore.case=TRUE)
			if (length(i) == 0) return(NA_integer_)
			i[1]
		}
		pull <- function(i) {
			if (is.na(i)) return(rep(NA_real_, n))
			as.numeric(sub[[i]])
		}
		as_text <- function(i) {
			if (is.na(i)) return(rep(NA_character_, n))
			v <- as.numeric(sub[[i]])
			out <- rep(NA_character_, n)
			ok <- !is.na(v)
			if (grepl("%", unit[i])) out[ok] <- paste0(v[ok], "%")
			else if (grepl("1-5", unit[i])) out[ok] <- paste0(v[ok], " (1-5)")
			else if (grepl("1-9", unit[i])) out[ok] <- paste0(v[ok], " (1-9)")
			else out[ok] <- as.character(v[ok])
			out
		}
		iy <- find("Yield")
		if (is.na(iy) || !grepl("Kg/ha", unit[iy])) return(NULL)
		id <- sub("\\.xls$", "", basename(f), ignore.case=TRUE)
		wide <- data.frame(
			set_no = as.integer(sub("^05CHTTY", "", id)),
			row_i = seq_len(n),
			variety_code = trimws(as.character(sub[[1]])),
			variety = trimws(as.character(sub[[2]])),
			yield = pull(iy),
			anthesis_days = pull(find("Pollen")),
			silking_days = pull(find("Silk")),
			asi = rep(NA_real_, n),
			plant_height = pull(find("Plnt ht")),
			ear_height = pull(find("Ear ht")),
			yield_moisture = pull(find("^Moist")),
			root_lodging = as_text(find("Root Lodg")),
			stalk_lodging = as_text(find("Stalk Lodg|Stem Lodg")),
			aspect = pull(find("Plant Asp")),
			ear_aspect = pull(find("Ear Asp")),
			husk = as_text(find("Husk")),
			grain_texture = rep(NA_character_, n),
			ears_per_plant_ = pull(find("Ears/")),
			endosperm_hardness_ = pull(find("Endo"))
		)
		specs <- list(
			c("Head Smut", "disease", "head smut"),
			c("Fusa", "disease", "fusarium"),
			c("Smut", "disease", "smut"),
			c("GLS|Gray Leaf", "disease", "gray leaf spot"),
			c("turcicum", "disease", "maize leaf blight"),
			c("Southern rust", "disease", "southern rust"),
			c("Common Rust", "disease", "rust"),
			c("Puccinia", "disease", "rust"),
			c("MSV|Streak", "disease", "maize streak virus"),
			c("Ear Rot", "disease", "ear rot"),
			c("Bipolaris|maydis", "disease", "southern leaf blight"),
			c("Rhizoc", "disease", "rhizoctonia"),
			c("Leaf spot", "disease", "leaf spot"),
			c("Stunt", "disease", "stunt"),
			c("Leaf Diseases", "disease", "leaf diseases"),
			c("Stem Borer", "pest", "stem borer"),
			c("Ear Worm", "pest", "earworm")
		)
		used <- c()
		long <- empty_long()
		for (sp in specs) {
			hits <- grep(sp[1], nm, ignore.case=TRUE)
			hits <- hits[!hits %in% used]
			if (length(hits) == 0) next
			i <- hits[1]
			used <- c(used, i)
			txt <- as_text(i)
			is_pct <- rep(grepl("%", unit[i]), length(txt))
			long <- add_score(long, wide$set_no, wide$row_i, txt, sp[2], sp[3], is_pct)
		}
		list(wide=wide, long=long)
	}

	read_noyield <- function(f) {
		r <- carobiner::read.excel(f, col_names=FALSE, na=c("", "- - -", "."))
		hdr <- trimws(as.character(unlist(r[1, ])))
		hdr[is.na(hdr)] <- ""
		lab1 <- trimws(as.character(r[[1]]))
		lab2 <- trimws(as.character(r[[2]]))
		lab1[is.na(lab1)] <- ""
		lab2[is.na(lab2)] <- ""
		stopw <- c("Mean", "Maximum", "Minimum", "Promedio", "LSD", "CV", "MSe")
		hit <- which(lab1 %in% stopw | lab2 %in% stopw)
		last <- nrow(r)
		if (length(hit)) last <- min(hit) - 1
		if (last < 3) return(NULL)
		sub <- r[3:last, ]
		lab1 <- trimws(as.character(sub[[1]]))
		lab2 <- trimws(as.character(sub[[2]]))
		lab1[is.na(lab1)] <- ""
		lab2[is.na(lab2)] <- ""
		keep <- nzchar(lab2) & !lab2 %in% c("Pedigree", "Genealogia") & !grepl("[A-Za-z]", lab1)
		sub <- sub[keep, ]
		n <- nrow(sub)
		if (n == 0) return(NULL)
		find <- function(pat) {
			i <- grep(pat, hdr, ignore.case=TRUE)
			if (length(i) == 0) return(NA_integer_)
			i[1]
		}
		pull <- function(i) {
			if (is.na(i)) return(rep(NA_real_, n))
			as.numeric(sub[[i]])
		}
		as_text <- function(i) {
			if (is.na(i)) return(rep(NA_character_, n))
			v <- as.numeric(sub[[i]])
			out <- rep(NA_character_, n)
			ok <- !is.na(v)
			if (grepl("%", hdr[i])) out[ok] <- paste0(v[ok], "%")
			else if (grepl("1-5", hdr[i])) out[ok] <- paste0(v[ok], " (1-5)")
			else out[ok] <- as.character(v[ok])
			out
		}
		id <- sub("\\.xls$", "", basename(f), ignore.case=TRUE)
		wide <- data.frame(
			set_no = as.integer(sub("^05CHTTY", "", id)),
			row_i = seq_len(n),
			variety_code = trimws(as.character(sub[[1]])),
			variety = trimws(as.character(sub[[2]])),
			yield = rep(NA_real_, n),
			anthesis_days = pull(find("Male")),
			silking_days = pull(find("Fem")),
			asi = pull(find("^ASI$")),
			plant_height = pull(find("Plt\\. Hght")),
			ear_height = pull(find("Ear Hght")),
			yield_moisture = rep(NA_real_, n),
			root_lodging = as_text(find("Rt Ldg")),
			stalk_lodging = as_text(find("Stlk Ldg")),
			aspect = pull(find("Plt\\. Asp")),
			ear_aspect = pull(find("Ear Asp")),
			husk = as_text(find("Husk")),
			grain_texture = as_text(find("^Text")),
			ears_per_plant_ = pull(find("Ears/")),
			endosperm_hardness_ = rep(NA_real_, n)
		)
		long <- empty_long()
		specs <- list(
			c("Ear Rot", "disease", "ear rot"),
			c("Fluores", "disease", "stenocarpella ear rot"),
			c("A\\. flav|flavus", "disease", "aspergillus flavus"),
			c("Tar Spot", "disease", "tar spot"),
			c("Weevil", "pest", "maize weevil")
		)
		used <- c()
		for (sp in specs) {
			hits <- grep(sp[1], hdr, ignore.case=TRUE)
			hits <- hits[!hits %in% used]
			if (length(hits) == 0) next
			i <- hits[1]
			used <- c(used, i)
			txt <- as_text(i)
			is_pct <- rep(grepl("%", hdr[i]), length(txt))
			long <- add_score(long, wide$set_no, wide$row_i, txt, sp[2], sp[3], is_pct)
		}
		list(wide=wide, long=long)
	}

	read_one <- function(f) {
		r <- carobiner::read.excel(f, col_names=FALSE, na=c("", "- - -", "."))
		h1 <- as.character(unlist(r[1, ]))
		h1[is.na(h1)] <- ""
		if (any(grepl("Days Flwr", h1))) return(read_noyield(f))
		read_standard(f)
	}

	trials <- ff[grepl("^05CHTTY[0-9]", basename(ff)) & grepl("\\.xls$", basename(ff), ignore.case=TRUE)]
	parts <- lapply(trials, read_one)
	parts <- parts[!vapply(parts, is.null, logical(1))]
	d <- do.call(rbind, lapply(parts, function(p) p$wide))
	long <- do.call(rbind, lapply(parts, function(p) p$long))
	d <- merge(d, locs, by="set_no", all.x=TRUE)
	d$crop <- "maize"
	d$treatment <- d$variety
	d$yield_part <- "grain"
	d$trial_id <- paste(d$set_no, d$location, sep="_")
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$row_key <- paste(d$set_no, d$row_i, sep="-")
	d$record_id <- seq_len(nrow(d))
	if (nrow(long) > 0) {
		long$row_key <- paste(long$set_no, long$row_i, sep="-")
		long$record_id <- d$record_id[match(long$row_key, d$row_key)]
		long <- long[!is.na(long$record_id), ]
		long$set_no <- NULL
		long$row_i <- NULL
		long$row_key <- NULL
	}
	d$row_key <- NULL
	d$row_i <- NULL
	d$set_no <- NULL
	if (all(is.na(d$asi))) d$asi <- NULL
	if (all(is.na(d$grain_texture))) d$grain_texture <- NULL
	if (all(is.na(d$endosperm_hardness_))) d$endosperm_hardness_ <- NULL
	if (all(is.na(d$ears_per_plant_))) d$ears_per_plant_ <- NULL

	if (nrow(long) == 0) carobiner::write_files(path, meta, d)
	else carobiner::write_files(path, meta, d, long=long)
}
