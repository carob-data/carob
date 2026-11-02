# R script for "carob"
# license: GPL (>=3)

## NOTES
# Each location workbook is a variety-mean table. Grain yield is labeled kg/ha.
# Days to pollen is stored as anthesis_days. Moisture is percent.
# Site coordinates, elevation, and dates come from 05EVT17HLW-Locations.xls.
# Latitude and longitude are degrees and minutes with a hemisphere letter.
# Planting and harvest dates in that sheet are Excel serials (origin 1899-12-30).
# Lodging, husk cover, and disease scores keep the scale printed in the unit row.
# Smut Carbon is carbón (smut). Helm. turcicum is stored as maize leaf blight.
# Plant aspect uses the 1-5 or 1-9 score in aspect. Ears per plant is a ratio.

## ISSUES
# Bako and Ambo are both published at 8°58' N, 77°52' E. That longitude is not in Ethiopia. The coordinates are left as published.
# Cuendo harvest serial 38364 (2005-01-12) is before planting serial 38463 (2005-04-21). Bako has no harvest date.
# write_files: missing variables: K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: ears_per_plant_. datespan: 8 records with harvest_date within 45 days of planting_date. coordinates in wrong country: Ethiopia/India. duplicate coordinates: fewer coordinates than locations. invalid terms: disease: fusarium, smut, southern rust. contributor:

## NEW VARIABLES
# ears_per_plant_: ears per plant, ratio, as printed.

## NOT PROCESSED
# Percent of the best check. It is computed from yield.
# Plants germinated and plants harvested, counts with no plot area.
# 05EVT17HLW-Combined.xls, which repeats the location means.
# Cooperator names.
# Health scores are not in these workbooks.


carob_script <- function(path) {

"
International Late White Variety Trial - ILWV0513

Summary results and individual trial results from the International Late White Variety - ILWV, (Highland Late Normal and QPM Varieties - EVT17HLW) conducted in 2005.
"

	uri <- "hdl:11529/10595"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Late White Variety Trial - ILWV0513",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.6
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

	floc <- ff[basename(ff) == "05EVT17HLW-Locations.xls"]
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

	read_trial <- function(f) {
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
			set_no = as.integer(sub("^05EVT17HLW", "", id)),
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
			keep <- !is.na(txt) & nzchar(txt)
			if (!any(keep)) next
			inc <- txt[keep]
			is_pct <- grepl("%$", inc)
			piece <- data.frame(
				set_no = wide$set_no[keep],
				row_i = wide$row_i[keep],
				disease = NA_character_,
				disease_incidence = NA_character_,
				disease_severity = NA_character_,
				pest_species = NA_character_,
				pest_severity = NA_character_
			)
			if (sp[2] == "disease") {
				piece$disease <- sp[3]
				piece$disease_incidence <- ifelse(is_pct, inc, NA_character_)
				piece$disease_severity <- ifelse(is_pct, NA_character_, inc)
			} else {
				piece$pest_species <- sp[3]
				piece$pest_severity <- inc
			}
			long <- rbind(long, piece)
		}
		list(wide=wide, long=long)
	}

	trials <- ff[grepl("^05EVT17HLW[0-9]", basename(ff)) & grepl("\\.xls$", basename(ff), ignore.case=TRUE)]
	parts <- lapply(trials, read_trial)
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
