# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Exact farm/station coordinates are not in the data files; midpoint of the
#    Dataverse metadata bounding box in Réunion is used with 30 km uncertainty.
# 2. The gumming (112 rows = 56 plots x 2) and leaf-scald (108 rows = 54 plots
#    x 2) files contain two sequential assessments per elementary plot without
#    a date/cycle column; these are numbered `order = 1` and `2` in file order
#    (which also resolves an otherwise identical pair of rows for R570-7 in
#    gumming).
# 3. The variable description docx for leaf scald copies the gumming wording
#    by mistake; the leaf scald counts are mapped to `disease = "leaf scald"`.
# 4. In `leaf_damage`, sampling dates are given for 2016; in `gumming` and
#    `leaf_scald`, observation years are not given in the CSV files.

## NEW
# trap_crop_: perimeter trap crop treatment ("none" or "erianthus")
# cropping_season_: multi-year sugarcane cropping season (e.g. "2014/2015")
# damaged_stalks_: number of stalks with borer leaf damage
# stalks_observed_: number of stalks observed per plot
# stalk_id_: individual stalk identifier within plot (1-100)
# internodes_: total number of internodes on the sampled stalk
# bored_internodes_: number of stem-borer damaged internodes on the stalk
# bored_stalk_: logical indicator whether the stalk was bored
# whips_: cumulated number of smut whips per plot
# rating_0_..rating_5_, rating_5_side_shoots_: stalk counts per symptom rating class (0-5)
# crop values not in terminag: "erianthus" (Erianthus arundinaceus, clone 28NG7)
# pest_species values not in terminag: "Chilo sacchariphagus"
# disease values not in terminag: "gumming", "leaf scald"

## MISSING
# yield, yield_moisture, yield_part, irrigated, N_fertilizer, P_fertilizer,
# K_fertilizer, planting_date, harvest_date.

## NOTES
# Five raw CSV files from Nibouche et al. (2019, Crop Protection 124: 104877)
# in Réunion: two on-farm trials testing Erianthus arundinaceus (clone 28NG7)
# as a perimeter trap crop (PTC) against sugarcane stem borer Chilo
# sacchariphagus (leaf damage and stalk damage), and three on-station disease
# screening trials (gumming, leaf scald, smut) comparing 28NG7 with sugarcane
# cultivars. `disease_incidence` (%) and `pest_severity` (%) are derived from
# the raw stalk/internode counts, which are also retained.


carob_script <- function(path) {

"
Raw data for Nibouche et al. 2019 Crop Protection Erianthus arundinaceus as a trap crop for the sugarcane stem borer Chilo sacchariphagus: field validation and disease risk assessment

The aim of this study was to determine the efficiency of using Erianthus arundinaceus as a trap crop to control Chilo sacchariphagus in sugarcane in commercial field conditions and to assess the susceptibility of Erianthus arundinaceus to three major sugarcane diseases: smut, leaf-scald and gumming. The raw data presented here were collected in Reunion (1) in a three-year field trial and (2) in three on-station trials.
"

	uri <- "doi:10.18167/DVN1/WQQI0X"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.cropro.2019.104877",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "trap_crop_;variety",
		response_vars = "pest_severity;disease_incidence;bored_internodes_;whips_",
		carob_LLM = "Gemini 3.1 Pro",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_completion = 90,
		carob_effort = 0
	)

	parse_date <- function(x) {
		mon <- c(
			january="01", february="02", march="03", april="04",
			may="05", june="06", july="07", august="08",
			september="09", october="10", november="11", december="12"
		)
		parts <- strsplit(trimws(as.character(x)), "\\s+")
		vapply(parts, function(z) {
			if (length(z) != 3) return(NA_character_)
			m <- unname(mon[tolower(z[2])])
			if (is.na(m)) return(NA_character_)
			sprintf("%s-%s-%02d", z[3], m, as.integer(z[1]))
		}, character(1))
	}

	## 1. Leaf damage (on-farm PTC validation, 2016)
	f1 <- ff[basename(ff) == "Raw_data_for_Nibouche_et_al._2019_Crop_Protection_leaf_damage.csv"]
	r1 <- read.csv(f1, check.names=FALSE)
	d1 <- data.frame(
		trial_id = "leaf_damage",
		treatment = trimws(r1$modality),
		crop = ifelse(grepl("erianthus", tolower(r1$modality)), "erianthus", "sugarcane"),
		variety = ifelse(grepl("erianthus", tolower(r1$modality)), "28NG7", NA_character_),
		trap_crop_ = ifelse(grepl("control", tolower(r1$modality)), "none", "erianthus"),
		rep = match(trimws(r1$Rep), LETTERS),
		plot_id = trimws(r1$Rep),
		date = parse_date(r1$date),
		stress = "pests",
		borer_trial = TRUE,
		pest_species = "Chilo sacchariphagus",
		pest_severity = as.character(round(100 * as.numeric(r1$FA) / as.numeric(r1$nb_stalks), 1)),
		severity_scale = "%",
		damaged_stalks_ = as.integer(r1$FA),
		stalks_observed_ = as.integer(r1$nb_stalks),
		on_farm = TRUE
	)

	## 2. Stalk damage (on-farm PTC validation, 2014-2016, 100 stalks per plot)
	f2 <- ff[basename(ff) == "Raw_data_for_Nibouche_et_al._2019_Crop_Protection_stalk_damage.csv"]
	r2 <- read.csv(f2, check.names=FALSE)
	d2 <- data.frame(
		trial_id = paste0("stalk_damage_", r2$year),
		treatment = trimws(r2$modality),
		crop = "sugarcane",
		trap_crop_ = ifelse(tolower(trimws(r2$modality)) == "control", "none", "erianthus"),
		rep = match(trimws(r2$replication), LETTERS),
		plot_id = trimws(as.character(r2$plot)),
		date = as.character(as.integer(r2$year)),
		stress = "pests",
		borer_trial = TRUE,
		pest_species = "Chilo sacchariphagus",
		pest_severity = as.character(round(100 * as.numeric(r2$bored_inter) / as.numeric(r2$internodes), 1)),
		severity_scale = "%",
		stalk_id_ = as.integer(r2$stalk),
		internodes_ = as.integer(r2$internodes),
		bored_internodes_ = as.integer(r2$bored_inter),
		bored_stalk_ = as.logical(r2$bored_stalk),
		on_farm = TRUE
	)

	## 3-5. On-station disease screening trials (28NG7 = Erianthus arundinaceus; others = sugarcane)
	read_disease <- function(file, trial, dis_name) {
		r <- read.csv(file, check.names=FALSE)
		cv <- trimws(as.character(r$cultivar))
		out <- data.frame(
			trial_id = trial,
			treatment = cv,
			crop = ifelse(cv == "28NG7", "erianthus", "sugarcane"),
			variety = cv,
			trap_crop_ = "none",
			rep = as.integer(r$block),
			stress = "disease",
			disease = dis_name,
			on_farm = FALSE
		)
		if ("elementary_plot" %in% names(r)) {
			out$plot_id <- trimws(as.character(r$elementary_plot))
			n_plots <- length(unique(out$plot_id))
			if (nrow(r) == 2 * n_plots) {
				out$order <- rep(1:2, each=n_plots)
			}
		}
		if ("year" %in% names(r)) {
			out$cropping_season_ <- trimws(as.character(r$year))
			out$date <- substr(out$cropping_season_, 1, 4)
		}
		if ("whips" %in% names(r)) {
			out$whips_ <- as.integer(r$whips)
		}
		if ("rating_0" %in% names(r)) {
			rcols <- grep("^rating_", names(r), value=TRUE)
			tot <- rowSums(r[, rcols, drop=FALSE], na.rm=TRUE)
			symp <- tot - as.numeric(r$rating_0)
			out$disease_incidence <- as.character(round(100 * symp / tot, 2))
			out$severity_scale <- "0-5"
			for (rc in rcols) {
				out[[paste0(rc, "_")]] <- as.integer(r[[rc]])
			}
		}
		out
	}

	f3 <- ff[basename(ff) == "Raw_data_for_Nibouche_et al._2019_Crop_Protection_gumming.csv"]
	f4 <- ff[basename(ff) == "Raw_data_for_Nibouche_et_al._2019_Crop_Protection_leaf_scald.csv"]
	f5 <- ff[basename(ff) == "Raw_data_for_Nibouche_et_al._2019_Crop_Protection_smut.csv"]

	d3 <- read_disease(f3, "gumming", "gumming")
	d4 <- read_disease(f4, "leaf_scald", "leaf scald")
	d5 <- read_disease(f5, "smut", "smut")

	d <- carobiner::bindr(d1, d2, d3, d4, d5)

	d$country <- "Réunion"
	## Midpoint of Dataverse geographicBoundingBox (55.291..55.598 E, -21.346..-20.911 S)
	d$latitude <- -21.128
	d$longitude <- 55.445
	d$geo_uncertainty <- 30000
	d$geo_source <- "Dataverse metadata geographicBoundingBox midpoint (Réunion)"
	d$geo_from_source <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
