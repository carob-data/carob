# R script for "carob"
# license: GPL (>=3)

## NOTES
# Leaf and stalk files are the borer scores. Gumming, leaf scald, and smut are the cultivar disease counts. Counts are left as recorded (stalks in a score class, bored internodes, whips), not converted to a percent.
# Country is France from geographicCoverage. The csv files do not name a finer place, so Réunion is not added. The metadata bounding box is not used as a point.
# Leaf-scald variable note in the docx repeats the gumming wording. The counts are still taken from the leaf-scald file.

## ISSUES
# Disease-trial cultivars are not labeled Erianthus or sugarcane in the files, so those rows have no crop.
# No yield is in these files.
# The gumming file repeats one identical plot row, so write_files reports duplicate records.
# write_files: missing on_farm, yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# unknown variables: season_ (season?), trap_crop_ (crop?), stalk_ (stalk_lodging?), damaged_stalks_, stalks_observed_, internodes_, bored_internodes_, bored_stalk_, whips_, rating_0_, rating_1_, rating_2_, rating_3_, rating_4_, rating_5_, rating_5_side_shoots_. invalid terms: crop: erianthus. duplicates: duplicate records detected.

## NOT PROCESSED
# Variables description docx, other than the column definitions used above.
# rating_0 is kept with the other rating counts. The docx lists it and then says symptoms rated 1 to 5.


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
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "damaged_stalks_;bored_internodes_;whips_",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 50,
		carob_effort = 0.55
	)

	parse_date <- function(x) {
		mon <- c(january="01", february="02", march="03", april="04", may="05", june="06", july="07", august="08", september="09", october="10", november="11", december="12")
		parts <- strsplit(trimws(x), " ")
		vapply(parts, function(z) {
			if (length(z) != 3) return(NA_character_)
			m <- unname(mon[tolower(z[2])])
			if (is.na(m)) return(NA_character_)
			sprintf("%s-%s-%02d", z[3], m, as.integer(z[1]))
		}, character(1))
	}

	blank <- function(n) {
		data.frame(
			trial_id = rep(NA_character_, n),
			treatment = rep(NA_character_, n),
			crop = rep(NA_character_, n),
			variety = rep(NA_character_, n),
			rep = rep(NA_integer_, n),
			plot_id = rep(NA_character_, n),
			date = rep(NA_character_, n),
			season_ = rep(NA_character_, n),
			trap_crop_ = rep(NA_character_, n),
			stress = rep(NA_character_, n),
			damaged_stalks_ = rep(NA_real_, n),
			stalks_observed_ = rep(NA_real_, n),
			internodes_ = rep(NA_real_, n),
			bored_internodes_ = rep(NA_real_, n),
			bored_stalk_ = rep(NA_integer_, n),
			stalk_ = rep(NA_integer_, n),
			whips_ = rep(NA_real_, n),
			rating_0_ = rep(NA_real_, n),
			rating_1_ = rep(NA_real_, n),
			rating_2_ = rep(NA_real_, n),
			rating_3_ = rep(NA_real_, n),
			rating_4_ = rep(NA_real_, n),
			rating_5_ = rep(NA_real_, n),
			rating_5_side_shoots_ = rep(NA_real_, n)
		)
	}

	f1 <- ff[basename(ff) == "Raw_data_for_Nibouche_et_al._2019_Crop_Protection_leaf_damage.csv"]
	r <- read.csv(f1, check.names=FALSE)
	d1 <- blank(nrow(r))
	d1$trial_id <- "leaf_damage"
	d1$treatment <- trimws(r$modality)
	d1$crop <- ifelse(grepl("erianthus", tolower(r$modality)), "erianthus", "sugarcane")
	d1$rep <- match(trimws(r$Rep), LETTERS)
	d1$plot_id <- trimws(r$Rep)
	d1$date <- parse_date(r$date)
	d1$trap_crop_ <- ifelse(grepl("control", tolower(r$modality)), "none", "erianthus")
	d1$stress <- "pests"
	d1$damaged_stalks_ <- as.numeric(r$FA)
	d1$stalks_observed_ <- as.numeric(r$nb_stalks)

	f2 <- ff[basename(ff) == "Raw_data_for_Nibouche_et_al._2019_Crop_Protection_stalk_damage.csv"]
	r <- read.csv(f2, check.names=FALSE)
	d2 <- blank(nrow(r))
	d2$trial_id <- "stalk_damage"
	d2$treatment <- trimws(r$modality)
	d2$crop <- "sugarcane"
	d2$rep <- match(trimws(r$replication), LETTERS)
	d2$plot_id <- trimws(as.character(r$plot))
	d2$date <- as.character(r$year)
	d2$trap_crop_ <- ifelse(tolower(trimws(r$modality)) == "control", "none", "erianthus")
	d2$stress <- "pests"
	d2$internodes_ <- as.numeric(r$internodes)
	d2$bored_internodes_ <- as.numeric(r$bored_inter)
	d2$bored_stalk_ <- as.integer(r$bored_stalk)
	d2$stalk_ <- as.integer(r$stalk)

	disease <- function(file, trial) {
		r <- read.csv(file, check.names=FALSE)
		d <- blank(nrow(r))
		d$trial_id <- trial
		d$treatment <- trimws(as.character(r$cultivar))
		d$variety <- trimws(as.character(r$cultivar))
		d$rep <- as.integer(r$block)
		d$stress <- "disease"
		if ("elementary_plot" %in% names(r)) d$plot_id <- trimws(as.character(r$elementary_plot))
		if ("whips" %in% names(r)) d$whips_ <- as.numeric(r$whips)
		if ("year" %in% names(r)) d$season_ <- trimws(as.character(r$year))
		for (k in 0:5) {
			col <- paste0("rating_", k)
			if (col %in% names(r)) d[[paste0(col, "_")]] <- as.numeric(r[[col]])
		}
		if ("rating_5_side_shoots" %in% names(r)) {
			d$rating_5_side_shoots_ <- as.numeric(r$rating_5_side_shoots)
		}
		d
	}

	f3 <- ff[basename(ff) == "Raw_data_for_Nibouche_et al._2019_Crop_Protection_gumming.csv"]
	f4 <- ff[basename(ff) == "Raw_data_for_Nibouche_et_al._2019_Crop_Protection_leaf_scald.csv"]
	f5 <- ff[basename(ff) == "Raw_data_for_Nibouche_et_al._2019_Crop_Protection_smut.csv"]
	d <- rbind(d1, d2, disease(f3, "gumming"), disease(f4, "leaf_scald"), disease(f5, "smut"))
	d$country <- "France"
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
