# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot grain yield, flowering, and maturity from Field/data.csv, joined to Field/cov.csv.
# GY is t/ha of dry grain (README: GW is dry grain weight and GY is derived from it), stored as kg/ha with yield_moisture 0.
# Farmer rank is the General trait only (1-3, and 4 = not in the top 3), joined by environment and variety.
# Country Madagascar is the dataset geographicCoverage and the title. Coordinates, region, and commune are columns in cov.csv.

## ISSUES
# Mineral fertilizer is a product mass (kg/ha) with no grade, so N, P, and K are not set. Organic fertilizer is a product mass with no type.
# PRECEDENT_CATEGORY "Other" is not mapped to previous_crop. District, soil color, soil fertility, farmer age, and farm area are not mapped.
# ELE is a raster value in the README, not a field measurement. Some coordinates are village approximations and all released coordinates were jittered by less than 1 km.
# write_files: missing variables: K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: mineral_fertilizer_rate_, visual_score_. NA detected: yield.

## NOT PROCESSED
# Pedigree/A.csv relationship matrix. Farmer traits other than General (Earliness, Diseases, Height, Lodging, Grain, Production).
# GY_m2, GW_m2, and PSH (harvested hills, not plant density).

## NEW VARIABLES
# mineral_fertilizer_rate_: FERT_MIN_DOSE, kg/ha of mineral product, grade unknown. 0 means none applied.
# visual_score_: EVAL, visual score. README scale is 1 (worst) to 9 (best); 0 is kept as recorded.


carob_script <- function(path) {

"
Upland Rice Multi-Environment Trials in Madagascar (Field and Pedigree Data)

This dataset contains data from multi-environment upland rice variety trials conducted in Madagascar between 2022 and 2025 as part of the DINAAMICC and MAKIS projects. The trials were carried out across multiple locations and farmer fields and include data on grain yield, flowering time, and farmers’ varietal appreciation.
The R code used for data processing, analysis, and visualization is available in the associated CIRAD GitLab repository: https://gitlab.cirad.fr/agap/giv/MET-Madagascar.
"

	uri <- "doi:10.18167/DVN1/XKQ9SX"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "DINAAMICC; MAKIS",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;maturity_days;rank_score",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.8
	)

	mdy <- function(x) {
		x <- sub(" .*", "", trimws(as.character(x)))
		x[x %in% c("", "NA")] <- NA
		# README: dates are MM/DD/YYYY
		as.character(as.Date(x, "%m/%d/%Y"))
	}

	f1 <- ff[basename(ff) == "data.csv"]
	f2 <- ff[basename(ff) == "cov.csv"]
	f3 <- ff[basename(ff) == "eval.csv"]
	f4 <- ff[basename(ff) == "pedigree.csv"]

	r <- read.csv(f1)
	r1 <- read.csv(f2)
	r2 <- read.csv(f3)
	r3 <- read.csv(f4)

	r2 <- r2[r2$TRAIT == "General", c("ENV_ID", "GEN", "RANK")]
	r2 <- r2[!duplicated(r2[c("ENV_ID", "GEN")]), ]
	names(r2)[names(r2) == "RANK"] <- "rank_score"

	r3$variety <- trimws(r3$long_name)
	blank <- is.na(r3$variety) | r3$variety == ""
	r3$variety[blank] <- r3$name[blank]
	dame <- trimws(r3$dame)
	sire <- trimws(r3$sire)
	dame[is.na(dame)] <- ""
	sire[is.na(sire)] <- ""
	r3$variety_pedigree <- ifelse(dame != "" & sire != "", paste(dame, "x", sire), NA)
	yr <- trimws(as.character(r3$year_of_registration))
	yr[yr == ""] <- NA
	r3$variety_release_year <- as.integer(yr)
	r3 <- r3[, c("name", "variety", "variety_pedigree", "variety_release_year")]

	keep <- c("ENV_ID", "REGION", "COMMUNE", "LONG", "LAT", "ELE", "SOWING_DATE",
		"PRECEDENT_CATEGORY", "FERT_ORG_DOSE", "FERT_MIN_DOSE", "FIELD_TYPE", "GENDER", "PLOT_SIZE")
	r1 <- r1[, keep]
	r <- merge(r, r1, by = "ENV_ID", all.x = TRUE)
	r <- merge(r, r2, by = c("ENV_ID", "GEN"), all.x = TRUE)
	r <- merge(r, r3, by.x = "GEN", by.y = "name", all.x = TRUE)
	r$variety[is.na(r$variety)] <- r$GEN[is.na(r$variety)]

	pc <- c(
		"Fallow" = "none",
		"Legumes" = "legume",
		"Cassava" = "cassava",
		"Potato" = "potato",
		"Vegetables" = "vegetable",
		"Sweet Potato" = "sweetpotato",
		"Rice" = "rice",
		"Maize" = "maize",
		"Maize + Legumes" = "maize;legume"
	)
	prev <- unname(pc[trimws(r$PRECEDENT_CATEGORY)])

	# README: Tanety = rainfed upland; Baiboho = poorly irrigated; Paddy Field = irrigated. Blank left unknown.
	ft <- trimws(r$FIELD_TYPE)
	ft[ft == ""] <- NA
	irr <- rep(NA, nrow(r))
	irr[ft == "Tanety"] <- FALSE
	irr[ft %in% c("Baiboho", "Paddy Field")] <- TRUE

	g <- trimws(r$GENDER)
	farmer <- rep(NA_character_, nrow(r))
	farmer[g == "M"] <- "man"
	farmer[g == "W"] <- "woman"

	d <- data.frame(
		trial_id = r$ENV_ID,
		crop = "rice",
		variety = r$variety,
		variety_code = r$GEN,
		variety_pedigree = r$variety_pedigree,
		variety_release_year = r$variety_release_year,
		treatment = r$variety,
		rep = as.integer(r$REP),
		plot_id = as.character(r$PLOT),
		plot_area = r$PLOT_SIZE,
		country = "Madagascar",
		adm1 = trimws(r$REGION),
		location = trimws(r$COMMUNE),
		latitude = r$LAT,
		longitude = r$LONG,
		elevation = r$ELE,
		geo_from_source = TRUE,
		planting_date = mdy(r$SOWING_DATE),
		flowering_date = mdy(r$FLO_DATE),
		maturity_date = mdy(r$MAT_DATE),
		harvest_date = mdy(r$HARVEST_DATE),
		flowering_days = r$DTF,
		maturity_days = r$DTM,
		yield = r$GY * 1000,
		yield_part = "grain",
		yield_moisture = 0,
		previous_crop = prev,
		OM_amount = r$FERT_ORG_DOSE * 1000,
		mineral_fertilizer_rate_ = r$FERT_MIN_DOSE,
		irrigated = as.logical(irr),
		visual_score_ = r$EVAL,
		rank_score = as.integer(r$rank_score),
		farmer_gender = farmer,
		# Every cov row has a farmer gender. README also mentions research stations.
		on_farm = TRUE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
