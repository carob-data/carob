# R script for "carob"
# license: GPL (>=3)

## NOTES
# 169 extra-early quality-protein maize entries under Striga, three environments, 2019-2020.
# The data file has no separate year column. Loc codes are ABJI20 (Abuja 2020), MKWI19 (Mokwa 2019) and MKWI20 (Mokwa 2020), which matches the dictionary (Mokwa and Abuja, 2019 and 2020).
# YIELD is grain yield in kg/ha.
# STRIGA is the Striga damage rating at 10 weeks after planting (passed to mz_pd_long).
# CO1S and CO2S are transformed emerged-Striga counts at 8 and 10 weeks. The transform is not stated (values are about 0-2), so they were kept as extra columns and not treated as plant counts.
# Pedigrees that start with "Check" are checks; the other entries are inbreds. The check label and the inbred name are both kept in variety.

## ISSUES
# No coordinates, fertilizer, irrigation, grain moisture, or day of planting. Year was taken from the location code.
# No raw Striga counts, only transformed values.
# ears_per_plant_ reaches 8, which is outside a usual ears-per-plant range. Left as recorded.
# One Striga rating is missing.
# Expect missing longitude, latitude, geo_from_source, yield_moisture, irrigated, N/P/K, harvest_date, and unknown-variable warnings.

## NOT PROCESSED
# phenotypic-data.csv, ID: row number.
# column_metadata.csv: definitions used to map columns. It names a YEAR column that is not in the data file (year is inside Loc).

## NEW VARIABLES
# ears_per_plant_: ears harvested divided by plants in the plot (dimensionless).
# striga_count_8wap_transformed_: transformed number of emerged Striga plants at 8 weeks after planting. Transform not stated.
# striga_count_10wap_transformed_: transformed number of emerged Striga plants at 10 weeks after planting. Transform not stated.


carob_script <- function(path) {

"
Phenotypic data of 169 extra-early maturing quality protein maize  evaluated under Striga conditions across 3 environments, 2019-2020

Phenotypic data of 169 extra-early maturing quality protein maize  evaluated under Striga conditions across 3 environments, 2019-2020
"

	uri <- "doi:10.25502/14yp-5w46/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Stress Tolerant Maize for Africa (STMA) Phase II",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;ear_aspect",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4,
		notes = NA
	)

	f <- ff[basename(ff) == "phenotypic-data.csv"]
	r <- read.csv(f)

	loc <- trimws(as.character(r$Loc))
	location <- loc
	location[grepl("^ABJI", loc)] <- "Abuja"
	location[grepl("^MKWI", loc)] <- "Mokwa"
	year <- rep(NA_character_, length(loc))
	year[grepl("19$", loc)] <- "2019"
	year[grepl("20$", loc)] <- "2020"
	variety <- trimws(as.character(r$Pedigree))
	is_check <- grepl("^Check", variety)

	d <- data.frame(
		location = location,
		country = "Nigeria",
		planting_date = year,
		rep = as.integer(r$Rep),
		block_id = as.character(r$BLOCK),
		variety_code = as.character(r$Entry),
		variety = variety,
		variety_pedigree = variety,
		variety_type = ifelse(is_check, "check", "inbred"),
		treatment = variety,
		yield = as.numeric(r$YIELD),
		ear_aspect = as.numeric(r$EASP),
		ears_per_plant_ = as.numeric(r$EPP),
		striga_count_8wap_transformed_ = as.numeric(r$CO1S),
		striga_count_10wap_transformed_ = as.numeric(r$CO2S),
		str_rat2 = as.numeric(r$STRIGA),
		crop = "maize",
		yield_part = "grain",
		stress = "striga",
		striga_trial = TRUE,
		striga_infected = TRUE,
		borer_trial = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d$trial_id <- paste(d$location, d$planting_date, sep = "_")
	d$record_id <- as.integer(seq_len(nrow(d)))
	mz_pd_long <- carobiner::get_function("mz_pd_long", path, group)
	long <- mz_pd_long(d)
	d[intersect(c("e_rot", "str_co1", "str_co2", "str_rat1", "str_rat2"), names(d))] <- NULL
	carobiner::write_files(path, meta, d, long = long)
}
