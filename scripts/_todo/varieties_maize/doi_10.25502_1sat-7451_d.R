# R script for "carob"
# license: GPL (>=3)

## NOTES
# Striga-infested early-hybrid trial, West Africa, 2017-2019 (Badu-Apraku / STMA). One row per plot.
# YIELD is grain yield in kg/ha as recorded. MOIST is grain moisture at harvest (%), used as yield_moisture.
# AB_STR mapped to Abuja and MO_STR/MOSTR to Mokwa. IA_STR and NY_STR are not expanded; they are stored as Ia_str and Ny_str so the location is not left in all capitals.
# Root and stalk lodging taken from the percentage columns (RL_PERC, SLPERCT). The companion integer columns are smaller and were not used as percentages.
# Striga damage ratings and emerged counts at 8 and 10 weeks after planting are passed to mz_pd_long (56 and 70 days). Square-root/log transforms (STRC0_*) were not used.
# Year of development, breeding-period code (ERA) and year-rating kept as extra columns.

## ISSUES
# No coordinates in the data. Metadata coverage_x/coverage_y is one unlabelled point and was not used. Georeference Abuja, Mokwa, IA_STR, NY_STR.
# IA_STR and NY_STR were not expanded (NY_STR may be Nyankpala, Ghana; IA_STR may be Ina). Metadata country is only Nigeria, which may be incomplete.
# No fertilizer rates, irrigation flag, or day of planting/harvest. Planting year only.
# Plot area is not given, so plant stand, plants and ears harvested, and field weight (per plot) were not converted to per-ha values.
# One ear-aspect value is 16 (scale is 1-9). Left as recorded.
# Some grain-moisture values are 0 or missing. Left as recorded; do not treat 0 as a filled-in moisture.
# Dictionary lists PLASP, but that column is not in the data file.
# Expect missing longitude, latitude, geo_from_source, irrigated, N/P/K fertilizer, harvest_date, and unknown-variable warnings for the extra columns.

## NOT PROCESSED
# striga-infested_dataset.csv, ID: row number.
# striga-infested_dataset.csv, ENV: environment index of location x year; trial_id is location plus year.
# striga-infested_dataset.csv, PLST: plants per plot. Plot area unknown.
# striga-infested_dataset.csv, RL: integer root lodging. RL_PERC is the percentage.
# striga-infested_dataset.csv, RL_TRANS, SL_TRANS: transformed lodging.
# striga-infested_dataset.csv, SL: integer stalk lodging. SLPERCT is the percentage.
# striga-infested_dataset.csv, PL_HARV, E_HARV: plants and ears harvested per plot.
# striga-infested_dataset.csv, FWT: field weight of ears per plot (about 0-6.6; unit not stated, likely kg).
# striga-infested_dataset.csv, STRC0_1, STRC0_2: transformed Striga counts. CO1 and CO2 are the counts.
# striga_infested_dictionary.csv: column definitions, already used to map names and units.

## NEW VARIABLES
# ears_per_plant_: ears harvested divided by plants in the plot (dimensionless).
# era_: breeding-period code of the hybrid (integer 1-3).
# year_developed_: year the hybrid was developed.
# year_rating_: code assigned to the year of development (integer 1-8).


carob_script <- function(path) {

"
Evaluation of early maize hybrids under Striga infested conditions in West Africa between 2017 and 2019

Stress Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/1sat-7451/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Stress Tolerant Maize for Africa (STMA)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;anthesis_days;silking_days;asi;plant_height;ear_height;ear_aspect",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 72,
		carob_effort = 0.6,
		notes = NA
	)

	f <- ff[basename(ff) == "striga-infested_dataset.csv"]
	r <- read.csv(f)

	loc <- trimws(as.character(r$LOC))
	location <- loc
	location[loc == "AB_STR"] <- "Abuja"
	location[loc %in% c("MO_STR", "MOSTR")] <- "Mokwa"
	up <- location == toupper(location)
	location[up] <- carobiner::fix_name(location[up], "title")
	country <- ifelse(location %in% c("Abuja", "Mokwa"), "Nigeria", NA_character_)
	variety <- trimws(as.character(r$Pedigree))

	d <- data.frame(
		trial_name = trimws(as.character(r$TRIAL)),
		location = location,
		country = country,
		planting_date = as.character(r$YEAR),
		plot_id = as.character(r$PLOT),
		rep = as.integer(r$REP),
		block_id = as.character(r$BLOCK),
		variety_code = as.character(r$ENTRY),
		variety = variety,
		variety_pedigree = variety,
		variety_type = "hybrid",
		treatment = variety,
		era_ = as.integer(r$ERA),
		year_developed_ = as.integer(r$Year_Dev),
		year_rating_ = as.integer(r$Yr_rating),
		yield = as.numeric(r$YIELD),
		anthesis_days = as.numeric(r$POLLEN),
		silking_days = as.numeric(r$DYSK),
		asi = as.numeric(r$ASI),
		plant_height = as.numeric(r$PLHT),
		ear_height = as.numeric(r$EHT),
		root_lodging = ifelse(is.na(r$RL_PERC), NA_character_, paste0(r$RL_PERC, "%")),
		stalk_lodging = ifelse(is.na(r$SLPERCT), NA_character_, paste0(r$SLPERCT, "%")),
		husk = trimws(as.character(r$HUSK)),
		ear_aspect = as.numeric(r$EASP),
		e_rot = as.numeric(r$E_ROT),
		yield_moisture = as.numeric(r$MOIST),
		ears_per_plant_ = as.numeric(r$EPP),
		str_rat1 = as.numeric(r$RAT1),
		str_rat2 = as.numeric(r$RAT2),
		str_co1 = as.numeric(r$CO1),
		str_co2 = as.numeric(r$CO2),
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
