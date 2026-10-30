# R script for "carob"
# license: GPL (>=3)

## NOTES
# Striga-free companion to the 2017-2019 early-hybrid trial (same pedigrees and trial codes BA17232, BA18232, BA19232).
# YIELD is grain yield in kg/ha. MOIST is grain moisture at harvest (%).
# The file repeats the header YEAR. The first YEAR is the trial year. The second (YEAR.1) is the hybrid development year (for example 2008), matching Year_Dev in the infested companion and YEAR_DEV in the dictionary.
# ABUN mapped to Abuja and MOOPT to Mokwa. INAUN mapped to Ina, country left unset.
# Lodging taken from RL_PERC and SL_PERC. This file has plant aspect (PLASP); the infested companion does not.

## ISSUES
# No coordinates. Metadata coverage_x/coverage_y is one unlabelled point and was not used. Georeference Abuja, Mokwa, and Ina.
# INAUN was read as Ina. Other IITA maize files use INA for Baba Ina (Nigeria) and Ina is also a Benin site. Confirm the site and country.
# No fertilizer rates, irrigation flag, or day of planting/harvest.
# Plot area unknown, so plant stand, harvest counts, and field weight were not converted per ha.
# The dictionary lists Striga rating and count columns, but they are not in this file.
# Some scripts would need to confirm that yield is not already adjusted to a standard moisture. Moisture is stored separately as recorded, including any zeros.
# Expect missing longitude, latitude, geo_from_source, irrigated, N/P/K, harvest_date, NA country for Ina, and unknown-variable warnings.

## NOT PROCESSED
# striga-free_dataset.csv, ID: row number.
# striga-free_dataset.csv, ENV: environment index; trial_id is location plus year.
# striga-free_dataset.csv, PLST: plants per plot.
# striga-free_dataset.csv, RL, SL: integer lodging. Percentage columns were used.
# striga-free_dataset.csv, RL_TRANS, SL_TRANS: transformed lodging.
# striga-free_dataset.csv, PL_HARV, E_HARV: plants and ears harvested per plot.
# striga-free_dataset.csv, FWT: field weight of ears per plot.
# data_dictionary.csv: definitions. Striga columns named there are absent from the data file.

## NEW VARIABLES
# ears_per_plant_: ears harvested divided by plants in the plot (dimensionless).
# era_: breeding-period code of the hybrid (integer 1-3).
# year_developed_: year the hybrid was developed (second YEAR column).
# year_rating_: code assigned to the year of development.


carob_script <- function(path) {

"
Evaluation of early maize hybrids under Striga non-infested conditions in West Africa between 2017 and 2019

Stress Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/3022-7b68"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Stress Tolerant Maize for Africa (STMA)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;anthesis_days;silking_days;asi;plant_height;ear_height;aspect;ear_aspect",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 72,
		carob_effort = 0.5,
		notes = NA
	)

	f <- ff[basename(ff) == "striga-free_dataset.csv"]
	r <- read.csv(f)

	loc <- trimws(as.character(r$LOC))
	location <- loc
	location[loc == "ABUN"] <- "Abuja"
	location[loc == "MOOPT"] <- "Mokwa"
	location[loc == "INAUN"] <- "Ina"
	country <- ifelse(location %in% c("Abuja", "Mokwa"), "Nigeria", NA_character_)
	variety <- trimws(as.character(r$PEDIGREE))

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
		year_developed_ = as.integer(r$YEAR.1),
		year_rating_ = as.integer(r$YR_RATING),
		yield = as.numeric(r$YIELD),
		anthesis_days = as.numeric(r$POLLEN),
		silking_days = as.numeric(r$DYSK),
		asi = as.numeric(r$ASI),
		plant_height = as.numeric(r$PLHT),
		ear_height = as.numeric(r$EHT),
		root_lodging = ifelse(is.na(r$RL_PERC), NA_character_, paste0(r$RL_PERC, "%")),
		stalk_lodging = ifelse(is.na(r$SL_PERC), NA_character_, paste0(r$SL_PERC, "%")),
		husk = trimws(as.character(r$HUSK)),
		aspect = as.numeric(r$PLASP),
		ear_aspect = as.numeric(r$EASP),
		e_rot = as.numeric(r$E_ROT),
		yield_moisture = as.numeric(r$MOIST),
		ears_per_plant_ = as.numeric(r$EPP),
		crop = "maize",
		yield_part = "grain",
		stress = "none",
		striga_trial = FALSE,
		striga_infected = FALSE,
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
