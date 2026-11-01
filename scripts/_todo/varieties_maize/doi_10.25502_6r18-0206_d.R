# R script for "carob"
# license: GPL (>=3)

## NOTES
# Same 70 early PVA-QPM hybrids under low soil nitrogen at Ile-Ife and Mokwa, 2016 and 2017 (STMA). Separate dataset from the optimal trial.
# Used low-n-hybrid_datasets_ile-ife-mokwa_2016-2017.csv. YIELD is kg/ha in the column metadata (that metadata file is shared with the optimal dataset and lists columns that are not in this file).
# RESEARCH CONDITION is Low-N for every row, stored as stress = nitrogen deficiency. It does not vary, so it is not a treatment variable.
# Stalk lodging is SLPERC (percent). RL has no percent column; values run from 0 to 17 and are stored as recorded.
# Ear rot (EROT) is the count of rotten ears. No harvest moisture column.

## ISSUES
# No coordinates in the file. CKAN gives one unlabeled point (coverage_x = 7.49046, coverage_y = 4.55213) for two sites. Not assigned. Georeference Ile-Ife and Mokwa.
# No planting day, harvest date, fertilizer rate, or irrigation. Low-N management (how the low-N plots were fertilized) is not in the file.
# Yield moisture is not in this file. Do not copy 15% from other IITA trials.
# Column metadata lists PLST, RLPERC, MOIST, PHARV, FWT and a 1-9 husk scale, but those columns are absent here except HUSK. RL may be a count rather than the percent the dictionary describes.
# No paper DOI in the dataset metadata.
# write_files: missing longitude, latitude, geo_from_source, yield_moisture, harvest_date, N/P/K, irrigated.

## NOT PROCESSED
# low-n-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, SN: row number.
# low-n-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, ENV: location x year code.
# low-n-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, SL: stalk-lodging count; percent is SLPERC.
# low-n-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, RLSTAT, SLSTAT, EROTSTAT: transformed values.
# low-n-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, trailing blank column.

## NEW VARIABLES
# set_: mating-design set. Unit: none.
# ears_per_plant_: ears per plant (EPP). Unit: ears/plant.
# stay_green_: stay-green score, 1-9 (STGR). Unit: score.


carob_script <- function(path) {

"
Phenotypic data of 70 early PVA-QPM maize hybrids evaluated under low N conditions at Ile Ife and Mokwa, 2016 & 2017

Stress Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/6r18-0206/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Stress Tolerant Maize for Africa (STMA) Phase II",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height;anthesis_days;silking_days",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "low-n-hybrid_datasets_ile-ife-mokwa_2016-2017.csv"]
	r <- read.csv(f, check.names = FALSE)

	code <- function(x) {
		z <- trimws(as.character(x))
		z[is.na(z) | z %in% c("", "NA", "na")] <- NA_character_
		z
	}
	pct <- function(x) {
		out <- rep(NA_character_, length(x))
		i <- !is.na(x)
		out[i] <- paste0(as.character(x[i]), "%")
		out
	}
	as_count <- function(x) {
		out <- rep(NA_character_, length(x))
		i <- !is.na(x)
		out[i] <- as.character(x[i])
		out
	}

	loc <- trimws(r$LOCATION)
	year <- as.character(r$YEAR)
	variety <- trimws(r$PEDIGREE)
	d <- data.frame(
		trial_id = paste(loc, year, "low-N"),
		location = loc,
		planting_date = year,
		plot_id = as.character(r$PLOT),
		rep = as.integer(r$REP),
		block_id = as.character(r$BLOCK),
		variety = variety,
		variety_pedigree = variety,
		variety_code = as.character(r$ENTRY),
		variety_type = "early PVA-QPM hybrid",
		treatment = variety,
		female_parent = code(r$FEMALE),
		male_parent = code(r$MALE),
		set_ = code(r$SET),
		stress = "nitrogen deficiency",
		yield = r$YIELD,
		anthesis_days = r$POLLEN,
		silking_days = r$DYSK,
		asi = r$ASI,
		plant_height = r$PLHT,
		ear_height = r$EHT,
		# no RLPERC in this file; RL is 0-17
		root_lodging = as_count(r$RL),
		stalk_lodging = pct(r$SLPERC),
		husk = as.character(r$HUSK),
		aspect = r$PASP,
		ear_aspect = r$EASP,
		ears_per_plant_ = r$EPP,
		stay_green_ = r$STGR,
		crop = "maize",
		yield_part = "grain",
		country = "Nigeria",
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$record_id <- as.integer(seq_len(nrow(d)))

	keep <- !is.na(r$EROT)
	long <- data.frame(
		record_id = d$record_id[keep],
		disease = "ear rot",
		disease_severity = as.character(r$EROT[keep]),
		disease_incidence = NA_character_,
		pest_species = NA_character_,
		pest_severity = NA_character_,
		pest_incidence = NA_integer_,
		DAP = NA_integer_,
		order = NA_integer_
	)

	carobiner::write_files(path, meta, d, long = long)
}
