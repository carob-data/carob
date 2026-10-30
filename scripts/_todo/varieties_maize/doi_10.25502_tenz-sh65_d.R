# R script for "carob"
# license: GPL (>=3)

## NOTES
# 70 early provitamin A quality-protein maize hybrids plus checks, optimal conditions, Ile-Ife and Mokwa, 2016 and 2017 (STMA).
# Used optimal-hybrid_datasets_ile-ife-mokwa_2016-2017.csv. Column metadata gives YIELD in kg/ha and MOIST as grain moisture at harvest (%).
# Root and stalk lodging stored from RLPERC and SLPERC (percent). RL and SL are smaller integers and do not match the percent columns.
# Ear rot is a count of rotten ears (EROT) with a percent (EROTPER). Transformed columns were not stored.
# Parents are numeric codes, not names. Rows with blank SET and blank parents are the named checks.
# Trial is one location x year. Plot numbers repeat across trials.

## ISSUES
# No coordinates in the file. CKAN coverage_x = 7.49046 and coverage_y = 4.55213 are one unlabeled point for two sites (Ile-Ife and Mokwa). Not assigned.
# No planting day, harvest date, fertilizer product, or N/P/K rate. Irrigation is not stated.
# Five MOIST values are 0. Three HUSK values are 0 (dictionary scale is 1-9). Left as recorded.
# The dictionary does not say whether YIELD was already adjusted to a standard moisture or is at harvest moisture. MOIST was mapped to yield_moisture as the recorded harvest moisture.
# No paper DOI in the dataset metadata.
# write_files: missing longitude, latitude, geo_from_source, harvest_date, N/P/K, irrigated. Unknown variables are the underscore fields. A few yield_moisture values are 0.

## NOT PROCESSED
# optimal-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, SN: row number.
# optimal-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, ENV: location x year code, same split as location and year.
# optimal-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, RL: lodging count; percent is in RLPERC.
# optimal-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, SL: lodging count; percent is in SLPERC.
# optimal-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, RLSTAT, SLSTAT, EROTSTAT: transformed lodging and ear-rot values.
# optimal-hybrid_datasets_ile-ife-mokwa_2016-2017.csv, FWT: field weight of ears per plot; unit not stated, plot area not stated, so not converted to kg/ha.

## NEW VARIABLES
# set_: mating-design set (1-6). Unit: none.
# plant_stand_: plants per plot (PLST). Unit: plants/plot.
# plants_harvested_: plants harvested per plot (PHARV). Unit: plants/plot.
# ears_harvested_: ears harvested per plot (EHARVACR). Unit: ears/plot.
# ears_per_plant_: ears per plant (EPP). Unit: ears/plant.


carob_script <- function(path) {

"
Phenotypic data of 70 early PVA-QPM maize hybrids evaluated under optimal conditions at Ile Ife and Mokwa, 2016 & 2017

Stress Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/tenz-sh65/d"
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
		carob_completion = 74,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "optimal-hybrid_datasets_ile-ife-mokwa_2016-2017.csv"]
	r <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)

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

	loc <- trimws(r$LOCATION)
	year <- as.character(r$YEAR)
	variety <- trimws(r$PEDIGREE)
	d <- data.frame(
		trial_id = paste(loc, year),
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
		stress = "none",
		yield = r$YIELD,
		yield_moisture = r$MOIST,
		anthesis_days = r$POLLEN,
		silking_days = r$DYSK,
		asi = r$ASI,
		plant_height = r$PLHT,
		ear_height = r$EHT,
		root_lodging = pct(r$RLPERC),
		stalk_lodging = pct(r$SLPERC),
		husk = as.character(r$HUSK),
		aspect = r$PASP,
		ear_aspect = r$EASP,
		plant_stand_ = r$PLST,
		plants_harvested_ = r$PHARV,
		ears_harvested_ = r$EHARVACR,
		ears_per_plant_ = r$EPP,
		crop = "maize",
		yield_part = "grain",
		country = "Nigeria",
		# researcher-managed trial at the sites named in the title
		on_farm = FALSE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)
	d$record_id <- as.integer(seq_len(nrow(d)))

	keep <- !is.na(r$EROT) | !is.na(r$EROTPER)
	inc <- rep(NA_character_, nrow(r))
	j <- !is.na(r$EROTPER)
	inc[j] <- paste0(as.character(r$EROTPER[j]), "%")
	long <- data.frame(
		record_id = d$record_id[keep],
		disease = "ear rot",
		disease_severity = as.character(r$EROT[keep]),
		disease_incidence = inc[keep],
		pest_species = NA_character_,
		pest_severity = NA_character_,
		pest_incidence = NA_integer_,
		DAP = NA_integer_,
		order = NA_integer_,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d, long = long)
}
