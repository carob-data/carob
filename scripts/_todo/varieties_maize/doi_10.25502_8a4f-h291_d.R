# R script for "carob"
# license: GPL (>=3)

## NOTES
# Early-maturing hybrid trials at Abuja and Mokwa, 2010-2021, under Striga-infested (STRINF) and non-infested (STRNON) management. Each location x year x management is a trial. Variety is the treatment.
# The dictionary says YIELD is shelled-grain yield adjusted with an 80% shelling percentage and corrected to 15% moisture. yield_moisture is therefore 15 for every row. Shelling percentage is that computational factor, not a measured value, so it is not stored as shelling_percentage.
# Variety is NAME when present, otherwise PEDIGREE. variety_pedigree is always PEDIGREE. CHECK == 1 marks checks; other rows are recorded as hybrids.
# Striga counts (C01_STAT, C02_STAT) and damage ratings (STRRAT1, STRRAT2) at 8 and 10 weeks go to the long table. They are missing on the non-infested rows, as expected.

## ISSUES
# No coordinates. Metadata coverage point (about 9.6 E, 8.0 N) is not a trial site and was not used. Georeference Abuja and Mokwa.
# No fertilizer rates, irrigation flag, or day of planting/harvest. Trial year only.
# Five rows have no yield. Kept.
# NAME is missing on 726 rows; PEDIGREE was used as the variety name there.
# The 15% moisture and 80% shelling figures are dictionary statements about how yield was computed, not plot-level measurements.
# Expect missing longitude, latitude, geo_from_source, irrigated, N/P/K, harvest_date, NA yield, and unknown-variable warnings.

## NOT PROCESSED
# gg_-strinfnoninf_2010-2021.csv, ID: row number.
# data_dictionary.csv: definitions. The dictionary column GENOTYPE is the CHECK column in the data file.

## NEW VARIABLES
# ears_per_plant_: ears harvested divided by plants harvested (dimensionless).
# testing_year_: year the genotype was first tested.


carob_script <- function(path) {

"
REPEATABILITY AND GENETIC ADVANCEMENT IN EARLY MATURING MAIZE HYBRID TRIALS CONDUCTED UNDER STRIGA-INFESTED AND NON-INFESTED CONDITIONS

Stress Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/8a4f-h291/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Drought Tolerant Maize for Africa (DTMA); Accelerated Genetic Gains (AGG)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;anthesis_days;silking_days;asi;ear_aspect",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 74,
		carob_effort = 0.5,
		notes = NA
	)

	f <- ff[basename(ff) == "gg_-strinfnoninf_2010-2021.csv"]
	r <- read.csv(f)

	name <- trimws(as.character(r$NAME))
	name[name == ""] <- NA
	ped <- trimws(as.character(r$PEDIGREE))
	variety <- name
	variety[is.na(variety)] <- ped[is.na(variety)]
	infested <- r$Condition == "STRINF"
	loc <- trimws(as.character(r$LOC))
	location <- loc
	location[loc == "ABUJA"] <- "Abuja"
	location[loc == "MOKWA"] <- "Mokwa"

	d <- data.frame(
		location = location,
		country = "Nigeria",
		planting_date = as.character(r$TRIAL_YEAR),
		plot_id = as.character(r$PLOT_NO),
		rep = as.integer(r$REP),
		block_id = as.character(r$BLK),
		variety_code = as.character(r$ENTRY),
		variety = variety,
		variety_pedigree = ped,
		variety_type = ifelse(!is.na(r$CHECK) & r$CHECK == 1, "check", "hybrid"),
		treatment = variety,
		testing_year_ = as.integer(r$TESTING_YEAR),
		yield = as.numeric(r$YIELD),
		# dictionary: yield corrected to 15% moisture
		yield_moisture = 15,
		anthesis_days = as.numeric(r$ANTHESIS),
		silking_days = as.numeric(r$DYSK50),
		asi = as.numeric(r$ASI),
		ears_per_plant_ = as.numeric(r$EPP),
		ear_aspect = as.numeric(r$EASP),
		str_co1 = as.numeric(r$C01_STAT),
		str_co2 = as.numeric(r$C02_STAT),
		str_rat1 = as.numeric(r$STRRAT1),
		str_rat2 = as.numeric(r$STRRAT2),
		stress = ifelse(infested, "striga", "none"),
		striga_trial = TRUE,
		striga_infected = infested,
		crop = "maize",
		yield_part = "grain",
		borer_trial = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d$trial_id <- paste(d$location, d$planting_date, ifelse(infested, "striga", "noninfested"), sep = "_")
	d$record_id <- as.integer(seq_len(nrow(d)))
	mz_pd_long <- carobiner::get_function("mz_pd_long", path, group)
	long <- mz_pd_long(d)
	d[intersect(c("e_rot", "str_co1", "str_co2", "str_rat1", "str_rat2"), names(d))] <- NULL
	carobiner::write_files(path, meta, d, long = long)
}
