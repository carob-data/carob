# R script for "carob"
# license: GPL (>=3)

## NOTES
# 300 diallel crosses plus 6 checks, evaluated at Abuja and Mokwa under Striga-infested and optimum conditions (two files).
# Grain yield is kg/ha (GYD in the Striga file, GY in the optimum file).
# Striga file: SDS is damage at 10 weeks after planting and ESP is emerged Striga plants at 10 weeks. Passed to mz_pd_long as the 10-week record.
# Optimum file has plant aspect and husk cover; the Striga file does not. Column Plot_nunmber in the optimum file is the plot number (header typo).
# Each location x management (striga or optimum) is a trial. Variety is the treatment within a trial.
# Checks are not flagged in the files, so every entry is recorded as a hybrid.

## ISSUES
# No trial year, planting date, harvest date, fertilizer, irrigation, or grain moisture.
# No coordinates. Metadata coverage point was not used. Georeference Abuja and Mokwa, Nigeria.
# Dictionary says both diallel indices i and j are the male parent. The pedigree string is the cross; i and j were not mapped.
# The 6 checks are not identified in the data.
# Optimum-file ear height includes values below 10 cm. Left as recorded.
# Striga yield includes zeros. Left as recorded.
# Expect missing longitude, latitude, geo_from_source, planting_date, harvest_date, yield_moisture, irrigated, N/P/K, and an unknown-variable warning for ears_per_plant_.

## NOT PROCESSED
# phenotypic-data-of-the-300-diallel-cross-and-6-checks.csv, ID: row number.
# phenotypic-data-of-the-300-diallel-cross-and-6-checks.csv, i, j: diallel parent indices. Dictionary text is inconsistent (both described as male). Pedigree stores the cross.
# phenotypic-data-optimum-of-the-300-diallel-cross-and-6-checks.csv, ID, i, j: same as above.
# data_dictionary.csv: definitions used to map columns. It describes the Striga file only (no optimum-file plant aspect or husk).

## NEW VARIABLES
# ears_per_plant_: ears harvested divided by plants in the plot (dimensionless).


carob_script <- function(path) {

"
Phenotypic data of the 300 diallel crosses and 6 checks evaluated in Abuja and Mokwa under Striga-infested and optimum environments

Stress Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/0zkd-zf95/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Accelerated Genetic Gain (AGG)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height;ear_height;ear_aspect;asi",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 68,
		carob_effort = 0.5,
		notes = NA
	)

	f_str <- ff[basename(ff) == "phenotypic-data-of-the-300-diallel-cross-and-6-checks.csv"]
	f_opt <- ff[basename(ff) == "phenotypic-data-optimum-of-the-300-diallel-cross-and-6-checks.csv"]
	r1 <- read.csv(f_str)
	r2 <- read.csv(f_opt)

	v1 <- trimws(as.character(r1$Pedigree))
	d1 <- data.frame(
		location = trimws(as.character(r1$Location)),
		country = "Nigeria",
		plot_id = as.character(r1$Plot_number),
		rep = as.integer(r1$Replication),
		block_id = as.character(r1$Block),
		variety_code = as.character(r1$Entry_number),
		variety = v1,
		variety_pedigree = v1,
		variety_type = "hybrid",
		treatment = v1,
		yield = as.numeric(r1$GYD),
		plant_height = as.numeric(r1$PHT),
		ear_height = as.numeric(r1$EHT),
		ear_aspect = as.numeric(r1$EASP),
		asi = as.numeric(r1$ASI),
		ears_per_plant_ = as.numeric(r1$EPP),
		str_rat2 = as.numeric(r1$SDS),
		str_co2 = as.numeric(r1$ESP),
		stress = "striga",
		striga_trial = TRUE,
		striga_infected = TRUE,
		management = "striga"
	)

	v2 <- trimws(as.character(r2$Pedigree))
	d2 <- data.frame(
		location = trimws(as.character(r2$Location)),
		country = "Nigeria",
		plot_id = as.character(r2$Plot_nunmber),
		rep = as.integer(r2$Replication),
		block_id = as.character(r2$Block),
		variety_code = as.character(r2$Entry_number),
		variety = v2,
		variety_pedigree = v2,
		variety_type = "hybrid",
		treatment = v2,
		yield = as.numeric(r2$GY),
		plant_height = as.numeric(r2$PHT),
		ear_height = as.numeric(r2$EHT),
		aspect = as.numeric(r2$PASP),
		ear_aspect = as.numeric(r2$EASP),
		husk = trimws(as.character(r2$HC)),
		asi = as.numeric(r2$ASI),
		ears_per_plant_ = as.numeric(r2$EPP),
		stress = "none",
		striga_trial = FALSE,
		striga_infected = FALSE,
		management = "optimum"
	)

	d <- carobiner::bindr(d1, d2)
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$borer_trial <- FALSE
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$trial_id <- paste(d$location, d$management, sep = "_")
	d$management <- NULL
	d$record_id <- as.integer(seq_len(nrow(d)))
	mz_pd_long <- carobiner::get_function("mz_pd_long", path, group)
	long <- mz_pd_long(d)
	d[intersect(c("e_rot", "str_co1", "str_co2", "str_rat1", "str_rat2"), names(d))] <- NULL
	carobiner::write_files(path, meta, d, long = long)
}
