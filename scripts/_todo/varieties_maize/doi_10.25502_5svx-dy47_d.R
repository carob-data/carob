# R script for "carob"
# license: GPL (>=3)

## NOTES
# Early white QPM hybrids under three managements, Nigeria, 2019-2021 (STMA). Files: combined-heat-and-drought.csv (KDW, 2020-2021), low-nitrogen.csv (MK, 2019-2020), optimal.csv (ENV KDWOP, MKOP1, MKOP2).
# The dictionary says YIELD is grain yield but does not give the unit. Values are in the same range as the IITA files that are labeled kg/ha, so they were left as kg/ha (not multiplied).
# MOIST is harvest grain moisture (%) and was mapped to yield_moisture. The dictionary does not say the yield was adjusted to 15%.
# Location in the optimal file is the ENV code (KDWOP, MKOP1, MKOP2). LOC in that file is 1, 2, 3 and is not a place. KDW and MK were kept as recorded; the dictionary does not expand them.
# Root and stalk lodging are the percent columns (RL_PERC, SLPERCT). TB and LF are tassel blast and leaf firing (dictionary names TABLAST and LEFFNG).
# Ear rot values exceed 9, so they were stored as a count-like severity, not as ear_aspect.

## ISSUES
# Confirm KDW and MK (Mokwa is the usual MK code in this series; KDW is not defined here) and georeference them. CKAN coverage is two unlabeled pairs, coverage_x 7.49046 and 9.3, coverage_y 4.55213 and 5.066667. Not assigned, because they are not tied to KDW vs MK in the file.
# No planting day, harvest date, fertilizer rate, or irrigation. Managed heat/drought and low-N fertilizer protocols are not in the files.
# One plant height is 1333 cm (KDW 2020, plot 1059, TZEQI 165 x TZEQI 140). One optimal yield is 50103.81 kg/ha (KDWOP 2020, plot 1015, TZEQI 241 x TZEQI 219, FWT 15). Left as recorded.
# GWT and FWT have no unit. Plot area is not given.
# No paper DOI in the dataset metadata.
# write_files: missing longitude, latitude, geo_from_source, harvest_date, N/P/K, irrigated; out of bounds plant_height 1333; high crop yield 50103.8; NA yield on 2 heat-drought rows. Unknown underscore variables.

## NOT PROCESSED
# All three phenotype files, RL and SL: lodging counts; the percent columns were used.
# All three phenotype files, RLSTAT and SLSTAT: transformed lodging.
# combined-heat-and-drought.csv and low-nitrogen.csv, GWT: grain weight, unit not stated.
# optimal.csv, FWT: field weight of ears, unit not stated.
# optimal.csv, LOC: values 1, 2, 3, paired 1:1 with ENV but not a location name.
# combined-heat-and-drought.csv and low-nitrogen.csv, ENV: numeric environment index, redundant with location and year.

## NEW VARIABLES
# set_: mating-design set (A-F). Unit: none.
# plant_stand_: plants per plot (PLST). Missing on the optimal file. Unit: plants/plot.
# plants_harvested_: plants harvested per plot (PHARV). Unit: plants/plot.
# ears_harvested_: ears harvested per plot (E_HARV). Unit: ears/plot.
# ears_per_plant_: ears per plant (EPP). Unit: ears/plant.
# stay_green_: stay-green score (STGR). Missing on the optimal file. Unit: score.
# tassel_blast_: tassel blast score (TB). Heat-drought file only. Unit: score.
# leaf_firing_: leaf firing score (LF). Heat-drought file only. Unit: score.


carob_script <- function(path) {

"
Phenotypic data generated from QPM Hybrids evaluation under Low Soil Nitrogen and Combined heat and drought

Determine the gene action conditioning grain yield and other traits of the early maturing white QPM inbred lines under CDHS, low-N, and optimal environments and (ii) group the inbred lines into heterotic groups (iii) assess the performance of the inbred lines in hybrid combinations and identify high yielding and stable QPM hybrids across test environments.
"

	uri <- "doi:10.25502/5svx-dy47/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Stress Tolerant Maize for Africa (STMA) Phase II",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "yield;plant_height;anthesis_days;silking_days",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 72,
		carob_effort = 0.8
	)

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
	num_or_na <- function(r, nm) {
		if (nm %in% names(r)) r[[nm]] else rep(NA_real_, nrow(r))
	}

	read_piece <- function(fname, stress, stress_id, from_env) {
		f <- ff[basename(ff) == fname]
		r <- read.csv(f)
		if (from_env) {
			env <- trimws(r$ENV)
			loc <- ifelse(env == "KDWOP", "KDW", ifelse(substr(env, 1, 2) == "MK", "MK", env))
		} else {
			loc <- trimws(r$LOC)
		}
		entry <- if ("Entry" %in% names(r)) r$Entry else r$ENTRY
		variety <- trimws(r$PEDIGREE)
		d <- data.frame(
			trial_id = paste(loc, r$YEAR, stress_id),
			location = loc,
			planting_date = as.character(r$YEAR),
			plot_id = as.character(r$Plot_no),
			rep = as.integer(r$Rep),
			block_id = as.character(r$Block),
			variety = variety,
			variety_pedigree = variety,
			variety_code = as.character(entry),
			variety_type = "early-maturing QPM hybrid",
			treatment = variety,
			female_parent = code(r$FEMALE),
			male_parent = code(r$MALE),
			set_ = code(r$SET),
			stress = stress,
			yield = r$YIELD,
			yield_moisture = r$MOIST,
			anthesis_days = r$POLLEN,
			silking_days = r$DYSK,
			asi = r$ASI,
			plant_height = r$PLHT,
			ear_height = r$EHT,
			root_lodging = pct(r$RL_PERC),
			stalk_lodging = pct(r$SLPERCT),
			aspect = r$PLASP,
			ear_aspect = r$EASP,
			plant_stand_ = num_or_na(r, "PLST"),
			plants_harvested_ = r$PHARV,
			ears_harvested_ = r$E_HARV,
			ears_per_plant_ = r$EPP,
			stay_green_ = num_or_na(r, "STGR"),
			tassel_blast_ = num_or_na(r, "TB"),
			leaf_firing_ = num_or_na(r, "LF"),
			crop = "maize",
			yield_part = "grain",
			country = "Nigeria",
			on_farm = FALSE,
			is_survey = FALSE,
			e_rot = r$E_ROT
		)
		d
	}

	d1 <- read_piece("combined-heat-and-drought.csv", "heat;drought", "heat-drought", FALSE)
	d2 <- read_piece("low-nitrogen.csv", "nitrogen deficiency", "low-N", FALSE)
	d3 <- read_piece("optimal.csv", "none", "optimal", TRUE)
	d <- rbind(d1, d2, d3)
	d$record_id <- as.integer(seq_len(nrow(d)))

	keep <- !is.na(d$e_rot)
	long <- data.frame(
		record_id = d$record_id[keep],
		disease = "ear rot",
		disease_severity = as.character(d$e_rot[keep]),
		disease_incidence = NA_character_,
		pest_species = NA_character_,
		pest_severity = NA_character_,
		pest_incidence = NA_integer_,
		DAP = NA_integer_,
		order = NA_integer_
	)
	d$e_rot <- NULL

	carobiner::write_files(path, meta, d, long = long)
}
