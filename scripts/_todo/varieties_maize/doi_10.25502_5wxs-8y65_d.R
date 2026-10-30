# R script for "carob"
# license: GPL (>=3)

## NOTES
# Extra-early maize hybrids, sites BG and MK, dataset title year 2014 (DTMA). Used optimal-hybrid-_dataset-mk-bg-2014.csv.
# The dictionary in this dataset expands BG = Bagauda and MK = Mokwa. Those names were used. The file has no Ikenne rows.
# The dictionary does not state the YIELD unit. Values (about 1700-10300) match kg/ha in the other IITA maize files in this series, so they were not rescaled.
# There is no pedigree string. variety is the line code crossed with the tester code (A-D). Line and Tester are the parents.
# RL_PERC and SLPERT are percents. HC, PLASP and EASP include half scores (1.5).
# Planting year is the year in the dataset title. It is not a column.

## ISSUES
# The dictionary title says "managed drought at Ikenne during the 2013 and 2014 dry seasons" and lists LFDTH, but the file is the Mokwa/Bagauda optimal set and has no LFDTH and no Ikenne. Treat the dictionary title as not describing this file.
# No coordinates in the file. CKAN coverage_x = 7.49046, coverage_y = 4.55213 does not match Mokwa or Bagauda. Not used. Georeference Bagauda and Mokwa.
# No fertilizer, irrigation, harvest date, or yield moisture.
# No paper DOI in the dataset metadata.
# write_files: missing longitude, latitude, geo_from_source, yield_moisture, harvest_date, N/P/K, irrigated. Unknown variable ears_per_plant_.

## NOT PROCESSED
# data_dictionary.csv, LFDTH: leaf-death score described in the dictionary but absent from the phenotype file.
# optimal-hybrid-_dataset-mk-bg-2014.csv, ID: row number.

## NEW VARIABLES
# ears_per_plant_: ears per plant (EPP). Unit: ears/plant.


carob_script <- function(path) {

"
Evaluation of extra-early maize hybrids under optimal conditions at Mokwa and Bagauda in 2014

Drought Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/5wxs-8y65/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Drought Tolerant Maize for Africa (DTMA)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height;anthesis_days;silking_days",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 72,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "optimal-hybrid-_dataset-mk-bg-2014.csv"]
	r <- read.csv(f, stringsAsFactors = FALSE)

	pct <- function(x) {
		out <- rep(NA_character_, length(x))
		i <- !is.na(x)
		out[i] <- paste0(as.character(x[i]), "%")
		out
	}

	# dictionary: Bagauda (BG), Mokwa (MK)
	loc <- ifelse(r$Site == "BG", "Bagauda", ifelse(r$Site == "MK", "Mokwa", trimws(r$Site)))
	female <- as.character(r$Line)
	male <- trimws(as.character(r$Tester))
	variety <- paste(female, male, sep = " x ")

	d <- data.frame(
		# year is the dataset title, not a column
		trial_id = paste(loc, "2014"),
		location = loc,
		planting_date = "2014",
		plot_id = as.character(r$Plot),
		rep = as.integer(r$Rep),
		block_id = as.character(r$Blk),
		variety = variety,
		variety_pedigree = variety,
		variety_code = as.character(r$Entry),
		variety_type = "extra-early hybrid",
		treatment = variety,
		female_parent = female,
		male_parent = male,
		stress = "none",
		yield = r$YIELD,
		silking_days = r$DYSK,
		anthesis_days = r$POLLEN,
		asi = r$ASI,
		plant_height = r$PLHT,
		ear_height = r$EHT,
		root_lodging = pct(r$RL_PERC),
		stalk_lodging = pct(r$SLPERT),
		husk = as.character(r$HC),
		aspect = r$PLASP,
		ear_aspect = r$EASP,
		ears_per_plant_ = r$EPP,
		crop = "maize",
		yield_part = "grain",
		country = "Nigeria",
		on_farm = FALSE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
