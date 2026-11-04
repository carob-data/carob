# R script for "carob"
# license: GPL (>=3)

## NOTES
# Fusarium head blight index and deoxynivalenol for a panel of wheat lines. The data dictionary places the 2023 and 2024 field tests, and the greenhouse tests, at El Batan, Mexico.
# Field and greenhouse scores are kept as separate trials. The index and the toxin are percent and ppm.
# FHB24 is stored with decimals, as in the file.

## ISSUES
# write_files: missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# write_files: unknown variables: fhb_index_, don_ppm_

## NEW VARIABLES
# fhb_index_: Fusarium head blight index, percent.
# don_ppm_: deoxynivalenol in grain, ppm.

## NOT PROCESSED
# Cross identification number and source identification number. The genotype id is stored in variety_code. The cross is the variety name and the selection history is variety_pedigree.


carob_script <- function(path) {

"
Fusarium head blight data for a panel of African wheat varieties

Field and greenhouse FHB resistance evaluation for a panel of African wheat varieties, with deoxynivalenol (DON) toxin data provided.
"

	uri <- "hdl:11529/10549125"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

		meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "fhb_index_;don_ppm_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "African_panel.csv"]
	r <- read.csv(f)

	one <- function(index, don, trial) {
		data.frame(
			country = "Mexico",
			location = "El Batan",
			trial_id = trial,
			crop = "wheat",
			variety = r$Cross,
			variety_code = as.character(r$GID),
			variety_pedigree = r$Sel_Hist,
			treatment = r$Cross,
			disease = "fusarium scab spike",
			stress = "disease",
			fhb_index_ = as.numeric(index),
			don_ppm_ = as.numeric(don),
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	d <- rbind(
		one(r$FHB23, r$DON23, "field 2023"),
		one(r$FHB24, rep(NA_real_, nrow(r)), "field 2024"),
		one(r$GH_FHB23, rep(NA, nrow(r)), "greenhouse 2023"),
		one(r$GH_FHB24, r$GH_DON24, "greenhouse 2024")
	)
	keep <- !is.na(d$fhb_index_) | !is.na(d$don_ppm_)
	d <- d[keep, ]
	d$variety_pedigree[!nzchar(as.character(d$variety_pedigree))] <- NA_character_

	carobiner::write_files(path, meta, d)
}
