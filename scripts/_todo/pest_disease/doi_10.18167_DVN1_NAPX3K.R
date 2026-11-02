# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field file only. Cage sheets (oviposition and larval survival) are an insectary trial and are not used.
# Two field sowings (10 Nov 2020 and 14 Jan 2022). Seed treatment is cyantraniliprole (Fortenza) or none.
# Devis is the 1-9 plant score in Data_damage. Row spacing 0.75 m and within-row spacing 0.24 m, and drip irrigation, are on the Meta_data sheet.
# The dataset has no geographicCoverage and the field sheet has no place name.

## ISSUES
# A few plant rows are repeated with the same scores, so write_files reports duplicate records.
# write_files: missing on_farm, yield, yield_moisture, yield_part, country, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date.
# unknown variables: plant_number_, larva_present_, damage_present_, larval_instar_. invalid terms: pest_species: fall armyworm. duplicates: duplicate records detected.

## NOT PROCESSED
# Data cage Seye.xlsx: insectary oviposition and larval survival.
# Data_seedling: line-level stand counts and plants per ha. They are not the same grain as the plant damage scores.
# Data_damage %plant_attaque: 0 or 100, the same flag as Pres_degat.
# Data_damage Nb_plant_obs: 1 on every row.


carob_script <- function(path) {

"
Maize seed treatment for fall armyworm control

Data on field and semi-field trials of maize seed treament for the control of fall armyworm
"

	uri <- "doi:10.18167/DVN1/NAPX3K"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "pest_severity",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Data Field Seye.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data_damage")
	treated <- r$Trait_au_fortenza == "Traite au Cyantranilipole"

	d <- data.frame(
		trial_id = paste0("essai_", r$Num_essai),
		crop = "maize",
		treatment = trimws(r$Trait_au_fortenza),
		seed_treatment = ifelse(treated, "cyantraniliprole", "none"),
		insecticide_used = treated,
		rep = as.integer(r$Bloc),
		plot_id = trimws(as.character(r$Parcelle)),
		plant_number_ = as.integer(r$Num_plant),
		planting_date = as.character(as.Date(r$Date_semis)),
		date = as.character(as.Date(r$Date_obs)),
		pest_species = "fall armyworm",
		pest_severity = as.character(as.integer(r$Devis)),
		severity_scale = "1-9",
		larva_present_ = r$Presence_larv == 1,
		damage_present_ = r$Pres_degat == 1,
		larval_instar_ = as.integer(r$Stade_larv),
		row_spacing = 75,
		plant_spacing = 24,
		irrigated = TRUE,
		stress = "pests",
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
