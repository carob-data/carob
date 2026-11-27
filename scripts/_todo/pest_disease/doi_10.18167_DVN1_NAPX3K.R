# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Neither the dataset files nor the publicly accessible abstract of Seye et al.
#    (2023, doi:10.1111/jen.13114) gives the specific town or station coordinates
#    within Senegal (plot codes are D7C7D and JAUNE 3). Country is set to Senegal
#    from the publication; coordinates and on_farm remain missing.
# 2. Trial 2 (2022) has only one damage assessment week (Semaine 2, 2022-02-05)
#    and one seedling-count week (Semaine 1), whereas Trial 1 (2020) has four
#    damage weeks and multiple seedling-count weeks.

## NEW
# plant_number_: individual sampled plant number within plot and date
# row_number_: sampled row number (10 m line) for seedling stand count
# larva_present_: logical indicator of fall armyworm larva presence on plant
# damage_present_: logical indicator of fall armyworm foliar damage on plant
# larval_instar_: observed fall armyworm larval developmental stage (2-4)

## MISSING
# latitude, longitude, geo_from_source, on_farm, yield, yield_part,
# yield_moisture, harvest_date, N_fertilizer, P_fertilizer, K_fertilizer.

## NOTES
# Two field trials in Senegal (2020 and 2022) evaluating cyantraniliprole
# (Fortenza) maize seed treatment vs untreated control against Spodoptera
# frugiperda (Seye et al. 2023). Wide table d holds the 16 experimental plots;
# long holds all 960 plant-level damage scores (1-9 Davis scale) and 120
# row-level seedling density counts linked via record_id.

## NOT PROCESSED
# Data cage Seye.xlsx: semi-field cage oviposition and tunnel larval survival
# experiments (non-field insectary/cage trials).
# Data_damage: %plant_attaque (0/100 duplicate of Pres_degat), Nb_plant_obs (always 1).

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
		publication = "doi:10.1111/jen.13114",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "pest_severity;plant_density",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Data Field Seye.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Data_damage")
	r2 <- carobiner::read.excel(f, sheet = "Data_seedling", skip = 4)

	## Plot-level (wide) records: 2 trials x 4 blocks x 2 seed treatments = 16 plots
	plots <- unique(r1[, c("Num_essai", "Bloc", "Parcelle", "Trait_au_fortenza", "Date_semis", "Date_lev")])
	plots <- plots[order(plots$Num_essai, plots$Bloc, plots$Trait_au_fortenza), ]
	rownames(plots) <- NULL
	treated <- trimws(plots$Trait_au_fortenza) == "Traite au Cyantranilipole"

	d <- data.frame(
		record_id = seq_len(nrow(plots)),
		trial_id = paste0("essai_", as.integer(plots$Num_essai)),
		country = "Senegal",
		crop = "maize",
		treatment = trimws(plots$Trait_au_fortenza),
		seed_treatment = ifelse(treated, "cyantraniliprole", "none"),
		insecticide_used = treated,
		insecticide_product = ifelse(treated, "cyantraniliprole", "none"),
		rep = as.integer(plots$Bloc),
		plot_id = paste0(trimws(plots$Parcelle), "_B", as.integer(plots$Bloc), "_", ifelse(treated, "T", "NT")),
		plot_area = 10000, # 1 ha plots per Meta_data
		planting_date = as.character(as.Date(plots$Date_semis)),
		emergence_date = as.character(as.Date(plots$Date_lev)),
		row_spacing = 75,
		plant_spacing = 24,
		irrigated = TRUE,
		irrigation_method = "drip",
		stress = "pests",
		is_survey = FALSE
	)

	## Key to link observation-level sheets to wide plot records
	plot_key <- paste(plots$Num_essai, plots$Bloc, trimws(plots$Parcelle), trimws(plots$Trait_au_fortenza), sep = "|")

	k1 <- match(paste(r1$Num_essai, r1$Bloc, trimws(r1$Parcelle), trimws(r1$Trait_au_fortenza), sep = "|"), plot_key)
	l1 <- data.frame(
		record_id = d$record_id[k1],
		trial_id = d$trial_id[k1],
		date = as.character(as.Date(r1$Date_obs)),
		plant_number_ = as.integer(r1$Num_plant),
		pest_species = "Spodoptera frugiperda",
		pest_severity = as.character(as.integer(r1$Devis)),
		severity_scale = "1-9",
		larva_present_ = r1$Presence_larv == 1,
		damage_present_ = r1$Pres_degat == 1,
		larval_instar_ = as.integer(r1$Stade_larv)
	)

	k2 <- match(paste(r2$Num_essai, r2$Bloc, trimws(r2$Parcelle), trimws(r2$Trait_au_fortenza), sep = "|"), plot_key)
	l2 <- data.frame(
		record_id = d$record_id[k2],
		trial_id = d$trial_id[k2],
		date = as.character(as.Date(r2$Date_obs)),
		row_number_ = as.integer(r2$Num_ligne),
		plant_density = as.numeric(r2$Nombre_plant_ha)
	)

	long <- carobiner::bindr(l1, l2)

	carobiner::write_files(path, meta, d, long = long)
}
