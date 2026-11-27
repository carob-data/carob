# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Despite the title mentioning foliar diseases, the data file contains no disease scores; consider moving this script from pest_disease to varieties.
# Plot 102 (ICG 5891) has DFWkgha = 19982.2 kg/ha (all other plots are 1280-2564 kg/ha), likely a 10x typo for 1998.2; left as recorded.
# In the first plant-count pair (cols 9-10), NPLE - NPH is identically 25 on all 53 non-missing plots. Columns 17-22 contain a second incompatible set of counts and weights (NPLE, NPH, DFW, DPWg, DPWKg, DPWKg_h where DPWKg_h = 10 * DPWg / 12 m2, r = 0.49 with DPWkgha); left unprocessed pending clarification of whether they represent another trial/environment.
# Plot 312 (FDRF7-57) has NA for all measured traits and is kept as recorded.
# In Dataverse geospatial metadata, southLongitude (12.620996) is slightly north of northLongitude (12.619884); their midpoint is used.

## NEW
# plant_population_: first NPLE column, plants emerged per plot, count
# plants_harvested_: first NPH column, plants harvested per plot, count
# sound_mature_kernel_: SMK, sound mature kernels, %

## MISSING
# harvest_date, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Rainfed medium-duration groundnut variety trial (15 breeding lines + 3 released SAMNUT varieties x 3 reps) at Gumel, Jigawa State, Nigeria in 2015.
# Dataverse timePeriodCovered is 2015-06-14 to 2015-12-25 (repeated across companion 2015 deposits), so only planting year "2015" is used.
# DPWkgha is dry pod yield (yield_moisture = 0, yield_isfresh = FALSE), DFWkgha is dry fodder (dmy_residue), HSW * 10 is 1000-seed weight (seed_weight), and DEM/DFF/DM are days to emergence, flowering, and maturity.

## NOT PROCESSED
# NPLE...17, NPH...18, DFW, DPWg, DPWKg, DPWKg_h: second incompatible set of plant counts and pod/fodder weights (see ISSUES).

carob_script <- function(path) {

"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under rainfed condition in Gumel

Medium-Duration Groundnut for Multi-location evaluation comprises 15 breeding lines of groundnut and 3 released Varieties. The trial was conducted under rainfed condition and it was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses location which includes; Gumel
"

	uri <- "doi:10.21421/D2/H2GX2F"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_residue;seed_weight;shelling_percentage;emergence_days;flowering_days;maturity_days",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Data file of Medium duration lines for agronomic traits in the location Gumel..xlsx"]
	r <- carobiner::read.excel(f, na = "NA")

	variety <- trimws(r$Variety)

	d <- data.frame(
		country = "Nigeria",
		adm1 = "Jigawa",
		adm2 = "Gumel",
		location = "Gumel",
		crop = "groundnut",
		variety = variety,
		variety_type = ifelse(grepl("^SAMNUT", variety), "released variety", "breeding line"),
		treatment = variety,
		rep = as.integer(r[["Replication number"]]),
		block_id = as.character(r$Block),
		plot_id = as.character(r[["Plot no"]]),
		emergence_days = as.numeric(r$DEM),
		flowering_days = as.numeric(r$DFF),
		maturity_days = as.numeric(r$DM),
		plant_population_ = as.numeric(r[["NPLE...9"]]),
		plants_harvested_ = as.numeric(r[["NPH...10"]]),
		yield = as.numeric(r$DPWkgha),
		dmy_residue = as.numeric(r$DFWkgha),
		seed_weight = as.numeric(r$HSW) * 10,
		shelling_percentage = as.numeric(r[["Shelling ratio"]]),
		sound_mature_kernel_ = as.numeric(r$SMK),
		yield_part = "pod",
		yield_moisture = 0,
		yield_isfresh = FALSE,
		irrigated = FALSE,
		planting_date = "2015",
		trial_id = "Gumel_2015",
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = (9.370741 + 9.393268) / 2,
		latitude = (12.619884 + 12.620996) / 2,
		geo_from_source = TRUE,
		geo_uncertainty = 1225,
		geo_source = "ICRISAT Dataverse bounding box"
	)

	carobiner::write_files(path, meta, d)
}
