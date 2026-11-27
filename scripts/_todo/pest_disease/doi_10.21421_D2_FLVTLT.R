# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Plot area is not reported, so NPLE and NPH could not be converted to plant_density (plants/ha); however, DPWkgha exactly equals 10 * DPWg / (NPH * 0.05), indicating yield was scaled per harvested plant assuming 0.05 m2/plant.
# The codebook labels disease traits ELS80 (80 DAP) and LLS90 (90 DAP) whereas the spreadsheet columns are named ELS, LLS, and RUST; DAP = 80 and 90 were applied from the codebook.

## NEW
# plant_population_: NPLE, plants emerged per plot, count
# plants_harvested_: NPH, plants harvested per plot, count
# sound_mature_kernel_: SMK, sound mature kernels, %
# disease: late leaf spot

## MISSING
# harvest_date, irrigated, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Medium-duration groundnut variety trial (15 varieties x 3 reps in 5 blocks per rep) at ICRISAT Minjibir (Kano State, Nigeria), post-rainy 2015.
# Dataverse timePeriodCovered is 2015-06-14 to 2015-12-25 (repeated across companion 2015 deposits), so only planting year "2015" is used.
# DPWkgha is dry pod yield (yield_moisture = 0, yield_isfresh = FALSE), DFWkgha is dry fodder (dmy_residue), HSW * 10 is 1000-seed weight (seed_weight), and DEM/DFF/DM are days to emergence, flowering, and maturity.
# Foliar disease severity scores (1-9 scale) for early leaf spot (80 DAP), late leaf spot (90 DAP), and rust are stored in the long table.

## NOT PROCESSED
# DPWg: plot dry pod weight in g, redundant with DPWkgha (DPWkgha = 10 * DPWg / (NPH * 0.05)).

carob_script <- function(path) {

"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under post rainy season in Minjibir

Medium-Duration Multi-location trial conducted under Post-season .The trial was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses across the location which includes, Minjibir
"

	uri <- "doi:10.21421/D2/FLVTLT"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_residue;disease_severity;seed_weight;shelling_percentage;emergence_days;flowering_days;maturity_days",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 95,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Data file of medium duration multi-location trial post rainy in Minjibir.xlsx"]
	r <- carobiner::read.excel(f, na = "NA")

	d <- data.frame(
		country = "Nigeria",
		adm1 = "Kano",
		adm2 = "Minjibir",
		location = "Minjibir",
		crop = "groundnut",
		variety = trimws(r$Variety),
		treatment = trimws(r$Variety),
		rep = as.integer(r[["Replication number"]]),
		block_id = as.character(r$Block),
		plot_id = as.character(r[["Plot no"]]),
		emergence_days = as.numeric(r$DEM),
		flowering_days = as.numeric(r$DFF),
		maturity_days = as.numeric(r$DM),
		plant_population_ = as.numeric(r$NPLE),
		plants_harvested_ = as.numeric(r$NPH),
		yield = as.numeric(r$DPWkgha),
		dmy_residue = as.numeric(r$DFWkgha),
		seed_weight = as.numeric(r$HSW) * 10,
		shelling_percentage = as.numeric(r[["Shelling ratio"]]),
		sound_mature_kernel_ = as.numeric(r$SMK),
		yield_part = "pod",
		yield_moisture = 0,
		yield_isfresh = FALSE,
		stress = "disease",
		planting_date = "2015",
		trial_id = "Minjibir_2015",
		record_id = as.integer(seq_len(nrow(r))),
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = (8.625937 + 8.630400) / 2,
		latitude = (12.192154 + 12.191986) / 2,
		geo_from_source = TRUE,
		geo_uncertainty = 243,
		geo_source = "ICRISAT Dataverse bounding box"
	)

	n <- nrow(d)
	long <- data.frame(
		record_id = rep(d$record_id, 3),
		order = rep(1:3, each = n),
		DAP = rep(c(80L, 90L, NA_integer_), each = n),
		disease = rep(c("early leaf spot", "late leaf spot", "rust"), each = n),
		disease_severity = as.character(c(r$ELS, r$LLS, r$RUST)),
		severity_scale = rep("1-9", 3 * n)
	)

	carobiner::write_files(path, meta, d, long = long)
}
