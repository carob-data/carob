# R script for "carob"
# license: GPL (>=3)

## NOTES
# Medium-duration groundnut at Minjibir, post-rainy season. Dataverse time coverage is 2015. Only the year is used: the day-of-month (14 and 25) is repeated across these ICRISAT deposits.
# DPWkgha is dry pod yield (kg/ha). DFWkgha is dry fodder (kg/ha). Shelling ratio is already a percentage (about 58-71).
# HSW is grams per 100 seeds, multiplied by 10 for seed_weight.
# DEM, DFF and DM are days (values about 6-8, 26-28 and 90-115), stored as emergence_days, flowering_days and maturity_days. The description calls them dates.
# ELS, LLS and RUST are 1-9 scores in the long table. The description's ELS80 and LLS90 names are not the column names in the file (the file says ELS, LLS, RUST).
# Coordinates are the same Minjibir bounding box as the 2016 elite trial.

## ISSUES
# No fertilizer, irrigation, or plot area. The title says post-rainy season; irrigation is not stated.
# Dataverse puts Minjibir in the state field. adm1 was not set from that.
# SMK reaches 98.9, so it is not an integer kernel count. The description does not give the unit.
# early leaf spot and late leaf spot are not in the disease vocabulary.
# write_files: missing carob_contributor, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown plant_population_, plants_harvested_, sound_mature_kernel_; invalid disease early leaf spot, late leaf spot.

## NOT PROCESSED
# S.no.
# DPWg: dry pod weight in grams. DPWkgha was used.

## NEW VARIABLES
# plant_population_: NPLE, plants per plot. The description says "plant population".
# plants_harvested_: NPH, plants per plot.
# sound_mature_kernel_: SMK as recorded.

carob_script <- function(path) {

"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under post rainy season in Minjibir

Medium-Duration Multi-location trial conducted under Post-season .The trial was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses across the location which includes, Minjibir        Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/FLVTLT"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = "3 replications in blocks",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_residue;disease_severity",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Data file of medium duration multi-location trial post rainy in Minjibir.xlsx"]
	r <- carobiner::read.excel(f, na="NA")

	d <- data.frame(
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
		sound_mature_kernel_ = as.numeric(r$SMK)
	)
	d$crop <- "groundnut"
	d$yield_part <- "pod"
	d$yield_moisture <- 0
	d$yield_isfresh <- FALSE
	d$stress <- "disease"
	d$country <- "Nigeria"
	d$location <- "Minjibir"
	d$planting_date <- "2015"
	d$trial_id <- "Minjibir_2015"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	d$longitude <- (8.625937 + 8.630400) / 2
	d$latitude <- (12.192154 + 12.191986) / 2
	d$geo_uncertainty <- 243
	d$geo_source <- "ICRISAT Dataverse bounding box"
	d$record_id <- as.integer(seq_len(nrow(d)))

	n <- nrow(d)
	long <- data.frame(
		record_id = rep(d$record_id, 3),
		disease = rep(c("early leaf spot", "late leaf spot", "rust"), each=n),
		disease_severity = as.character(c(r$ELS, r$LLS, r$RUST))
	)
	long$order <- as.integer(ave(seq_len(nrow(long)), long$record_id, FUN=seq_along))

	carobiner::write_files(path, meta, d, long=long)
}
