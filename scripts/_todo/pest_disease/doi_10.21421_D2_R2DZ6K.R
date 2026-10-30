# R script for "carob"
# license: GPL (>=3)

## NOTES
# Medium-duration groundnut at Bayero University Kano under rainfed conditions. Dataverse year 2015.
# DPWkgha is dry pod weight (kg/ha) and is yield. DFWkgha is dry fodder. The description also calls Pykg_ha pod yield (kg/ha); the two columns are not the same, so Pykg_ha is kept as pod_yield_.
# Shelling ratio is a percentage. HSW (g/100 seeds) times 10 is seed_weight.
# DEM, DFF and DM are days, not calendar dates.
# Early leaf severity, late leaf severity and rust severity are 1-9 scores in the long table.
# Coordinates are the Buk bounding-box midpoint. northLongitude and southLongitude in the metadata are latitudes.

## ISSUES
# No fertilizer or plot area. Irrigated is FALSE because the description says rainfed.
# Pykg_ha / DPWkgha ranges from about 0.95 to 2.47. Which column the authors treat as the reported yield needs a check. This script uses the dry-pod column, consistent with the other files.
# The abstract says 15 lines and 3 released varieties and does not say which names are the checks.
# adm1 is not in the metadata as a state (the city string is Bayero University Kano).
# write_files: missing carob_contributor, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown pod_yield_ (the checker asks if this should be yield; dry pod weight is already yield), plant_population_, plants_harvested_, sound_mature_kernel_; invalid disease early leaf spot, late leaf spot.

## NOT PROCESSED
# None of the measurement columns were dropped except where noted as mapped. There is no row-number column.

## NEW VARIABLES
# pod_yield_: Pykg_ha, kg/ha, as labeled. Not used as yield.
# plant_population_: NPLE, plants per plot.
# plants_harvested_: NPH, plants per plot.
# sound_mature_kernel_: SMK. Unit not stated (Buk elite description says "Number").

carob_script <- function(path) {

"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under rainfed condition in Buk

Medium-Duration Groundnut for Multi-location evaluation comprises 15 breeding lines of groundnut and 3 released Varieties. The trial was conducted under rainfed condition and it was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses location which includes; Buk to confirm their        Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/R2DZ6K"
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
		carob_completion = 70,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Data file of Medium duration lines for agronomic traits in the location Buk.xlsx"]
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
		pod_yield_ = as.numeric(r$Pykg_ha),
		dmy_residue = as.numeric(r$DFWkgha),
		seed_weight = as.numeric(r$HSW) * 10,
		shelling_percentage = as.numeric(r[["Shelling ratio"]]),
		sound_mature_kernel_ = as.numeric(r$SMK),
		stringsAsFactors = FALSE
	)
	d$crop <- "groundnut"
	d$yield_part <- "pod"
	d$yield_moisture <- 0
	d$yield_isfresh <- FALSE
	d$stress <- "disease"
	d$irrigated <- FALSE
	d$country <- "Nigeria"
	d$location <- "Bayero University Kano"
	d$planting_date <- "2015"
	d$trial_id <- "Buk_2015"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	d$longitude <- (8.474923 + 8.475408) / 2
	d$latitude <- (11.983994 + 11.983964) / 2
	d$geo_uncertainty <- 27
	d$geo_source <- "ICRISAT Dataverse bounding box"
	d$record_id <- as.integer(seq_len(nrow(d)))

	n <- nrow(d)
	long <- data.frame(
		record_id = rep(d$record_id, 3),
		disease = rep(c("early leaf spot", "late leaf spot", "rust"), each=n),
		disease_severity = as.character(c(r[["Early Leaf Severity"]], r[["Late Leaf Severity"]], r[["Rust severity"]])),
		stringsAsFactors = FALSE
	)
	long$order <- as.integer(ave(seq_len(nrow(long)), long$record_id, FUN=seq_along))

	carobiner::write_files(path, meta, d, long=long)
}
