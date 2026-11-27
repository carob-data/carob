# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Plot area is not reported, so NPLE and NPH cannot be converted to plants/ha and remain per-plot counts.
# Pykg_ha is used as yield because DPWkgha equals Pykg_ha * 128 / NPH on 39 of 54 rows (stand-adjusted to 128 plants/plot) and exceeds Pykg_ha by up to 2.47x on the remaining 15 rows; DPWkgha is retained as pod_yield_adjusted_ for verification.

## NEW
# plant_population_: emerged plants per plot (NPLE; count/plot, plot area unknown)
# plants_harvested_: harvested plants per plot (NPH; count/plot, plot area unknown)
# pod_yield_adjusted_: stand-adjusted dry pod yield (DPWkgha; kg/ha)
# sound_mature_kernel_: sound mature kernel score/count (SMK; values 75-98, unit not stated)
# disease: "late leaf spot" not yet in values_disease.csv

## MISSING
# N_fertilizer, P_fertilizer, K_fertilizer, harvest_date

## NOTES
# Medium-duration groundnut multi-location trial at Bayero University Kano (BUK), Kano State, Nigeria, 2015 under rainfed conditions (irrigated = FALSE).
# 18 entries (15 breeding lines + 3 released varieties SAMNUT22, SAMNUT24, SAMNUT25) x 3 reps in blocks (54 plots).
# Pykg_ha is dry pod yield (yield_part = "pod", yield_moisture = 0, yield_isfresh = FALSE); DFWkgha is dry fodder (dmy_residue). DEM, DFF, and DM are days, not calendar dates.
# Early leaf spot, late leaf spot, and rust severity (1-9 scale) are stored in the long table.
# Coordinates are the midpoint of the Dataverse bounding box (northLongitude/southLongitude fields hold latitudes ~11.984 N).

carob_script <- function(path) {

"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under rainfed condition in Buk

Medium-Duration Groundnut for Multi-location evaluation comprises 15 breeding lines of groundnut and 3 released Varieties. The trial was conducted under rainfed condition and it was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses location which includes; Buk to confirm their        Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/R2DZ6K"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = "3 replications in blocks",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_residue;emergence_days;flowering_days;maturity_days;seed_weight;shelling_percentage;disease_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Data file of Medium duration lines for agronomic traits in the location Buk.xlsx"]
	r <- carobiner::read.excel(f, na = "NA")

	variety <- trimws(r$Variety)
	vtype <- ifelse(variety %in% c("SAMNUT22", "SAMNUT24", "SAMNUT25"), "released variety", "breeding line")

	d <- data.frame(
		variety = variety,
		variety_type = vtype,
		treatment = variety,
		rep = as.integer(r[["Replication number"]]),
		block_id = as.character(r$Block),
		plot_id = as.character(r[["Plot no"]]),
		emergence_days = as.numeric(r$DEM),
		flowering_days = as.numeric(r$DFF),
		maturity_days = as.numeric(r$DM),
		plant_population_ = as.numeric(r$NPLE),
		plants_harvested_ = as.numeric(r$NPH),
		# Pykg_ha is unadjusted pod yield; DPWkgha is stand-adjusted (Pykg_ha * 128 / NPH on 39/54 rows)
		yield = as.numeric(r$Pykg_ha),
		pod_yield_adjusted_ = as.numeric(r$DPWkgha),
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
	d$irrigated <- FALSE
	d$country <- "Nigeria"
	d$adm1 <- "Kano"
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
		disease = rep(c("early leaf spot", "late leaf spot", "rust"), each = n),
		disease_severity = as.character(c(r[["Early Leaf Severity"]], r[["Late Leaf Severity"]], r[["Rust severity"]])),
		severity_scale = "1-9"
	)
	long$order <- as.integer(ave(seq_len(nrow(long)), long$record_id, FUN = seq_along))

	carobiner::write_files(path, meta, d, long = long)
}
