# R script for "carob"
# license: GPL (>=3)

## NOTES
# Medium-duration groundnut at IAR, post-rainy season. The workbook has only dry pod yield (DPWkgha, kg/ha). Dataverse year is 2015.
# The description says IAR is the Institute for Agricultural Research. The bounding box is about 7.362N, 3.850E. That midpoint is used as published.
# There are no foliar-disease columns in the file, despite the title.

## ISSUES
# Kept in pest_disease because the raw files are already in that folder. The table is a yield variety trial with no disease scores.
# No fertilizer, irrigation, flowering, or plot area. Post-rainy season does not by itself say the trial was irrigated.
# The bounding box should be checked against the institute the authors meant. It is not the Samaru/Zaria area.
# No state name in the metadata.
# write_files: missing carob_contributor, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date.

## NOT PROCESSED
# S.no. Block and plot number are kept.

carob_script <- function(path) {

"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under post rainy season in IAR

Medium-Duration Multi-location trial conducted under Post-season .The trial was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses across the location which includes, IAR(Institute for Agriculture Research),Nigeria        Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/EUDKST"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = "3 replications in blocks",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Data file of medium duration multi-location trial post rainy in IAR.xlsx"]
	r <- carobiner::read.excel(f, na="NA")

	d <- data.frame(
		variety = trimws(r$Variety),
		treatment = trimws(r$Variety),
		rep = as.integer(r[["Replication number"]]),
		block_id = as.character(r$Block),
		plot_id = as.character(r[["Plot no"]]),
		yield = as.numeric(r$DPWkgha)
	)
	d$crop <- "groundnut"
	d$yield_part <- "pod"
	d$yield_moisture <- 0
	d$yield_isfresh <- FALSE
	d$country <- "Nigeria"
	d$location <- "IAR"
	d$planting_date <- "2015"
	d$trial_id <- "IAR_2015"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	d$longitude <- (3.850134 + 3.850365) / 2
	d$latitude <- 7.362372
	d$geo_uncertainty <- 13
	d$geo_source <- "ICRISAT Dataverse bounding box"

	carobiner::write_files(path, meta, d)
}
