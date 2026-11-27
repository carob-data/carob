# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Despite the title mentioning foliar diseases, the data file contains only dry pod yield (DPWkgha) with no disease scores; consider moving this script from pest_disease to varieties.
# The Dataverse bounding box (7.362 N, 3.850 E) points to IAR&T (Moor Plantation, Ibadan, Oyo State), whereas the description names IAR (Institute for Agricultural Research, normally at Samaru, Zaria, Kaduna State); coordinates are kept as published and adm1 is omitted pending confirmation.

## MISSING
# harvest_date, irrigated, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Medium-duration groundnut variety trial (15 varieties x 3 reps in 5 blocks per rep) at IAR, Nigeria in 2015 (post-rainy season).
# Dataverse timePeriodCovered is 2015-06-14 to 2015-12-25 (repeated across companion 2015 ICRISAT Nigeria deposits), so only planting year "2015" is used.
# DPWkgha is dry pod weight per hectare (yield_part = "pod", yield_moisture = 0, yield_isfresh = FALSE).

carob_script <- function(path) {

"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under post rainy season in IAR

Medium-Duration Multi-location trial conducted under Post-season .The trial was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses across the location which includes, IAR(Institute for Agriculture Research),Nigeria
"

	uri <- "doi:10.21421/D2/EUDKST"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Data file of medium duration multi-location trial post rainy in IAR.xlsx"]
	r <- carobiner::read.excel(f, na = "NA")

	d <- data.frame(
		country = "Nigeria",
		location = "IAR",
		crop = "groundnut",
		variety = trimws(r$Variety),
		treatment = trimws(r$Variety),
		rep = as.integer(r[["Replication number"]]),
		block_id = as.character(r$Block),
		plot_id = as.character(r[["Plot no"]]),
		yield = as.numeric(r$DPWkgha),
		yield_part = "pod",
		yield_moisture = 0,
		yield_isfresh = FALSE,
		planting_date = "2015",
		trial_id = "IAR_2015",
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = (3.850134 + 3.850365) / 2,
		latitude = 7.362372,
		geo_from_source = TRUE,
		geo_uncertainty = 13,
		geo_source = "ICRISAT Dataverse bounding box"
	)

	carobiner::write_files(path, meta, d)
}
