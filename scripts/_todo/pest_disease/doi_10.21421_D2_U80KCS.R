# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Despite the title mentioning foliar diseases, the workbook contains only pod yield (no disease scores); this dataset may belong in the varieties group.
# Source coordinates (7.362 N, 3.850 E) fall at IAR&T Moor Plantation, Ibadan (Oyo State), whereas ICRISAT's northern Nigeria groundnut partner IAR is in Samaru, Zaria (Kaduna State); confirm the actual site before setting adm1.
# No fertilizer rates, moisture basis, plot size, or exact planting/harvest dates are reported.

## MISSING
# yield_moisture, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date

## NOTES
# Rainfed medium-duration groundnut trial at IAR, Nigeria (2015, from Dataverse timePeriodCovered/productionDate): 15 breeding lines + 3 released varieties (SAMNUT22, SAMNUT24, SAMNUT25) in 3 replications x 3 blocks.
# Pykg_h is pod yield in kg/ha per the description file. Coordinates are the midpoint of the Dataverse bounding box.

## NOT PROCESSED
# Data file, S.no and Entry number: row/entry indexes only.

carob_script <- function(path) {

"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under rainfed condition in IAR

Medium-Duration Groundnut for Multi-location evaluation comprises 15 breeding lines of groundnut and 3 released Varieties. The trial was conducted under rainfed condition and it was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses location which includes.        Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/U80KCS"
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

	f <- ff[basename(ff) == "Data file of Medium duration lines for agronomic traits in the location IAR.xlsx"]
	r <- carobiner::read.excel(f)

	variety <- trimws(r$Variety)

	d <- data.frame(
		country = "Nigeria",
		location = "IAR",
		crop = "groundnut",
		variety = variety,
		# Abstract notes 15 breeding lines and 3 released varieties (SAMNUT22, SAMNUT24, SAMNUT25)
		variety_type = ifelse(grepl("^SAMNUT", variety), "improved", NA_character_),
		treatment = variety,
		rep = as.integer(r[["Replication number"]]),
		block_id = as.character(r$Block),
		plot_id = as.character(r[["Plot no"]]),
		yield = as.numeric(r$Pykg_h),
		yield_part = "pod",
		# Dataverse timePeriodCovered is 2015-06-14 to 2015-12-25 (day-of-month is template across ICRISAT deposits)
		planting_date = "2015",
		trial_id = "IAR_2015",
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = FALSE,
		longitude = (3.850134 + 3.850365) / 2,
		latitude = 7.362372,
		geo_uncertainty = 13,
		geo_source = "ICRISAT Dataverse bounding box",
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
