# R script for "carob"
# license: GPL (>=3)

## NOTES
# Medium-duration groundnut lines at IAR, Nigeria, rainfed. The description workbook says Pykg_h is pod yield in kg/ha.
# One trial (18 lines, 3 blocks, 3 replications). Block is not the same as replication.
# Coordinates are the midpoint of the Dataverse bounding box (3.850134–3.850365 E, 7.362372 N).

## ISSUES
# The title is an evaluation for foliar diseases, but the workbook has no disease score, date, fertilizer, or plot size.
# Dataverse productionDate is 2015; it is not used as a planting date.
# The metadata does not name a state. The point is not the usual Samaru (Zaria) IAR coordinate; confirm the site.
# No publication DOI in the Dataverse record.
# write_files: missing carob_contributor, yield_moisture, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# Data file, S.no: row number.
# Data file, Entry number: trial entry index, one per variety.

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
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Data file of Medium duration lines for agronomic traits in the location IAR.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		country = "Nigeria",
		location = "IAR",
		crop = "groundnut",
		variety = trimws(r$Variety),
		treatment = trimws(r$Variety),
		rep = as.integer(r[["Replication number"]]),
		block_id = as.character(r$Block),
		plot_id = as.character(r[["Plot no"]]),
		yield = as.numeric(r$Pykg_h),
		yield_part = "pod",
		stress = "disease",
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = FALSE,
		longitude = (3.850134 + 3.850365) / 2,
		latitude = 7.362372,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
