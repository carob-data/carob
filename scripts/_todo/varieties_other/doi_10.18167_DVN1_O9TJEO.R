# R script for "carob"
# license: GPL (>=3)

## NOTES
# One row per plot in CNA6_phenotypic_data. Yield is already kg/ha (description: YLD, kg/ha).
# Crop is rice from the dataset alternative title ("upland rice breeding"), keyword "rice", and studyAssayOrganism Oryza sativa. The phenotype file has no crop column.
# Country Brazil is the dataset geographicCoverage. Location is the location code in the file. Design codes BAF and LAT are not decoded.

## ISSUES
# No coordinates, fertilizer, or planting date in the file. Year is the trial year, not a planting date. Plant height includes a value of 2 cm.
# write_files: missing variables: project. missing variables: on_farm, yield_moisture, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: selection_cycle_. out of bounds: plant_height (2, 156). NA detected: yield.

## NOT PROCESSED
# None beyond the columns above. The design code is kept out of design because BAF and LAT are not defined in the file.

## NEW VARIABLES
# selection_cycle_: recurrent-selection cycle (3 to 7).


carob_script <- function(path) {

"
Phenotypic data of the CNA6 population from cycle 3 to cycle 7

The CNA6 population, evaluated over five cycles (3 to 7) of selection, including 20 field trials, was used to assess the realized genetic gain. The experimental data used in this study was generated in multisite trials conducted on S0:2 families from five cycles of recurrent selection in the CNA6 population. The following traits were analyzed: grain yield (YLD, kg‧ha−1), days to flowering (FLW, days) and plant height (PHT, cm).
"

	uri <- "doi:10.18167/DVN1/O9TJEO"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1038/s41437-023-00636-3",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;plant_height",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "CNA6_phenotypic_data.csv"]
	r <- read.csv(f)

	d <- data.frame(
		trial_id = r$study,
		crop = "rice",
		variety = as.character(r$gid),
		treatment = as.character(r$gid),
		location = r$location,
		country = "Brazil",
		rep = as.integer(r$rep),
		block_id = as.character(r$bloc),
		plot_id = as.character(r$plot),
		flowering_days = as.numeric(r$flw),
		plant_height = as.numeric(r$pht),
		yield = as.numeric(r$yld),
		yield_part = "grain",
		selection_cycle_ = as.integer(r$cycle),
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
