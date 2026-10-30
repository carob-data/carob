# R script for "carob"
# license: GPL (>=3)

## NOTES
# Malawi potato baseline, part 4 only (input use). Country Malawi is the study title. This file has no district column. The abstract names Dowa and Mzimba as study districts, not as a location for each row.
# The season column is the code 1 or 2, which is not a season name, so it is stored as season_code_. Questionnaire number qnno is the household id and repeats across seasons. One fully identical raw row is dropped. A second part-4 line for the same household and season is kept as input_line_.
# Seed and fertilizer quantities use unit codes (pail, basket, 50 kg bag, 70-80 kg bag, oxcart, other, 88 = missing). Area is not in this file, so nothing is converted to kg/ha. Fertilizer type codes are D-compound, NPK 23:21, urea, CAN, sulphate of ammonia, other, and 88.
# part4b9 is 1 = fertilizer at planting, 0 = not applied, 88 = missing.

## ISSUES
# No coordinates, no yield, and no fertilizer rate. Unit codes are not a single kilogram measure.
# write_files: missing carob_contributor, latitude, longitude. unknown variable: season_code_ (season?). unknown variables: fertilizer_at_planting_, input_line_.

## NEW VARIABLES
# season_code_: season code 1 or 2. The values are not season names.
# fertilizer_at_planting_: TRUE when part4b9 is 1, FALSE when it is 0, NA when it is 88.
# input_line_: line number within a household and season.

## NOT PROCESSED
# Seed quantities, fertilizer quantities and prices, top-dressing quantities, foliar fertilizer, and manure. The unit code is not kilograms per hectare.
# Variable Description.docx is the questionnaire text.


carob_script <- function(path) {

"
Dataset for: RTC-Action Malawi Project Baseline study for Potato Component

The baseline study was conducted to assess and understand the socio-economic situation of farmers including potato production methods and technologies, seed system, input and output market systems, farmer capacity levels, perception on potato nutrition issues and food security situation. The results show that potato production is mainly dominated by female headed households in the new districts (Dowa and Mzimba) mainly because the crop is relatively newer but slowly getting popularity due to the declining tobacco performance that has been the number one cash crop in these districts. The average age of the sampled household heads was 34 in all the districts and this result indicates that the potato production is mainly dominated by people who are still in the productive age. This observation is consistent with the fact that potato production is relatively labour intensive hence it requires participation of active adults and the youth. Further to this, the results show that on average the households have a total land size of 1.5 acres (0.6 ha) which is consistent with the results of the recent IHS 4 that also reported that on average in Malawi, the average cultivated area is about 1.5 acres and further results also show that on average, male-headed households cultivated 1.7 acres compared to their female counterparts who cultivated 1.2 acres.
"

	uri <- "doi:10.21223/P3/15NRGC"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.3
	)

	f <- ff[grepl("Part 4 Input Use", basename(ff))]
	r <- haven::read_dta(f)
	r <- r[!duplicated(as.data.frame(r)), ]
	applied <- as.integer(r$part4b9)
	d <- data.frame(
		hhid = trimws(as.character(r$qnno)),
		season_code_ = as.integer(r$season),
		fertilizer_at_planting_ = ifelse(applied == 1, TRUE, ifelse(applied == 0, FALSE, NA)),
		country = "Malawi",
		crop = "potato",
		geo_from_source = FALSE,
		is_survey = TRUE,
		on_farm = TRUE,
		trial_id = "Malawi_potato_inputs",
		stringsAsFactors = FALSE
	)
	# A few household-seasons have a second part-4 line. The line number keeps those rows distinct.
	d$input_line_ <- ave(seq_len(nrow(d)), d$hhid, d$season_code_, FUN=seq_along)
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
