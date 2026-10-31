# R script for "carob"
# license: GPL (>=3)

## NOTES
# Respondent rows are the first sheet of Data V2.xlsx.
# Country Uganda is the geographic coverage. The sheet has district, division, parish, and village, and no coordinates.
# The interview is about boiled sweetpotato and banana. Those preference sheets are not plot yields.

## ISSUES
# No coordinates.
# No crop yield or fertilizer.
# write_files: ; geo_from_source, latitude, longitude.

## NOT PROCESSED
# matooke and potato quality, preparation, and storage sheets in Data V2.xlsx, and Data_V1.xlsx. Preference answers, not a field table.

carob_script <- function(path) {

"
Dataset for: CC 4.1. Urban consumer preference study

This data set details information collected on preferred characteristics of urban consumers for boiled sweetpotato and banana.
"

	uri <- "doi:10.21223/RJVRRL"
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
		carob_effort = 0.25
	)

	f <- ff[basename(ff) == "Data V2.xlsx"]
	r <- carobiner::read.excel(f, sheet=1)
	sex <- tolower(trimws(as.character(r$sex)))

	d <- data.frame(
		hhid = trimws(as.character(r$ID)),
		country = "Uganda",
		adm2 = trimws(as.character(r$district)),
		adm3 = trimws(as.character(r$parish)),
		location = trimws(as.character(r$village)),
		sex = sex,
		age = as.numeric(r$age),
		on_farm = FALSE,
		is_survey = TRUE,
		irrigated = NA,
		trial_id = "urban_consumers"
	)

	carobiner::write_files(path, meta, d)
}
