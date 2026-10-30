# R script for "carob"
# license: GPL (>=3)

## NOTES
# The measurement files for this record are restricted on Dataverse. The raw folder only has the unrestricted documentation (dictionary and, in some cases, a questionnaire). There are no observation rows to standardize.

## ISSUES
# No data table. The restricted measurement files were not downloaded.
# The script stops before write_files because there is no table.

## NOT PROCESSED
# The dictionary or questionnaire that is on disk describes columns that are not in the downloaded files.


carob_script <- function(path) {

"
Dataset for: Data collected for regulatory studies

This dataset contains the assessment of resistance to late blight disease in the field under natural infestation without the use of fungicide for more than one season. With varieties such as: 'Desiree', and non-transgenic 'Victoria' and the moderately resistant variety 'Cruza' planted in the field.
"

	uri <- "doi:10.21223/UY4GOV"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	stop("Measurement files are restricted and are not in the raw folder. Only documentation was available, so no data table was built.")
}

