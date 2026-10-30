# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has a dictionary and the dataset metadata. The measurement workbook is marked restricted and was not extracted.
# Files on disk: 03_Data_dictionary.xlsx, doi_10.21223_3BDI1X.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Yield Evaluation of Virus Heat Tolerant (VHT) Clones Potato in Lima - 2014

A total of 162 clones from the Virus Heat Tolerant (VHT) population were analyzed in the La Molina region Lima during 2014 to determine their yield.
"

	uri <- "doi:10.21223/3BDI1X"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.1
	)

	message("No measurement table in the local raw files for ", uri)
	invisible(meta)
}
