# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has a dictionary and the dataset metadata. The measurement workbook is marked restricted and was not extracted.
# Files on disk: 04_Data_dictionary.xlsx, doi_10.21223_PSAJJN.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Micronutrient and Vitamin C Evaluation of  Advanced Potato Clones from Chiara, Ayacucho - 2006

In 2006, 75 advanced potato clones, originating from the locality of Chiara in the province of Huamanga, Ayacucho, were evaluated. Micronutrient and vitamin C analyses were conducted using spectrophotometry. The study employed a completely randomized design (CRD) with two replications to accurately determine the nutrient content of the clones.
"

	uri <- "doi:10.21223/PSAJJN"
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
