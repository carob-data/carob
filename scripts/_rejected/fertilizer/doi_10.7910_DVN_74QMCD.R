# R script for "carob"
# license: GPL (>=3)

## ISSUES
# DEACCESSIONED


carob_script <- function(path) {

"
Rapid Characterisation of Farming Systems in Africa RISING - Ghana

Africa RISING farming-systems characterisation survey. No files were listed in the Dataverse dataset metadata, so the records could not be processed.
"

	uri <- "doi:10.7910/DVN/74QMCD"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=0, minor=0,
		publication = NA,
		project = "Africa RISING",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 0,
		carob_effort = 0.1
	)

	return(FALSE)
}
