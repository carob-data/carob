# R script for "carob"
# license: GPL (>=3)

## ISSUES
# All tabular files are restricted on Harvard Dataverse (001_soybeanMother_UW, 002_soybean_NR, 003_soybeanMother_UE). Credentials/guestbook needed.


carob_script <- function(path) {

"
Africa RISING-Soybean Baby Trial in Northern Ghana

This data study contains three-year data from three northern regions of Ghana which were collected to evaluate and demonstrate soybean practices.
"

	uri <- "doi:10.7910/DVN/2TLSY3"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication = NA,
		project = "Africa RISING",
		design = NA,
		data_type = "on-farm experiment",
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
