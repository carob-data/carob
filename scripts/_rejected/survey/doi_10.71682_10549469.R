# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Household survey xlsx, codebook, and impact report are restricted. Only the questionnaire PDF downloaded.


carob_script <- function(path) {

"
Scaling Conservation Agriculture-based Sustainable Intensification in Ethiopia (SCASI): Endline Household Survey Dataset (2025)

This dataset contains household-level data from 417 households collected in Ethiopia during February–March 2025 under the Scaling Conservation Agriculture-based Sustainable Intensification in Ethiopia (SCASI) project. The endline survey was conducted to assess the adoption of Conservation Agriculture-based Sustainable Intensification (CASI) practices and associated outcomes on agricultural production, livelihoods, and food security. It also includes information on household demographics, livestock ownership and management, income sources, and farming practices.
"

	uri <- "doi:10.71682/10549469"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=0, minor=0,
		publication = NA,
		project = "SCASI",
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
