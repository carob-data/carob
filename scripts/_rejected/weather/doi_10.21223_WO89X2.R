# R script for "carob"
# license: GPL (>=3)

## REJECTED
# Rainfall and temperature are HOBOware Pro 3.3.2 binaries for a UA-003-64 Pendant Temp/Event logger. The series body is proprietary, and R has no reader for this .hobo layout.

carob_script <- function(path) {

"
Dataset for: Regional climatic data from Burundi

Rainfall and Temperature of certain localities of Burundi
"

	uri <- "doi:10.21223/WO89X2"
	group <- "weather"
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
