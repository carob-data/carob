# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Agronomy trial file TAMASA_NOTs_Database_Tanzania_2016_17 data.xlsx is restricted.
# Downloaded file is soil chemistry only (0-20 cm). Same soil already processed as
# soil_samples hdl:11529/10548217. Not usable as agronomy without yield/treatment data.


carob_script <- function(path) {

"
Tamasa Nutrient Omission Trial Tanzania 2015-6

Nutrient Ommission Trials (NOT's) conducted in Tanzania 2015 (2016)
"

	uri <- "hdl:11529/10548311"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 0,
		carob_effort = 0.2
	)

	return(FALSE)
}
