# R script for "carob"
# license: GPLv3

## NOTES
# IITA cassava breeding trial (Cassavabase export); processed with the shared process_cassava() function.

## ISSUES
# out of bounds: fwy_storage (5000, 62500)

carob_script <- function(path) {

"
Bio-fortification of Cassava varieties in Preliminary Yield Trial (14 clones) in Ibadan 2017/2018 Breeding Season from 2016 (clonalevaluation 30poundabls and pyt8htc)_ IB.
"

	uri <- "doi:10.25502/hjb5-8k96"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Claude Opus 5.5",
		carob_completion = 90,
		carob_effort = NA,
		notes = NA
	)

	process_cassava <- carobiner::get_function("process_cassava", path, group)
	d <- process_cassava(ff)
	carobiner::write_files(path, meta, d$records, d$timerecs)
}

