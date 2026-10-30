# R script for "carob"
# license: GPLv3

## NOTES
# IITA cassava breeding trial (Cassavabase export); processed with the shared process_cassava() function.

## ISSUES
# NA detected: yield

carob_script <- function(path) {

"
Clonal Evaluation of cassava varieties (379 clones) for poundability in Ibadan 2017/2018 breeding season from 2016sn b15 IB.
"

	uri <- "doi:10.25502/9gdk-f316"
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

