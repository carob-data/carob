# R script for "carob"
# license: GPLv3

## NOTES
# IITA cassava breeding trial (Cassavabase export); processed with the shared process_cassava() function.

## ISSUES
# out of bounds: fwy_storage (1400, 93100), yield_moisture (55.8, 90.4)
# high crop yield: cassava: 93100
# NA detected: yield

carob_script <- function(path) {

"
Clonal evaluation of cassava varieties (676 clones) for high total carotenoid retention in 2018-19 Breeding season advanced from 2018seedling nursery_b16 IB.
"

	uri <- "doi:10.25502/67jy-s510"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_contributor = "Robert Hijmans",
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

