# R script for "carob"
# license: GPLv3

## NOTES
# IITA cassava breeding trial (Cassavabase export); processed with the shared process_cassava() function.

## ISSUES
# out of bounds: yield (3720, 188470), fwy_storage (3720, 188470)
# high crop yield: cassava: 188470
# NA detected: yield

carob_script <- function(path) {

"
22 clones established in Valley Forest preliminary yield trial in Kiyaka, DRC for the 2019/2020 breeding season.
"

	uri <- "doi:10.25502/08qr-dr62"
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

