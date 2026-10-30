# R script for "carob"
# license: GPLv3

## NOTES
# IITA cassava breeding trial (Cassavabase export); processed with the shared process_cassava() function.

## ISSUES
# URL 'https://data.iita.org/dataset/de87d03b-2e42-4ca0-896c-884f86f8116c/resource/283c0d0b-1047-4bd1-97a8-7807de84ac9a/download/metadata.csv': status was 'SSL connect error'
# out of bounds: fwy_storage (1300, 88300)
# NA detected: yield

carob_script <- function(path) {

"
Evaluation of NCRP Trial using 12 yellow root clones in 2020-2021 breeding season in Zaria
"

	uri <- "doi:10.25502/q2nr-v965"
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

