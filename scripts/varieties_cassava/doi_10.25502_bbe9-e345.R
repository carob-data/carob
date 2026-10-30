# R script for "carob"
# license: GPLv3

## NOTES
# IITA cassava breeding trial (Cassavabase export); processed with the shared process_cassava() function.

## ISSUES
# Does not run with process_cassava(): URL 'https://data.iita.org/dataset/0e73a75c-ab6f-4231-82be-9fe6a045903f/resource/5a4f3b82-1088-41c4-a046-ab8628722e3e/download/2020-03-24t035503phenotype_download.csv': status was 'SSL connect error' || ERROR: invalid 'description' argument

carob_script <- function(path) {

"
Drought trial (49 clones) in Ngabu, Malawi in 2012-13 breeding season
"

	uri <- "doi:10.25502/bbe9-e345"
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

