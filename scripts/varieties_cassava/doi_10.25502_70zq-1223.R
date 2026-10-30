# R script for "carob"
# license: GPLv3

## NOTES
# IITA cassava breeding trial (Cassavabase export); processed with the shared process_cassava() function.

## ISSUES
# URL 'https://data.iita.org/dataset/f504557c-c4c0-4d92-a13b-85abe676aa8e/resource/93554e84-c7ca-4f5f-9956-600dad56cc30/download/metadata.csv': status was 'SSL connect error'
# NA detected: yield

carob_script <- function(path) {

"
Clonal Evaluation Trial using 456 accessions in Ibadan selected from 17.GS.C4. Seedling Nursery.IB
"

	uri <- "doi:10.25502/70zq-1223"
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

