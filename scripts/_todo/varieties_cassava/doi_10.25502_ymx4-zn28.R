# R script for "carob"
# license: GPLv3

## NOTES
# IITA cassava breeding trial (Cassavabase export); processed with the shared process_cassava() function.

## ISSUES
# @@ISSUES@@

carob_script <- function(path) {

"
Genomic Selection cycle 4 Preliminary Yield Trial (50 clones) evaluated in Ikenne 2018/2019 Breeding Season selected from 18.GS.cet120(REPLICATED).IKN 18.GS.CET120(REPLICATED).IKN from 17.GS.SN.IKN 2018-2019 PVA/CIAT_HDM special crosses
"

	uri <- "doi:10.25502/ymx4-zn28"
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

