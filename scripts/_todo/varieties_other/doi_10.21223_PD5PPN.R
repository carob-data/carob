# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has the data dictionary only. The oca, olluco, and mashua databases are marked restricted and were not extracted.
# Files on disk: 04_Data_dictionary.xlsx, doi_10.21223_PD5PPN.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Morphological Characterization of Native Andean Tubers from the Cumbal Indigenous Reservation

Twenty Oca, 18 Olloco and 11 majua varieties were characterised in situ. The variables recorded were at tuber level. The tubers came from farmers in Cumbal, Colombia. Highly discriminating morphological descriptors published by IPC 2014, IPGRI/CIP 2003, 2007 were used.
"

	uri <- "doi:10.21223/PD5PPN"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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
