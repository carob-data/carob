# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Rejected: the only data file is a 2-byte placeholder (a blank line).
# The CKAN record says the file was under embargo ("will be available after the embargo end date") and embargo_end_date is 2025-12-01.
# The downloaded CSV still has no header and no rows, so there are no measured observations and no simulated outputs to separate.
# Dataset-level notes mention cowpea genetic coefficients for DSSAT, Nigeria/Mali/Niger, and a coverage point (9.60004, 7.99997), but those are not plot measurements.


carob_script <- function(path) {

"
Generating genetic coefficient of different varieties of cowpea under different population to Validate DSSAT crop growing model

Kano State Agro-Pastoral Development Project (KSADP) and Climate Smart Agricultural Technologies (CSAT) for improved Rural Livelihoods and Food Security in Mali and Niger
"

	uri <- "doi:10.25502/31eh-8824/d"
	group <- "varieties_cowpea"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "KSADP-Kano; CSAT-Niger; CSAT_Mali",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.3
	)

	# The CSV is empty. Do not invent a table.
	message("Rejected: ", basename(ff[grepl("\\.csv$", ff)]), " has no data rows.")
	return(FALSE)
}
