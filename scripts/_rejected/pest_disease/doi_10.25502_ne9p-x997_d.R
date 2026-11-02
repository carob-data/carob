# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The only data file is a 2-byte placeholder (CRLF). Resource description:
# "This dataset is under embargo and will be available after the embargo end
# date. Please contact creator". embargo_end_date in metadata: 2025-12-01.


carob_script <- function(path) {

"
EVALUATION OF POST-EMERGENCE HERBICIDES FOR WEED CONTROL IN Soybean (Glycine max L.) in Sudan and Guinea savanna zone of Nigeria

Kano State Agro-Pastoral Development Project (KSADP) and Climate Smart Agricultural Technologies (CSAT) for improved Rural Livelihoods and Food Security in Mali and Niger
"

	uri <- "doi:10.25502/ne9p-x997/d"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication = NA,
		project = "KSADP; CSAT",
		design = NA,
		data_type = "experiment",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 0,
		carob_effort = 0.1
	)

	return(FALSE)
}
