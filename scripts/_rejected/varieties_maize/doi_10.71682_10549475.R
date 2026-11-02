# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Deposit contains only the 2026 CIMMYT Maize South Asia Product Announcement
# PDF. No plot-level Stage 4/5 trial tables were provided.


carob_script <- function(path) {

"
2026 CIMMYT-Asia Maize Regional On-Station (Stage 4) and On-Farm (Stage 5) Trials: Results of the 2024-2025 Seasons and Product Announcement

One new and improved maize hybrid, developed by the CIMMYT Global Maize Program, is available for uptake by public and private sector partners, especially those interested in marketing or disseminating hybrid maize seed for rainfed cultivation across the lowland tropics of South Asia and similar agro-ecologies in other regions.  Following a rigorous trialing and a stage-gate advancement process culminating in the 2025 Stage 5 trials, CIMMYT advanced one new elite maize hybrid in Asia in 2026. Phenotypic data collected in Stage 4 and Stage 5 trials for the selected hybrids as well as information about the trial sites are provided in this dataset. These trials were conducted through a network of partners, including NARES and private seed companies, in Asia, under various management and environmental conditions.
"

	uri <- "doi:10.71682/10549475"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-station experiment",
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
