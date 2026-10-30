# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: 05_Data_dictionary.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Evaluation of Yield and Micronutrient Concentrations in Advanced Potato Clones from Chiara, Ayacucho - 2006

An evaluation of 74 advanced potato clones from the district of Chiara, Ayacucho, was conducted in 2006. The study analyzed both yield and micronutrient concentrations, including minerals and vitamin C. Micronutrients were measured using spectrophotometry, and a completely randomized design with two replications was employed to ensure accuracy. The clones evaluated included early-maturing, intermediate, and late-maturing types, providing a diverse representation of advanced potato germplasm.
"

	uri <- "doi:10.21223/QTWOJV"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
		carob_effort = 0.15
	)

	# The Dataverse file list marks the observation workbooks restricted.
	# The raw folder only has documentation, so there is no table to standardize.
	stop("No observation file in data/raw/varieties_potato/doi_10.21223_QTWOJV. Files present: 05_Data_dictionary.xlsx")
}

