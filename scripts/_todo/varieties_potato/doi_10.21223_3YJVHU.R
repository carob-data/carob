# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: 03_Data_dictionary_minerals_201008_LAVICT_LD.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Micronutrient Concentration in Advanced Potato Clones in Junín, 2010

An evaluation was conducted on 32 advanced clones of the LTVR population under long-day (LD) conditions to assess mineral concentrations. The samples were sourced from the Huancayo region in 2010 and were cultivated using a randomized complete block design (RCBD) with two replications. This study aimed to analyze the mineral profiles of these clones to support breeding efforts and enhance their nutritional value.
"

	uri <- "doi:10.21223/3YJVHU"
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
	stop("No observation file in data/raw/varieties_potato/doi_10.21223_3YJVHU. Files present: 03_Data_dictionary_minerals_201008_LAVICT_LD.xlsx")
}

