# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: 03_Data_dictionary.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Screening and Yield of Virus Heat-Tolerant (VHT) Potato Clones in Junín, 2013

A total of 352 clones from the Virus Heat Tolerant (VHT) population were analyzed in the San Ramón region, Junín, during 2013 to evaluate their yield. These clones were selected from the 2012 LTVR population campaign and screened to determine their tolerance to heat.
"

	uri <- "doi:10.21223/H7BDBC"
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
	stop("No observation file in data/raw/varieties_potato/doi_10.21223_H7BDBC. Files present: 03_Data_dictionary.xlsx")
}

