# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: 03_Data_dictionary_Junin_Phureja_Collection.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Carotenoids Content in Native Potato Varieties, Junin 2009

In 2009, a study was conducted on six varieties of the phureja population in Huancavelica, focusing on carotenoid analysis. The research involved two treatments: cooked and raw samples.
"

	uri <- "doi:10.21223/LZIEMK"
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
	stop("No observation file in data/raw/varieties_potato/doi_10.21223_LZIEMK. Files present: 03_Data_dictionary_Junin_Phureja_Collection.xlsx")
}

