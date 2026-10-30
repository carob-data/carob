# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: Data_dictionary.xls.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Potential new bio-fortified potato varieties high in iron and zinc to contribute to food and nutrition security in Cusco, Peru (Participatory Varietal Selection).

A select group of 30 tetraploid biofortified clones from cycle I will be evaluated using a farmer participatory variety selection (PVS) in the highland of Cusco during the campaign 2018-2019; yield components, micronutrients content (Fe & Zn) and post-harvest traits will be determinated, with the main purpose  to improve the food security and nutrition of families in situation of vulnerability through the identification of 3-5 potential new biofortified potatoes varieties with  25% of micronutrient concentration (Fe) above the local varieties
"

	uri <- "doi:10.21223/P3/QUIKWK"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
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
	stop("No observation file in data/raw/varieties_potato/doi_10.21223_P3_QUIKWK. Files present: Data_dictionary.xls")
}

