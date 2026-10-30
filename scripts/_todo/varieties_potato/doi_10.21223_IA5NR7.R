# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: 02_Data_dictionary.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Evaluation of the Morphological Characterization of Native Potato Varieties in Cochacochayocc, Paucartambo - Cusco, 2024

In the Cochacochayoq field, Paucartambo, 80 native potato varieties were planted using zero-tillage practices. Agronomic management activities included Yapuy (maintenance), Jaimay (first earthing-up), and Qasuy (final maintenance). The experiment evaluates the morphological characteristics of native potatoes, including tuber traits and flower attributes. This study aims to assess the agronomic performance of these varieties under sustainable conditions, focusing on conserving genetic diversity and improving agricultural practices in the Paucartambo region.
"

	uri <- "doi:10.21223/IA5NR7"
	group <- "varieties_potato"
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
		carob_effort = 0.15
	)

	# The Dataverse file list marks the observation workbooks restricted.
	# The raw folder only has documentation, so there is no table to standardize.
	stop("No observation file in data/raw/varieties_potato/doi_10.21223_IA5NR7. Files present: 02_Data_dictionary.xlsx")
}

