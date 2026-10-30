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
Dataset for: Temporal Dynamics of Potato landraces diversity in Paucartambo, Cusco

The historical native potato database includes records from 1927 to 2020 and was expanded with additional information collected in 2022, reaching a total of 15,257 observations. It also incorporates data from 335 farming families surveyed in 2022. The names of the landraces were previously standardized to ensure consistency across all sources. The study was conducted in the districts of Challabamba, Colquepata, and Paucartambo, in the province of Paucartambo (Cusco, Peru), an area ranging from 2,900 to 4,500 meters above sea level and characterized by high agroecological diversity ideal for native potato cultivation.
"

	uri <- "doi:10.21223/P3/UXWTSU"
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
	stop("No observation file in data/raw/varieties_potato/doi_10.21223_P3_UXWTSU. Files present: 02_Data_dictionary.xlsx")
}

