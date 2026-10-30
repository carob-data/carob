# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: Dictionary PGIS Vietnam VNM1.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Utilization of Participatory Mapping Tools for the Assessment of Crop Diversity in Vietnam

A total of 200 farmers in Vietnam, were surveyed utilizing ArcGIS Survey 123. Each farmer's field was physically visited, and the 'Participatory Mapping of Farmers' Fields' tool was employed to delineate polygons for each 'parcel', accurately determining their respective areas. This process captured the characteristics of 773 mapped plots across 200 farmers' households. The resultant comprehensive dataset comprises five distinct databases: A) Preliminary Information, B) Plot Information, C) Plot and Land Use, D) Crop Information, E) Variety Names. These datasets have been individualized and standardized using the common identifiers 'GlobalID Household' and 'GlobalID Plot'.
"

	uri <- "doi:10.21223/XW8A1D"
	group <- "survey"
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
	stop("No observation file in data/raw/survey/doi_10.21223_XW8A1D. Files present: Dictionary PGIS Vietnam VNM1.xlsx")
}

