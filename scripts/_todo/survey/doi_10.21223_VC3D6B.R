# R script for "carob"
# license: GPL (>=3)

## NOTES
# The measurement files for this record are restricted on Dataverse. The raw folder only has the unrestricted documentation (dictionary and, in some cases, a questionnaire). There are no observation rows to standardize.

## ISSUES
# No data table. The restricted measurement files were not downloaded.
# The script stops before write_files because there is no table.

## NOT PROCESSED
# The dictionary or questionnaire that is on disk describes columns that are not in the downloaded files.


carob_script <- function(path) {

"
Dataset for: Utilization of Participatory Mapping Tools for the Assessment of Crop Diversity in Cumbal, Colombia

A total of 206 farmers in Cumbal, Colombia, were surveyed utilizing ArcGIS Survey 123. Each farmer's field was physically visited, and the 'Participatory Mapping of Farmers' Fields' tool was employed to delineate polygons for each 'shagra', accurately determining their respective areas. This process captured the characteristics of 557 mapped plots across 206 farmers' households. The resultant comprehensive dataset comprises five distinct databases: A) Preliminary Information, B) Shagra Information, C) Land Use Over the Years, and D) Variety Names. These datasets have been individualized and standardized using the common identifiers 'GlobalID Household' and 'GlobalID Shagra'.
"

	uri <- "doi:10.21223/VC3D6B"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	stop("Measurement files are restricted and are not in the raw folder. Only documentation was available, so no data table was built.")
}

