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
Dataset for: Characterization database of 68 genotypes of native potatoes of the Phurejas group from the Cumbal Indigenous Reservation - 2024

Since 2023, a participatory process with the Indigenous Pasto People in Cumbal (Nariño, Colombia) has supported the conservation of Phureja native potato landraces ('chauchas'). Through Five-Cell workshops and Red Listing exercises, 68 local varieties were identified and collected. Seed multiplication plots were established in early 2024, followed by three morphological characterization plots during 2024–2025 with local custodians and the Cumbe Indigenous Agricultural School. Using CIP standardized descriptors (Gómez, 2000), the dataset records 20 morphological traits related to plant, stem, flower, and tuber characteristics, supported by a systematic photographic record for the local variety catalogue.
"

	uri <- "doi:10.21223/OV8FUE"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	stop("Measurement files are restricted and are not in the raw folder. Only documentation was available, so no data table was built.")
}

