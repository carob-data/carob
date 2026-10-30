# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has a dictionary and the dataset metadata. The measurement workbook is marked restricted and was not extracted.
# Files on disk: 03_Data_dictionary_Junin_Phureja_2009.xlsx, doi_10.21223_67YAFJ.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Phenolic Content Analysis in Potato Varieties in Junin, 2009

In 2008, an analysis was conducted on six potato varieties in Comas, Junin. The study included two treatments: cooked and raw samples, with and without peel, using a 2x2 factorial design. Phenolic content was analyzed on both dry and fresh weight bases.
"

	uri <- "doi:10.21223/67YAFJ"
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
		carob_effort = 0.1
	)

	message("No measurement table in the local raw files for ", uri)
	invisible(meta)
}
