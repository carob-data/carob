# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has a dictionary and the dataset metadata. The measurement workbook is marked restricted and was not extracted.
# Files on disk: 04_Data_dictionary_VHT.xlsx, doi_10.21223_A4KSZZ.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Performance and Physiological Analysis of Heat-Tolerant Virus (VHT) Potato Clones in San Ramón, 2012

A study was conducted in San Ramón, Chanchamayo, in 2012 to evaluate the yield performance of 641 clones from the heat-tolerant (VHT) population. The trial included two replicates and a physiological analysis of canopy reflectance and chlorophyll content. These evaluations were performed under field conditions in this highland region.
"

	uri <- "doi:10.21223/A4KSZZ"
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
