# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has the data dictionary only. The soil and agronomic workbooks are marked restricted and were not extracted.
# Files on disk: 3_Data_dictionary.xlsx, doi_10.21223_7YOJIL.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Potato production under zero tillage with straw mulch during 2023-2024 season in agronomic trials in the Khulna district, Bangladesh (Soil analysis and agronomic data)

Experimental trials were conducted under a strip-plot design in the locations of Fultola, Moshamari and Hogalbunia of Khulna district in Bangladesh by GIZ-PZTM project. Two varieties of potato (BARI Alu 72 and BARI Alu 7) under zero tillage with straw mulch (ZTSM) and planting date (1 and 2) treatments were tested. This dataset corresponds to the results of yield (marketable tuber yield and fresh tuber yield), plant height, number of steam per plant and soil analysis (characterization, fertility, salinity and carbon organic) of the samples collected at the end season December 2023 to March 2024.
"

	uri <- "doi:10.21223/7YOJIL"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "strip-plot",
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
