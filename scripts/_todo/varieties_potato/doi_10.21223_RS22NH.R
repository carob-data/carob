# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: 04_Data_dictionary_VHT.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Performance and Physiological Analysis of Heat-Tolerant Virus (VHT) Potato Clones in Aco, Pasco 2012

A study was conducted in Aco, Paucartambo, Pasco, in 2012 to evaluate the yield performance of 600 clones from the heat-tolerant (VHT) population and three potato varieties. The trial included two replicates and a physiological analysis of canopy reflectance and chlorophyll content. These evaluations were performed under field conditions in this highland region.
"

	uri <- "doi:10.21223/RS22NH"
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
	stop("No observation file in data/raw/varieties_potato/doi_10.21223_RS22NH. Files present: 04_Data_dictionary_VHT.xlsx")
}

