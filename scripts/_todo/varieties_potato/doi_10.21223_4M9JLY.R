# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_Data_minerals_Anta_Cusco_2006.tab; 02_Data_VitaminC_Anta_Cusco_2006.tab; 03_Material_list_Anta_Cusco_2006.tab; TMIN200612_ANTA_Cusco_LNP_II.xls; TVIT200612_ANTA_Cusco_LNP_II.xls.

carob_script <- function(path) {

"
Dataset for: Micronutrient and Vitamin C Analysis of Native Varieties from Anta, Cusco  - 2006

Forty-one native local varieties from Anta, Cusco were evaluated in 2006 under a completely randomized design. Spectrophotometry was used, with two replications, to determine the content of micronutrients and vitamin C.
"

	uri <- "doi:10.21223/4M9JLY"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	# No measurement table is available in the raw folder.
	return(FALSE)
}

