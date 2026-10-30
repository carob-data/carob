# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_Data_minerals_Quilca_Junin_2006.tab; 02_Data_VitaminC_Quilca_Junin_2006.tab; 03_Material_list_Quilca_Junin_2006.tab; TMIN200610_QUILCA_LNP.xls; TVIT200610_QUILCA_LNP.xls.

carob_script <- function(path) {

"
Dataset for: Micronutrient and Vitamin C Analysis of Native Varieties from Huancavelica - 2006

In 2006, 17 samples of native potato varieties from Quilca, Junín were analyzed to assess micronutrient and vitamin C concentrations. The nutrient analysis was performed using spectrophotometry. The study followed a completely randomized design (CRD) with two replications to accurately determine the nutrient content in these varieties.
"

	uri <- "doi:10.21223/F4P74D"
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

