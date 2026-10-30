# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_Data_minerals_ANTA_Varieties_of_World_Cusco_2007.tab; 02_Data_VitaminC__ANTA_Varieties_of_World_Cusco_2007.tab; 03_Data_phenolic_ANTA_Varieties_of_World_Cusco_2007.tab; 04_Material_list_ANTA_Varieties_of_World_Cusco_2007.tab; TMIN200710_ANTA_Varieties_of_World_Cusco.xls; TPHE200710_ANTA_Varieties_of_World_Cusco.xls; TVIT200710_ANTA_Varieties_of_World_Cusco.xls.

carob_script <- function(path) {

"
Dataset for: Evaluation of Micronutrient and Polyphenol Concentrations in Globally Significant Potato Varieties from Cusco - 2007

Thirty-seven commercial potato varieties of global importance were evaluated for micronutrient concentrations in two locations in Peru: Cuzco and La Molina. The study was conducted at the INIA Andenes Station, and a genotype by environment (GxE) interaction analysis was performed. Processed samples were sent to WAS for mineral analysis, while the content of vitamin C and polyphenols was determined at CIP in Lima.
"

	uri <- "doi:10.21223/BV4LWI"
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

