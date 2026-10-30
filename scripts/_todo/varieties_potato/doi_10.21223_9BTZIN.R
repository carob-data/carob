# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_Data_minerals_Illpa_2006.tab; 02_Data_VitaminC_Illpa_2006.tab; 03_Material_list_Illpa_2006.tab; TMIN200612_ILLPA_LNP.xls; TVIT200612_ILLPA_LNP.xls.

carob_script <- function(path) {

"
Dataset for: Micronutrient Concentration Assessment in Illpa, Puno - 2006

Twelve native local varieties were evaluated in the Illpa, Puno area during 2006 under a completely randomized design with two replications to determine the content of micronutrients and vitamin C.
"

	uri <- "doi:10.21223/9BTZIN"
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

