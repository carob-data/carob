# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Morphological  and ecogeographical characterization of potato wild relatives collected in Ecuador

A total of 91 accessions representing eight wild Solanum species (S. acaule, S. albicans, S. albornozii, S. andreanum, S. chilliasense, S.chomatophilum, S. colombianum and S. minutifoliolum) collected and preserved at the National Germplasm Bank, INIAP in Ecuador were comprehensive characterized involved the utilization of 39 morphological descriptors under controlled greenhouse conditions. The data collection spanned various developmental stages of the plants.
"

	uri <- "doi:10.21223/LUEKR7"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP; National Institute of Agricultural Research - INIAP",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Your Name",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "01_Fieldbook_data_Morphological characterization of wild potato_ECU.xlsx"]
	f2 <- ff[basename(ff) == "02_Material_list_Morphological characterization of wild potato_ECU.xlsx"]
	f3 <- ff[basename(ff) == "03_Material_List_summary_Morphological characterization of wild potato_ECU.xlsx"]
	f4 <- ff[basename(ff) == "04_Data_dictionary_Morphological characterization of wild potato_ECU.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
