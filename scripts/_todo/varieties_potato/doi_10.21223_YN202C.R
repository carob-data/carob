# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Open field assessment for late blight evaluation in HERLB population (First trial_Ecuador)

29 families  potato  were exposed to late blight under natural infection in Quito, Eucador (3050 masl Quito-Ecuador) during the rainy season in 2023. Statistical Augmented row-column design without replication of 3 hill plots was properly established, three varieties with known late blight response were included as controls. After 40 days after planting (DAP), the percentage of leaf area affected by late blight infection was recorded by plot throughout the season to subsequently compute the area under the disease progress curve (AUDPC).
"

	uri <- "doi:10.21223/YN202C"
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
	

	f1 <- ff[basename(ff) == "01_Fieldbook_HERLB_ECU_2023_Lateblight_StaCatalina.xlsx"]
	f2 <- ff[basename(ff) == "02_Material list_HERLB_ECU_2023_Lateblight_StaCatalina.xlsx"]
	f3 <- ff[basename(ff) == "03_Crop managment_HERLB_ECU_2023_Lateblight_StaCatalina.xlsx"]
	f4 <- ff[basename(ff) == "04_Data_dictionary_HERLB_ECU_2023_Lateblight_StaCatalina.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
