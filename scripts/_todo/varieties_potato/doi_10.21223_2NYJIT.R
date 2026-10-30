# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Screening for resistance to late blight in Peruvian wild potato species - 2019

This data set includes information of late blight severity readings at 4 and 6 days after inoculation under greenhouse conditions, calculated values of Area Under Disease Progress Curve (AUDPC) and Relative Area Under Disease Progress Curve (rAUDPC) from experiments carried out in CIP-Lima during Jun to December 2019.
"

	uri <- "doi:10.21223/2NYJIT"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP; National University of the Center of Peru",
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
	

	f1 <- ff[basename(ff) == "01_Data_Screening for resistance to late blight in Peruvian wild potato species  2019.xlsx"]
	f2 <- ff[basename(ff) == "02_Material_list_Screening for resistance to late blight in Peruvian wild potato species  2019.xlsx"]
	f3 <- ff[basename(ff) == "03_Data_dictionary_Screening for resistance to late blight in Peruvian wild potato species 2019.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
