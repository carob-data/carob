# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: National Performance Trials and Durability, Uniformity and Stability data collected to support release of 2 LB resistant  varieties in Kenya

The results of National Performance trials in Kenya. It holds the average of 2 seasons: April-August 2015 and October 2015 to February 2016.
"

	uri <- "doi:10.21223/W5SVEQ"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
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
	

	f1 <- ff[basename(ff) == "4415_Data.xlsx"]
	f2 <- ff[basename(ff) == "4415_DataDictionary_ElementDescription.xlsx"]
	f3 <- ff[basename(ff) == "4415_DataDictionary_UniqueIdentifier.xlsx"]
	f4 <- ff[basename(ff) == "4415_Material_List.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
