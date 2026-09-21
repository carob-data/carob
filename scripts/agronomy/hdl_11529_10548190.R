# REJECTED 
# Reason: get_data() returned no files
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# data could not be processed as the get_data() function returned no files


carob_script <- function(path) {

"
Replication Data for: Nonlinear heat effects on African maize as evidenced by historical yield trials

This dataset provides supplementary files for the trials and sites described in the 2011 paper in https://www.nature.com/nclimate/ available at https://dx.doi.org/10.1038/NCLIMATE1043
"

	uri <- "hdl:11529/10548190"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "BAU; CIMMYT",
		publication = "",
		project = NA,
		carob_date = "2026-09-21",
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
	




	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
