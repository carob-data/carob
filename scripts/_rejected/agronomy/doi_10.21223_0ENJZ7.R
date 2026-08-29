# REJECTED 
# not field data. only derived parameters that cannot be interpreted.

# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: BLUPs predicted from combined analyses across different trials and locations, Kenya 2022

Collection of BLUPs predicted across potato yield traits combining a set of trials. It was considered trials with clones at advanced and intermediate stages planted and harvested in 2022 in Kenya. The trials were planted in regions representing the major potato-growing areas in Kenya.
"

	uri <- "doi:10.21223/0ENJZ7"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = "",
		project = NA,
		carob_date = "2026-08-29",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Robert Hijmans",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "01_data_Blup_Overall.xlsx"]
	f2 <- ff[basename(ff) == "02_h2_by_trial.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
