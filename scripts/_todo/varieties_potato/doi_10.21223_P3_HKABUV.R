# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Screening for resistance to late blight in Peruvian wild potato species

This data set includes information of late blight severity readings at 3 and 5 days after inoculation under greenhouse conditions, calculated values of Area Under Disease Progress Curve (AUDPC) and Relative Area Under Disease Progress Curve (rAUDPC)  from experiments carried out in CIP-Lima during May to November 2018. Vegetal material tested belonged to genotypes from wild potato landraces accessions hold in CIP's potato germplasm.
"

	uri <- "doi:10.21223/P3/HKABUV"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIP; National Agrarian University",
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
	

	f1 <- ff[basename(ff) == "Data_dictionary.xls"]
	f2 <- ff[basename(ff) == "Fieldbook.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="Material_list")
	r2b <- carobiner::read.excel(f2, sheet="Fieldbook")
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
