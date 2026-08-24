# R script for "carob"
# license: GPL (>=3)

## ISSUES
## NO response data

carob_script <- function(path) {

"
Clonal Evaluation trial (231 clones) in Chitedze, Malawi  2012-13 Breeding Season.

Clonal Evaluation trial (231 clones) in Chitedze, Malawi  2012-13 Breeding Season.
"

	uri <- "doi:10.25502/darh-nn48"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
  	data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "variety",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-22",
		carob_completion = 100,	
		carob_effort = 1
	)
	
	process_cassava <- carobiner::get_function("process_cassava", path, group)
	d <- process_cassava(ff)
	carobiner::write_files(path, meta, d$records, d$timerecs)

}


