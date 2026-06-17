# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Assessment of twelve clones selected from 2017 pyt14 yellow root trial, for mealiness in Ibadan from 2017ib.pyt24yrtpd IB."
  
	uri <- "doi:10.25502/63V0-5P22"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)
		
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Robert Hijmans",
		carob_effort = NA,
		carob_date = "2024-09-18",
		notes = NA
	)

	process_cassava <- carobiner::get_function("process_cassava", path, group)
	d <- process_cassava(ff)
	carobiner::write_files(path, meta, d$records, d$timerecs)
}

