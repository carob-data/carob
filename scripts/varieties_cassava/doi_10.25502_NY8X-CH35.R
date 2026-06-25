# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Advanced Yield Trial (15 clones) in Chitala, Zambia in 2016-17 breeding season"
  
	uri <- "doi:10.25502/NY8X-CH35"
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
	
	# data source has this in Malawi.
    country <- "Zambia"
	longitude <- 29.87
    latitude <- -14.27
    geo_from_source <- FALSE
	
	carobiner::write_files(path, meta, d$records, d$timerecs)
}

