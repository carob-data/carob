# R script for "carob"
# license: GPLv3

carob_script <- function(path) {
  
"The Cowpea Breeding Unit develops improved cowpea lines with high grain yield potential and resistance/tolerance to biotic and abiotic stresses and possessing farmers and consumers preferences. Data generated from multi-year evaluation trials carried out on different sets of materials developed within several projects conducted by the cowpea breeding unit are contained in the database."
  
	uri <- "doi:10.25502/20180920/1052/BO"
	group <- "varieties_cowpea"
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
		carob_date = "2024-09-20",
		notes = NA
	)

	process_cowpea <- carobiner::get_function("process_cowpea", path, group)
	d <- process_cowpea(ff)
	d$location <- "Lagos"
	carobiner::write_files(path = path, metadata = meta, wide=d)
}

