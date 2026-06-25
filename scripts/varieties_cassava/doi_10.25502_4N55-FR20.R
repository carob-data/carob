# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Assessment of poundable Varieties of Cassava for  high yield, high dry matter, and disease resistance using Advanced Yield Trial (12 clones) in Ibadan 2019/2020 Breeding Season from 2018ayt12pd.IB"
  
	uri <- "doi:10.25502/4N55-FR20"
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

