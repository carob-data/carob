# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Assessment of Yellow root varieties of Cassava for high yield, high dry matter and disease resistance using NCRP Trial (12 clones) in Mokwa 2019/2020 Breeding Season from 2018ikn(ayt18htc, ayt12htc and ayt20htc).MK"
  
	uri <- "doi:10.25502/87DD-2K16"
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

