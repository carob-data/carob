# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Advanced yield trial Set B in Ibadan using 20 accessions. 15 selected from 17.GS.C0.C1.AYT.58.IK,17.GS.C0.C1.AYT.58.IB and 17.GS.C0.C1.AYT.58.UB .Best 5 selected from each locations and five checks"
  
	uri <- "doi:10.25502/AJBQ-PA89"
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

