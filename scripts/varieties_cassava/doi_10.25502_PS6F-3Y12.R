# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Genomic selection trial from C1, C2, C3 in an advanced yield trial in Ubiaja for 2019-2020 breeding season selected from gensel.c1.uyt26.uyt26.pyt60&ayt28.AG"
  
	uri <- "doi:10.25502/PS6F-3Y12"
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

