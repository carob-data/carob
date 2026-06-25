# R script for "carob"


carob_script <- function(path) {

"Mali: Enquête Agricole de Conjoncture Intégrée aux Conditions de Vie des Ménages 2017"	

	if (is.null(carobiner::usr_pwd(path, "LSMS"))) return(TRUE)

	uri <- "doi:10.48529/e39g-m952"

	group <- "LSMS"
	ff  <- carobiner::get_data(uri, path, group, protocol="LSMS")
	if (is.null(ff)) {
		stop("these files need to be downloaded by hand")
	}

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication = NA,
		carob_contributor = "Robert Hijmans",
		data_organization = "xxx;WB",
		data_citation = "",		
		carob_completion = 0,
		carob_effort = NA,
		carob_date = "2025-05-14"
	)

	return(TRUE)
	carobiner::write_files(path, meta, d)
}



