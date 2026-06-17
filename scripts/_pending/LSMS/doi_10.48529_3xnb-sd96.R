# R script for "carob"


carob_script <- function(path) {

"Niger: National Survey on Household Living Conditions and Agriculture 2014, Wave 2 Panel Data"	

	if (is.null(carobiner::usr_pwd(path, "LSMS"))) return(TRUE)

	uri <- "doi:10.48529/3xnb-sd96"
	group <- "LSMS"
	ff  <- carobiner::get_data(uri, path, group, protocol="LSMS")

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



