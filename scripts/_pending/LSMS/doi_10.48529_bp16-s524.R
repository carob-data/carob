# R script for "carob"


carob_script <- function(path) {

"Niger: National Survey on Household Living Conditions and Agriculture 2011"	

	up <- carobiner::usr_pwd(path, "LSMS")
	if (is.null(up)) return(TRUE)	

	uri <- "doi:10.48529/bp16-s524"
	group <- "LSMS"
	ff  <- carobiner::get_data(uri, path, group, protocol="LSMS", username=up$username, password=up$password)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication = NA,
		carob_contributor = "Robert Hijmans",
		data_organization = "xxx;WB",
		data_citation = "",		
		completion = 0,
		carob_date = "2025-05-14"
	)

	return(TRUE)
	carobiner::write_files(path, meta, d)
}



