# R script for "carob"


carob_script <- function(path) {

"Ethiopia: Socioeconomic Survey 2018-2019"

	up <- carobiner::usr_pwd(path, "LSMS")
	if (is.null(up)) return(TRUE)

	uri <- "doi:10.48529/k739-c548"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)
	if (length(ff) == 0) return(TRUE)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication = NA,
		carob_contributor = "Robert Hijmans",
		data_organization = "xxx;WB",
		carob_date = "2025-05-14",
		completion = 0
	)

	return(TRUE)
	carobiner::write_files(path, meta, d)
}



