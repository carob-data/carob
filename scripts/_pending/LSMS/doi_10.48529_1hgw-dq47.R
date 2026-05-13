# R script for "carob"


carob_script <- function(path) {

"Nigeria: General Household Survey, Panel 2018-2019, Wave 4"

	if (is.null(carobiner::usr_pwd(path, "LSMS"))) return(TRUE)

	uri <- "doi:10.48529/1hgw-dq47"
	group <- "LSMS"
	ff  <- carobiner::get_data(uri, path, group, protocol="LSMS")
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		carob_contributor = "Robert Hijmans",
		carob_date = "2025-05-14",
		data_organization = "xxx;WB",
		data_citation = "",		
		completion = 0
	)
	
	
	hg <- read.csv(ff[basename(ff) == "nga_householdgeovars_y4.csv"])

	geo <- data.frame(
		hhid = hg$hhid,
		latitude = hg$lat_dd_mod,
		longitude = hg$lon_dd_mod
	)

	p1 <- read.csv(ff[basename(ff) == "sect1_plantingw4.csv"])




	return(TRUE)
	carobiner::write_files(path, meta, d)
}



