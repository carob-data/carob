# R script for "carob"


carob_script <- function(path) {
"
Grain yield and other agronomic traits of international maize trials-Camerron 1989-2002
Description: This is an international study that contains data on yield and other Agronomic
traits of maize including borer and striga attacks on maize in Africa. 
The study was carried out by the International Institute of Tropical Agriculture between 1989
and 2015 in over thirty African countries. This dataset contains output of the research for Cameroon.
"

	uri <- "doi:10.25502/20180716/0907/MA"
	group <- "varieties_maize"	
	ff <- carobiner::get_data(uri, path, group)
		

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		carob_effort = NA,
		carob_date="2021-06-02",
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project="International Maize Trials",
		data_organization="IITA"
	)


	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff)
	
	
	d$location[	d$location == "Cameroon"] <- NA
	d$description <- as.character(d$description)

	i <- which(d$location == "Mayo-Galke") 
	d$longitude[i] <- 14.2421
	d$latitude[i] <- 8.3868
	d$location[i] <- "Mayo-Galké"

	i <- which(d$location == "Nfonta") 
	d$longitude[i] <- 10.271
	d$latitude[i] <- 6.0178

	i <- which(d$location == "Djallingo") 
	d$longitude[i] <- 13.45833
	d$latitude[i] <- 9.245833


	carobiner::write_files(meta, d, path=path)
}
