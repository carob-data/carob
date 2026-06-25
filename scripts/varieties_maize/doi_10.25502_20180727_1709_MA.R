# R script for "carob"


# border tbd

carob_script <- function(path) {
"
This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Mali."
				
	uri <- "doi:10.25502/20180727/1709/MA"
	group <- "varieties_maize"	
	ff <- carobiner::get_data(uri, path, group)
		

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Robert Hijmans",
		carob_effort = NA,
		carob_date="2023-07-03",
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project="International Maize Trials",
		data_organization="IITA"
	)


	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff)
	d$country <- "Democratic Republic of the Congo"
	
	
	i <- d$location == "Gandajika"
	d$longitude[i] <- 23.95
	d$latitude[i] <- -6.750
	
	i <- d$location == "Kipopo"
	d$longitude[i] <- 17.83
	d$latitude[i] <- -4.970
	
	i <- d$location == "Mangongo"
	d$longitude[i] <- 23.744
	d$latitude[i] <- 0.9257

	i <- d$location == "Kalemba"
	d$longitude[i] <- 22.217
	d$latitude[i] <- -6.867
	
	i <- d$location == "Kashila"
	#Kashile
	d$longitude[i] <- 21.88 
	d$latitude[i] <- -5.17
	
	i <- d$location == "M'vuazi"
	d$longitude[i] <- 14.886
	d$latitude[i] <- -5.439
	
	# Mankewa, (alluvial area of M'vuazi river) IITA 1978 annual report
	i <- d$location == "Mankewa"
	d$longitude[i] <- 14.886
	d$latitude[i] <- -5.439


	carobiner::write_files(meta, d, path=path)
}
