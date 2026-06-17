# R script for "carob"



carob_script <- function(path) {
"
This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one asian country.
"
	uri <- "doi:10.25502/20180730/1112/MA"
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
	
	
	i <- d$location == "Mokwa"
	d$longitude[i] <- 5.06
	d$latitude[i] <- 9.29

	i <- d$location == "Kaduna"
	d$longitude[i] <- 7.45
	d$latitude[i] <- 10.6

	i <- d$location == "Dansadau"
	d$longitude[i] <- 8
	d$latitude[i] <- 10

	i <- d$location == "Ikorodu"
	d$longitude[i] <- 3.50
	d$latitude[i] <- 6.59

	i <- grep("Gusau", d$location)
	d$longitude[i] <- 7.08
	d$latitude[i] <- 12.03

	i <- d$location == "Kilissi"
	d$country[i] <- "Guinea"
	d$longitude[i] <- -12.6 
	d$latitude[i] <- 9.64 

	i <- d$location == "Sinematiali"
	d$country[i] <- "Côte d'Ivoire"
	
	i <- d$location == "Niaouli"
	d$country[i] <- "Benin"
	
	i <- d$location == "Touboro"
	d$country[i] <- "Cameroon"
	
	i <- d$location == "Rubona"
	d$country[i] <- "Uganda"
	
	carobiner::write_files(meta, d, path=path)

}
