# R script for "carob"


carob_script <- function(path) {
"
This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one asian country.
"

	uri <- "doi:10.25502/20180830/1731/MA"
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
	
	
	#lonlat reversed
	i <- d$location == "Eruwa"
	d$longitude[i] <- 3.42
	d$latitude[i] <- 7.53
	# presumably also reversed
	i <- d$location == "Agwan-Millan"
	d$longitude[i] <- 4.48
	d$latitude[i] <-  12.54
	
	i <- d$location == "Angaradebou"
	d$country[i] <- "Benin"
	
	i <- d$location == "Ikorodu"
	d$longitude[i] <- 3.5
	d$latitude[i] <-  6.59
	
	carobiner::write_files(meta, d, path=path)
}
