# R script for "carob"



carob_script <- function(path) {
"This is an international study that contains data on Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) in Africa. The study was carried out by the International Institute of Tropical Agriculture between 2008 and 2016 in twelve African countries."

	uri <- "doi:10.25502/20180925/1906/BB"
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
	d <- mzfun(ff, sf="Nigeria.csv")	

	d$yield[d$yield > 20000] <- NA

	#lonlat reversed
	i <- d$location == "Eruwa"
	d$longitude[i] <- 3.42
	d$latitude[i] <- 7.53
	
	i <- d$location == "Zaria"
	d$longitude[i] <- 7.717
	d$latitude[i] <- 11.131

	i <- d$location == "Tofa"
	d$longitude[i] <- 8.27
	d$latitude[i] <- 12.06 
	
	i <- d$location == "Mokwa"
	d$longitude[i] <- 5.06
	d$latitude[i] <- 9.29

	i <- d$location == "Zuru"
	d$longitude[i] <- 5.219
	d$latitude[i] <- 11.432

	i <- d$location=="Ikenne"
	d$longitude[i] <- 3.698
	d$latitude[i] <- 6.901

	i <- d$location == "Bagauda"
	d$longitude[i] <- 8.385
	d$latitude[i] <- 11.570
	
	i <- d$location == "Ejiba"
	d$longitude[i] <- 5.640
	d$latitude[i] <- 8.298

	i <- d$location == "Tamba Daura"
	d$longitude[i] <- 8.311
	d$latitude[i] <- 13.016
		
	i <- d$location == "Angaradebou"
	d$country[i] <- "Benin"
	
	d <- d[!is.na(d$yield), ]

	carobiner::write_files(meta, d, path=path)
}
