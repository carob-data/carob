# R script for "carob"



carob_script <- function(path) {
"This is an international study that contains data on Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) in Africa. The study was carried out by the International Institute of Tropical Agriculture between 2008 and 2016 in twelve African countries."


	uri <- "doi:10.25502/20180925/1812/BB"
	group <- "varieties_maize"	
	ff <- carobiner::get_data(uri, path, group)
		

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		## the source says "Gambia" but it is "Ghana", fixed
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Robert Hijmans",
		carob_date="2023-07-03",
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project="International Maize Trials",
		data_organization="IITA"
	)



	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff, sf="Ghana.csv")
		
	
	i <- d$location == "Babile"
	d$longitude[i] <- -2.83
	i <- d$location == "Wa"
	d$longitude[i] <- -2.5
	i <- d$location == "Manga"
	d$longitude[i] <- -0.16
	
	carobiner::write_files(meta, d, path=path)

}
