# R script for "carob"



carob_script <- function(path) {
"
This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one asian country.
"
				
	uri <- "doi:10.25502/20180730/1501/MA"
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
	

	i <- d$location == "Mlingano"
	d$longitude[i] <- 38.879
	d$latitude[i] <- -5.107

#Tanzania Agricultural Research Institute TARI-IFAKARA (KATRIN). Ifakara. Morogoro-Tanzania. 
	i <- d$location == "Katrin"
	d$longitude[i] <- 36.684
	d$latitude[i] <- -8.139
	
	carobiner::write_files(meta, d, path=path)

}
