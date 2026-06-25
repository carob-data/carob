# R script for "carob"


carob_script <- function(path) {
"This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Zimbabwe."

	uri <- "doi:10.25502/20180730/1608/MA"
	group <- "varieties_maize"	
	ff <- carobiner::get_data(uri, path, group)
		
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		carob_effort = NA,
		carob_date="2021-06-03",
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project="International Maize Trials",
		data_organization="IITA"
	)

	mzfun <- carobiner::get_function("intmztrial_striga", path, group)
	d <- mzfun(ff)

	d$location[d$location=='Harare1'] <- "Harare"
	d$longitude[d$location=='Harare'] <- 31.05
	d$latitude[d$location=='Harare'] <- -17.83
	d$description <- as.character(d$description)

	carobiner::write_files(meta, d, path=path)
}

