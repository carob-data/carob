# R script for "carob"



carob_script <- function(path) {
"This is an international study that contains data on Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) in Africa. The study was carried out by the International Institute of Tropical Agriculture between 2008 and 2016 in twelve African countries."

	uri <- "doi:10.25502/20180925/1624/BB"
	group <- "varieties_maize"	
	ff <- carobiner::get_data(uri, path, group)
		
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
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
	f <- grep("\\.csv$", basename(ff), value=TRUE)
	d <- mzfun(ff, sf=f)	
	d$country <- "Democratic Republic of the Congo"
	carobiner::write_files(meta, d, path=path)
}
