0# R script for "carob"

carob_script <- function(path) {

"International Durum Yield Nurseries are replicated yield trials designed to measure the yield potential and adaptation of superior CIMMYT-bred spring durum wheat germplasm that have been developed from tests conducted under irrigation and induced stressed cropping conditions in northwest Mexico.  These materials have been subjected to numerous diseases (leaf, stem and yellow rust; Septoria tritici blotch) and varied growing environments. It is distributed to 70 locations, and contains 50 entries. (2017)"

	uri <- "hdl:11529/10790"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=2,
		project="International Durum Yield Nursery",
		publication=NA,
		data_organization = "CIMMYT",
   		data_type="experiment", 
		response_vars = "yield",
		treatment_vars = "variety_code",	
		carob_contributor="Blessing Dzuda",
		carob_effort = NA,
		carob_date="2024-02-07"
	)
	
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)	
	d$crop <- "durum wheat"
	
	carobiner::write_files(path, meta, d)
}

