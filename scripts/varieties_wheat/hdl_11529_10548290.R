# R script for "carob"

carob_script <- function(path) {

"International Durum Yield Nurseries are replicated yield trials designed to measure the yield potential and adaptation of superior CIMMYT-bred spring durum wheat germplasm that have been developed from tests conducted under irrigation and induced stressed cropping conditions in northwest Mexico. These materials have been subjected to numerous diseases (leaf, stem and yellow rust; Septoria tritici blotch) and varied growing environments. It is distributed to 70 locations, and contains 50 entries. (2007)"

	uri <- "hdl:11529/10548290"
	group <- "varieties_wheat"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		project="International Durum Yield Nursery",
		publication=NA,
		data_organization = "CIMMYT",
   		data_type="experiment", 
		response_vars = "yield",
		treatment_vars = "variety_code",
		carob_contributor="Blessing Dzuda",
		carob_date="2024-02-01"
	)
		
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$crop <- "durum wheat"
	
	#removing out of bound value
	d$soil_pH[d$soil_pH == 24] <- NA
	d$soil_pH[d$soil_pH == 0] <- NA

	carobiner::write_files(path, meta, d)
}

