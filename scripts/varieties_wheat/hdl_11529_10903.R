# R script for "carob"

carob_script <- function(path) {

"International Durum Yield Nurseries are replicated yield trials designed to measure the yield potential and adaptation of superior CIMMYT-bred spring durum wheat germplasm that have been developed from tests conducted under irrigation and induced stressed cropping conditions in northwest Mexico. These materials have been subjected to numerous diseases (leaf, stem and yellow rust; Septoria tritici blotch) and varied growing environments. It is distributed to 70 locations, and contains 50 entries. (2002)"

	uri <- "hdl:11529/10903"
	group <- "varieties_wheat"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=2,
		data_organization = "CIMMYT",
		publication= NA,
		project="International Durum Yield Nursery",
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety_code",
		carob_contributor= "Robert Hijmans",
		carob_effort = NA,
		carob_date="2024-03-26"
	)

	sets <- gsub("_RawData.xls", "", grep("RawData", basename(ff), value=TRUE))

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)

	d <- vector("list", length(sets))
	for (i in seq_along(sets)) {
		fs <- grep(sets[i], ff, value=TRUE)
		d[[i]] <- proc_wheat(fs)
	}

	dd <- do.call(carobiner::bindr, d)
	dd$crop <- "durum wheat"
	dd$soil_pH[dd$soil_pH < 2] <- NA

	pds <- paste0(81:96, "-", 82:97)
	rpl <- 1981:1996
	for (i in 1:length(pds)) dd$planting_date <- gsub(pds[i], rpl[i], dd$planting_date)


	carobiner::write_files(path, meta, dd)
}


