# R script for "carob"


carob_script <- function(path) {
  
"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2017)"
  
    
	uri <- "hdl:11529/10548195"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=3,
		project="High Rainfall Wheat Yield Trial",		 
		publication=NA,
		data_organization = "CIMMYT",
		carob_contributor="Robert Hijmans",
		carob_effort = NA,
		carob_date="2024-07-14",
		data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code"
	)
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	years <- gsub(".xlsx", "", grep("HRWYT.xlsx", basename(ff), value=TRUE))
	
	maxid <- 0
	x <- lapply(years, \(y) {
		f <- ff[grep(paste0("^", y), basename(ff))]
		d <- proc_wheat(f)
		d$wide$record_id <- d$wide$record_id + maxid
		d$long$record_id <- d$long$record_id + maxid
		maxid <<- max(d$wide$record_id)
		d
	})
	
	dlong <- do.call(carobiner::bindr, lapply(x, \(d) d[["long"]]))
	dwide <- do.call(carobiner::bindr, lapply(x, \(d) d[["wide"]]))
	dlong$record_id <- as.integer(dlong$record_id)
	dwide$record_id <- as.integer(dwide$record_id)
	
	d <- list(long=dlong, wide=dwide)
	
	d$wide$planting_date[d$wide$planting_date == "92-93"] <- "1992"
	d$wide$planting_date[d$wide$planting_date == "99-00"] <- "1999"
	d$wide$planting_date[d$wide$planting_date == "00-01"] <- "2000"

	d$wide$soil_pH[d$soil_pH==60] <- 6.0
	
	carobiner::write_files(path, meta, d$wide, d$long)
}


	
	
