# R script for "carob"

carob_script <- function(path) {
  
"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Screening Nursery (HRWSN) contains spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall areas (Mega-environment 2). (2017)"
  
    
	uri <- "hdl:11529/10548249"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		project="High Rainfall Wheat Screening Nursery",		 
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

	years <- gsub(".xlsx", "", grep("HRWSN.xlsx", basename(ff), value=TRUE))
	
	maxid <- 0
	dd <- lapply(years, \(y) {
		f <- ff[grep(paste0("^", y), basename(ff))]
		d <- proc_wheat(f)
		d$wide$record_id <- d$wide$record_id + maxid
		d$long$record_id <- d$long$record_id + maxid
		maxid <<- max(d$wide$record_id)		
		d		
	})
	
	dlong <- do.call(carobiner::bindr, lapply(dd, \(x) x[["long"]]))
	dwide <- do.call(carobiner::bindr, lapply(dd, \(x) x[["wide"]]))
	dlong$record_id <- as.integer(dlong$record_id)
	dwide$record_id <- as.integer(dwide$record_id)
	d <- list(long=dlong, wide=dwide)
	
	d$wide$emergence_date[d$wide$emergence_date == "2026-08-26"] <- "2001-08-26"
		
	d$wide$planting_date[d$wide$planting_date == "94-95"] <- "1994"
	d$wide$planting_date[d$wide$planting_date == "96-97"] <- "1996"
	d$wide$planting_date[d$wide$planting_date == "98-99"] <- "1998"
	d$wide$planting_date[d$wide$planting_date == "00-01"] <- "2000"
		
	carobiner::write_files(path, meta, d$wide, d$long)
}

	
	
