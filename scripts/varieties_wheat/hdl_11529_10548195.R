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
	
	d <- lapply(years, \(y) {
		f <- ff[grep(paste0("^", y), basename(ff))]
		proc_wheat(f)
	})
	
	d <- do.call(carobiner::bindr, d) 
	d$planting_date[d$planting_date == "92-93"] <- "1992"
	d$planting_date[d$planting_date == "99-00"] <- "1999"
	d$planting_date[d$planting_date == "00-01"] <- "2000"

	d$soil_pH[d$soil_pH==60] <- 6.0
		
		
	carobiner::write_files(path, meta, d)
}

	
	
