# R script for "carob"

carob_script <- function(path) {
  
"The Elite Selection Wheat Yield Trial (ESWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents the optimally irrigated, low rainfall areas. Major stresses include leaf, stem and yellow rusts, Karnal bunt, and lodging. Representative areas include the Gangetic Valley (India), the Indus Valley (Pakistan), the Nile Valley (Egypt), irrigated river valleys in parts of China (e.g. Chengdu), and the Yaqui Valley (Mexico). This ME encompasses 36 million hectares spread primarily over Asia and Africa between 350S -350N latitudes. White (amber)-grained types are preferred by consumers of wheat in the vast majority of the areas. It is distributed to upto 200 locations and contains 50 entries. (2002)"
  
    
	uri <- "hdl:11529/10893"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=0,
		project="Elite Selection Wheat Yield Trial",		 
		publication=NA,
		data_organization = "CIMMYT",
		carob_contributor="Robert Hijmans",
		carob_date="2024-07-14",
		data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code"
	)
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)

	years <- sapply(grep("RawData.xls", basename(ff), value=TRUE), \(i) strsplit(i, " ")[[1]][1], USE.NAMES=F)
	
	d <- lapply(years, \(y) {
		f <- ff[grep(paste0("^", y), basename(ff))]
		proc_wheat(f)
	})

	d <- do.call(carobiner::bindr, d) 

	i <- grep("^00-..$", d$planting_date)
	d$planting_date[i] <- "2000"
	i <- grep("^..-..$", d$planting_date)
	d$planting_date[i] <- paste0("19", substr(d$planting_date[i],1, 2))

	i <- which(d$soil_pH > 50)
	d$soil_pH[i] <- d$soil_pH[i] / 10
	
	carobiner::write_files(path, meta, d)
}

