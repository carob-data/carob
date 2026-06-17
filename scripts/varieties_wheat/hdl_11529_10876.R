# R script for "carob"

carob_script <- function(path) {

"The Semi-Arid Wheat Yield Trial (SAWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to low rainfall, drought prone environments typically receiving less than 500 mm of water available during the cropping cycle. The combination of water-use efficiency and water responsive broad adaptation plus yield potential is important in drought environments where rainfall is frequently erratic across and within years. Stripe rust, leaf rust and stem rust, root rots, nematodes, and bunts are the key biotic constraints. Typical target environments include winter rain or Mediterranean-type drought associated with post-flowering moisture stress and heat stress such as those found at Aleppo (Syria), Settat (Morocco) and Marcos Juarez (Argentina), all classified by CIMMYT within Wheat Mega Environment 4 (Low rainfall, semi-arid environment; ME4: SA). It is distributed to 150 locations, and contains 50 entries. (2002)"

	uri <- "hdl:11529/10876"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=1,
		project="Semi-Arid Wheat Yield Trial",	   
		publication=NA,
		data_organization = "CIMMYT",
		carob_contributor="Robert Hijmans",
		carob_effort = NA,
		carob_date="2023-06-30",
		data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code"
 	)

	sets <- gsub("_RawData.xlsx", "", grep("RawData", basename(ff), value=TRUE))
	sets <- gsub("_RawData.xls", "", sets)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)

	d <- vector("list", length(sets))
	for (i in seq_along(sets)) {
		fs <- grep(sets[i], ff, value=TRUE)
		d[[i]] <- proc_wheat(fs)
	}

	dd <- do.call(carobiner::bindr, d)

	dd$soil_pH[dd$soil_pH > 20] <- NA
	
	dd$planting_date[dd$planting_date == "92-93"] <- "1992"
	dd$planting_date[dd$planting_date == "98-99"] <- "1998"
	dd$planting_date[dd$planting_date == "99-00"] <- "1999"
	
	carobiner::write_files(meta, dd, path=path)
}

