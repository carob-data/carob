# R script for "carob"


carob_script <- function(path) {

"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2002)"

	uri <- "hdl:11529/11089"
	group <- "varieties_wheat"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=1,
		project="High Temperature Wheat Yield Trial",	   
		publication=NA,
		data_organization = "CIMMYT",
		carob_contributor="Andrew Sila",
		carob_effort = NA,
		carob_date="2023-05-03",
		data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code"
 	)


	sets <- gsub("_RawData.xlsx", "", grep("RawData", basename(ff), value=TRUE))
	sets <- gsub("_RawData.xls", "", sets)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)

	maxid <- 0
	dd <- vector("list", length(sets))
	for (i in seq_along(sets)) {
		fs <- grep(sets[i], ff, value=TRUE)
		d <- proc_wheat(fs)
		d$wide$record_id <- d$wide$record_id + maxid
		d$long$record_id <- d$long$record_id + maxid
		maxid <<- max(d$wide$record_id)
		dd[[i]] <- d
	}
	dlong <- do.call(carobiner::bindr, lapply(dd, \(x) x[["long"]]))
	dwide <- do.call(carobiner::bindr, lapply(dd, \(x) x[["wide"]]))
	dlong$record_id <- as.integer(dlong$record_id)
	dwide$record_id <- as.integer(dwide$record_id)
	d <- list(long=dlong, wide=dwide)

	d$wide$planting_date[d$wide$planting_date == "92-93"] <- "1992"
	d$wide$planting_date[d$wide$planting_date == "99-00"] <- "1999"
	d$wide$planting_date[d$wide$planting_date == "00-01"] <- "2000"

	carobiner::write_files(path, meta, d$wide, d$long)
}

