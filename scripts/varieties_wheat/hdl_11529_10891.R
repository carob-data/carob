# R script for "carob"

carob_script <- function(path) {
  
"The International Bread Wheat Screening Nursery (IBWSN) is designed to rapidly assess a large number of advanced generation (F3-F7) lines of spring bread wheat under Mega-environment 1 (ME1) which represents diversity for a wide range of latitudes, climates, daylengths, fertility conditions, water management, and (most importantly) disease conditions. The distribution of these nurseries is deliberately biased toward the major spring wheat regions of the world where the diseases of wheat are of high incidence. It is distributed to 180 locations and contains 300-450 entries. (1990)"
  
    
	uri <- "hdl:11529/10891"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=2,
		project="International Bread Wheat Screening Nursery",
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
	years <- gsub(" IBWSN_RawData.xlsx", "", grep("IBWSN_RawData.xlsx", basename(ff), value=TRUE))
	
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

	d$wide$soil_pH[d$wide$soil_pH == 79] <- 7.9

	d$wide$planting_date[d$wide$planting_date == "90-91"] <- "1990"
	d$wide$planting_date[d$wide$planting_date == "91-92"] <- "1991"
	d$wide$planting_date[d$wide$planting_date == "92-93"] <- "1992"
	d$wide$planting_date[d$wide$planting_date == "95-96"] <- "1995"
	d$wide$planting_date[d$wide$planting_date == "97-98"] <- "1997"
	d$wide$planting_date[d$wide$planting_date == "98-99"] <- "1998"
	d$wide$planting_date[d$wide$planting_date == "00-01"] <- "2000"
	
	d$wide$yield[d$wide$location == "Pan Rice Exper. Sta."] <- d$wide$yield[d$wide$location == "Pan Rice Exper. Sta."] / 10
	d$wide$yield[d$wide$location == "La Ballenera"] <- d$wide$yield[d$wide$location == "La Ballenera"] / 10

	d$wide$emergence_date[d$wide$emergence_date == "2026-08-26"] <- "2001-08-26"
	
	carobiner::write_files(path, meta, d$wide, d$long)
}

	
	
