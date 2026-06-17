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
	
	d <- lapply(years, \(y) {
		f <- ff[grep(paste0("^", y), basename(ff))]
		proc_wheat(f)
	})
	
	d <- do.call(carobiner::bindr, d) 
	d$soil_pH[d$soil_pH == 79] <- 7.9

	d$planting_date[d$planting_date == "90-91"] <- "1990"
	d$planting_date[d$planting_date == "91-92"] <- "1991"
	d$planting_date[d$planting_date == "92-93"] <- "1992"
	d$planting_date[d$planting_date == "95-96"] <- "1995"
	d$planting_date[d$planting_date == "97-98"] <- "1997"
	d$planting_date[d$planting_date == "98-99"] <- "1998"
	d$planting_date[d$planting_date == "00-01"] <- "2000"
	
	d$yield[d$location == "Pan Rice Exper. Sta."] <- d$yield[d$location == "Pan Rice Exper. Sta."] / 10
	d$yield[d$location == "La Ballenera"] <- d$yield[d$location == "La Ballenera"] / 10

	d$emergence_date[d$emergence_date == "2026-08-26"] <- "2001-08-26"
	
	carobiner::write_files(path, meta, d)
}

	
	
