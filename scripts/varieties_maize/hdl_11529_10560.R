# R script for "carob"


carob_script <- function(path) {

"Summary results and individual trial results from the International Late Yellow Hybrid - ILYH, (Elite Tropical Late Yellow Normal and QPM Hybrid Trial - CHTTY) conducted in 2006."

	uri <- "hdl:11529/10560"
	group <- "varieties_maize"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Late Yellow Hybrid Trial",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-08-01"
	)
	
	get_data <- function(fname, id, country, longitude, latitude, elevation, location, adm1, planting, harvest) {
	  
	  f <- ff[basename(ff) == fname]
	  suppressWarnings(r <- carobiner::read.excel(f) )
	  r <- r[22:52, 2:35]
	  
	  d <- data.frame( 
	    trial_id = as.character(id),
	    yield_part = "grain",
	    variety = r$BreedersPedigree1,
	    yield = as.numeric(r$GrainYieldTons_FieldWt)*1000,
	    asi = as.numeric(r$ASI),
	    plant_height=as.numeric(r$PlantHeightCm),
	    ear_height = as.numeric(r$EarHeightCm),
	    rlper = as.numeric(r$RootLodgingPer),
	    slper = as.numeric(r$StemLodgingPer),
	    husk = as.numeric(r$BadHuskCoverPer),
	    e_rot = as.numeric(r$EarRotTotalPer),
	    yield_moisture = as.numeric (r$GrainMoisturePer),
#	    plant_density = as.numeric(r$PlantStand_NumPerPlot),
	    e_asp = as.numeric(r$EarAspect1_5),
	    p_asp = as.numeric(r$PlantAspect1_5),
	    country = country,
	    adm1 = adm1,
	    location = location,
	    longitude = longitude,
	    latitude = latitude,
	    elevation = elevation,
	    planting_date = planting,
	    harvest_date = harvest
	  )
	}
	
	d0 <- get_data("06CHTTY1-1.xls", 1, "Mexico", -103.5408, 19.8744, 1340, "Usmajac", "Jal", "2006-01-22", NA)
	d1 <- get_data("06CHTTY8-1.xls", 2, "Bolivia", -63.1333, -17.7, 398, "El Abra", "Santa Cruz", "2006-12-03", "2007-04-08")
	d2 <- get_data("06CHTTY12-1.xls", 3, "Ghana", -1.5833, 6.75, 270, "Fumesua", NA, "2007-07-04", "2007-09-08")
  d3 <- get_data("06CHTTY13-1.xls", 4, "Ghana", -1.3667, 7.3833, 232, "Ejura", NA, "2007-08-05", "2007-12-09")
  d4 <- get_data("06CHTTY22-1.xls", 5, "India", 75.8833, 19.85, 550, "Jaina", NA, "2006-07-12", "2006-11-24")
  d5 <- get_data("06CHTTY23-1.xls", 6, "Nigeria", 7.6167, 11.1833, 730, "Samaru", NA, "2006-09-27", "2007-01-30")
  d6 <- get_data("06CHTTY26-1.xls", 7, "Vietnam", 105.75, 21.0333, 5, "Dan Phuong", NA, "2006-09-27", "2007-01-30")
  d7 <- get_data("06CHTTY27-1.xls", 8, "Vietnam", 105.75, 21.0333, 5, "Dan Phuong-Hatay", NA, "2007-07-29", "2007-11-03")
  d8 <- get_data("06CHTTY28-1.xls", 9, "Bolivia", -60.8, -15.4667, 205, "El Palmar", "Santa Cruz", "2006-12-06", "2007-04-12")
  d9 <- get_data("06CHTTY29-1.xls", 10, "Sudan", 32.8333, 14.4, 0, "Wad Medani", NA, "2007-07-29", "2007-11-06")
  d10 <- get_data("06CHTTY33-1.xls", 11, "Bolivia", -62.95, -17.75, 380, "Cotoca", "Santa Cruz", "2006-12-04", "2007-04-10")
  d11 <- get_data("06CHTTY36-1.xls", 12, "Mexico", -96.1428, 19.1803, 15, "Cotaxtla", "Veracruz", "2006-09-27", "2007-01-30")
  
  d <- carobiner::bindr(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11)
  
  d$crop <- "maize"
  d$on_farm <- TRUE
  d$striga_trial <- FALSE
  d$striga_infected <- FALSE
  d$borer_trial <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  d$geo_from_source <- TRUE

	
	carobiner::write_files(path, meta, d)
}



