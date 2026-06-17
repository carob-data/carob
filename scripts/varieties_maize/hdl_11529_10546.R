# R script for "carob"


carob_script <- function(path) {

#Summary results and individual trial results from the International Intermediate White Hybrid - IIWH, (Transition Zone Intermediate White Hybrid - CHTHTZIW) conducted in 2007.
 
	uri <- "hdl:11529/10546"
	group <- "varieties_maize"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication= NA,
		project="International intermediate white hybrid trials",
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety;longitude;latitude",
		carob_contributor= "Mitchelle Njukuya",
		carob_effort = NA,
		carob_date="2024-02-27"
	)
	

	get_data <- function(fname, id, country, longitude, latitude, elevation) {
	  f <- ff[basename(ff) == fname]
	  r <- carobiner::read.excel(f) 
	  r <-r[22:31, 2:35]
		## ?r <- carobiner::read.excel(f, skip=21, n_max=11) 
	  
	  d <- data.frame( 
	    trial_id = as.character(id),
	    variety=r$BreedersPedigree1,
	    yield=as.numeric(r$GrainYieldTons_FieldWt)*1000,
	    asi=as.numeric(r$ASI),
	    plant_height=as.numeric(r$PlantHeightCm),
	    ear_height = as.numeric(r$EarHeightCm),
	    rlper = as.numeric(r$RootLodgingPer),
	    slper = as.numeric(r$StemLodgingPer),
	    husk = as.numeric(r$BadHuskCoverPer),
	    e_rot = as.numeric(r$EarRotTotalPer),
	    moist = as.numeric (r$GrainMoisturePer),
#	    plant_density = as.numeric(r$PlantStand_NumPerPlot),
	    e_asp = as.numeric(r$EarAspect1_5),
	    p_asp = as.numeric(r$PlantAspect1_5),
	    gls = r$GrayLeafSpot1_5,
	    rust = r$CommonRust1_5,
	    blight = r$LeafBlightTurcicum1_5,
	    country=country,
	    longitude=longitude,
	    latitude=latitude,
	    elevation = elevation)}
	
	d0 <- get_data("07CHTHTZIW2-1.xls",1,"Mexico", -98.8667, 19.5167, 2249)  
	d0$location <- "El Batan"
	d0$planting_date <- "2007-04-03"
	d0$harvest_date  <- "2007-10-25"
	
	d1 <- get_data("07CHTHTZIW3-1.xls",2,"Mexico", -99.2167, 20.0833, 2071)  
	d1$location <- "Tlaxcoapan, Hidalgo"
	d1$planting_date <- "2007-05-01"
	d1$harvest_date  <- "2007-10-04"
	
	d2 <- get_data("07CHTHTZIW5-1.xls",3,"Ethiopia", 37.0833, 5.85, 1650)  
	d2$location <- "Bako"
	d2$planting_date <- "2007-05-27"
	d2$harvest_date  <- "2007-09-04"
	
	d3 <- get_data("07CHTHTZIW10-1.xls",4,"Costa Rica", -96.6667, 19.35, 360)  
	d3$location <- "Guagaral"
	d3$planting_date <- "2007-05-09"
	d3$harvest_date  <- "2007-09-12"
	
	d4 <- get_data("07CHTHTZIW12-1.xls",5,"Egypt", 30.9833, 28.9333, 10)  
	d4$location <- "Sids"
	d4$planting_date <- "2007-05-15"
	d4$harvest_date  <- "2007-09-12"
	
	d5 <- get_data("07CHTHTZIW13-1.xls",6,"Mexico", 103.85, 20.3667, 1546)  
	d5$location <- "Poncitlan, Jalisco"
	d5$planting_date <- "2007-06-20"
	d5$harvest_date  <- "2007-12-21"
	
	d6 <- get_data("07CHTHTZIW18-1.xls",7,"Mexico", -99.0833, 20.2333, 1982)  
	d6$location <- "Tepatepec, Hidalgo"
	d6$planting_date <- "2007-05-15"
	d6$harvest_date  <- "2007-10-20" 
	
	d7 <- get_data("07CHTHTZIW19.xls",8,"Mexico", -98.8667, 19.5167, 2249)  
	d7$location <- "El Batan"
	d7$planting_date <- "2007-05-03"
	d7$harvest_date  <- "2007-11-14" 
	
	d8 <- get_data("07CHTHTZIW11-1.xls",9,"Egypt", 31.1167, 30.7167, 20)  
	d8$location <- "Gemmeiza"
	d8$planting_date <- "2007-06-04"
	d8$harvest_date  <- "2007-10-04"
	
	
	d <- carobiner::bindr(d0, d1, d2, d3, d4, d5, d6, d7, d8)
	d$crop = "maize"
	d$on_farm = TRUE
	d$striga_trial = FALSE 
	d$striga_infected = FALSE
	d$borer_trial = FALSE
	d$yield_part = "grain"
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	d <- d[!is.na(d$yield), ]
	carobiner::write_files(meta, d, path=path)
}



