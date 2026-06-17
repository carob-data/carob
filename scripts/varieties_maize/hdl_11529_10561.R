
carob_script <- function(path) {

"International Late White Hybrid Trial - ILWH0609 Summary results and individual trial results from the International Late White Hybrid - ILWH, (Advanced Tropical Three Way and Single Crosses White Normal and QPM Late - TTWSCWL) conducted in 2006."

	uri <- "hdl:11529/10561"
	group <- "varieties_maize"
	ff  <- carobiner::get_data(uri, path, group)
 
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Late White Hybrid Trial - ILWH0609",
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_effort = NA,
		carob_date = "2024-10-22",
		notes = NA,
		design = NA
	)
	
	
	f <- ff[basename(ff) == "06TTWSCWL-Locations.xls"]
	rlocs <- carobiner::read.excel.hdr(f, skip=8, hdr=2) 
	rlocs <- rlocs[-(1:2), ]
	names(rlocs) <- c("ID", names(rlocs)[-ncol(rlocs)])
	
	locs <- data.frame(
	  latitude = as.numeric(gsub("o", "", rlocs$Latitude_Latitud)) +  as.numeric(gsub("'", "", rlocs$X)) / 60,
	  longitude = (as.numeric(gsub("o", "", rlocs$Longitude_Longitud)) + as.numeric(gsub("'", "", rlocs$X.2)) / 60 ) * ifelse(rlocs$X.3 == "W", -1, 1),
	  country = rlocs$Country_País,
	  location = rlocs$Location_Localidad,
	  elevation = as.numeric(rlocs$Altitude_Masl_Altitud),
	  planting_date = as.character(rlocs$Planting_Date_Fecha.de),
	  harvest_date = as.character(rlocs$Harvest_Date_Fecha.de)
	)
	
	locs$harvest_date <- as.character(as.Date(as.numeric(locs$harvest_date), origin = "1900-01-01") - 2)
	locs$planting_date <- as.character(as.Date(as.numeric(locs$planting_date), origin = "1900-01-01") - 2 )
	
	
	locs$country <- gsub("México", "Mexico", locs$country)
	s <- sapply(strsplit(locs$location, ", "), \(i) i[1:2]) |> t()
	locs$location <- s[,1]
	locs$adm1 <- s[,2]
	locs$geo_from_source <- TRUE

	
	
	get_data <- function(fname, location, cols=2:43) {
	  f <- ff[basename(ff) == fname]
	  r <- carobiner::read.excel(f,  col_types="text")
	  r <- r[22:50, cols]
	  
	  data.frame( 
	    trial_id = gsub("-1.xls", "", fname),
	    location = location,
	    variety_code = as.character(r$Name),
	    variety_pedigree=r$BreedersPedigree1,
	    asi=as.numeric(r$ASI),
	    plant_height=as.numeric(r$PlantHeightCm),
	    ear_height = as.numeric(r$EarHeightCm),
	    rlper = as.numeric(r$RootLodgingPer),
	    slper = as.numeric(r$StemLodgingPer),
	    husk = as.numeric(r$BadHuskCoverPer),
	    e_rot = as.numeric(r$EarRotTotalPer),
	    moist = as.numeric (r$GrainMoisturePer),
#		should be per ha
#	    plant_density = as.numeric(r$PlantStand_NumPerPlot),
	    e_asp = as.numeric(r$EarAspect1_5),
	    p_asp = as.numeric(r$PlantAspect1_5),
	    gls = r$GrayLeafSpot1_5,
	    rust = r$CommonRust1_5,
	    blight = r$LeafBlightTurcicum1_5,
	    yield = as.numeric(r$GrainYieldTons_FieldWt) * 1000
	  ) 
	}
	
	d1 <- get_data("06TTWSCWL3-1.xls", "Cotaxtla")
	d2 <- get_data("06TTWSCWL4-1.xls", "Las Vegas")
	d3 <- get_data("06TTWSCWL5-1.xls", "Las Vegas")
	d4 <- get_data("06TTWSCWL8-1.xls", "Samaru")
	d5 <- get_data("06TTWSCWL10-1.xls", "Ejura")
	
	
	dd <- rbind(d1, d2, d3, d4, d5)
	
	d <- merge(dd, locs, by="location")
	
	d$crop = "maize"
	d$on_farm = TRUE
	d$striga_trial = FALSE 
	d$striga_infected = FALSE
	d$borer_trial = FALSE
	d$yield_part = "grain"
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$N_fertilizer <- d$P_fertilizer  <- d$K_fertilizer <- as.numeric(NA)
	carobiner::write_files(path, meta, d)
}
	  
