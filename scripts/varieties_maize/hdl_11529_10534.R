# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"International Early White Hybrid trial - IEWH0852+ Summary results and individual trial results from the International Early White Hybrid - IEWH, (Highland Early White Hybrids - CHTHEW) conducted in 2008."

	uri <- "hdl:11529/10534"
	group <- "varieties_maize"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Wheat Trials",
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-10-20",
		notes = NA,
		design = NA
	)
	

	f <- ff[basename(ff) == "08CHTHEW-Locations.xls"]
	rlocs <- carobiner::read.excel.hdr(f, skip=8, hdr=2) 
	rlocs <- rlocs[-1, ]
	names(rlocs) <- c("ID", names(rlocs)[-ncol(rlocs)])
	
	locs <- data.frame(
	  latitude = as.numeric(gsub("o", "", rlocs$Latitude_Latitud)) +  as.numeric(gsub("'", "", rlocs$X)) / 60,
	  longitude = (as.numeric(gsub("o", "", rlocs$Longitude_Longitud)) + as.numeric(gsub("'", "", rlocs$X.3)) / 60 ) * ifelse(rlocs$`X.4` == "W", -1, 1),
	  country = rlocs$Country_País,
	  location = rlocs$Location_Localidad,
	  elevation = rlocs$Masl_Altitud_msnm,
	  planting_date = as.character(rlocs$Date_Fecha.de_Siembra),
	  harvest_date = as.character(rlocs$Date_Fecha.de_Cosecha)
	)
	i <- locs$harvest_date == "2007-12-11"
	locs$harvest_date[i] <- "2008-12-11"
	
	
	locs$country <- gsub("México", "Mexico", locs$country)
	s <- sapply(strsplit(locs$location, ", "), \(i) i[1:2]) |> t()
	locs$location <- s[,1]
	locs$adm1 <- s[,2]
	locs$geo_from_source <- TRUE
	
	get_data <- function(fname, location, cols=2:35) {
	  f <- ff[basename(ff) == fname]
	  r <- carobiner::read.excel(f,  col_types="text")
	  r <- r[19:33, cols]
	  r <- r[-(1:2), ]
	  	  
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
#	    plant_density = as.numeric(r$PlantStand_NumPerPlot),
	    e_asp = as.numeric(r$EarAspect1_5),
	    p_asp = as.numeric(r$PlantAspect1_5),
	    gls = r$GrayLeafSpot1_5,
	    rust = r$CommonRust1_5,
	    blight = r$LeafBlightTurcicum1_5,
	    yield = as.numeric(r$GrainYieldTons_FieldWt) * 1000
	  ) 
	}
	
	d1 <- get_data("08CHTHEW7-1.xls", "Huamantla")
	d2 <- get_data("08CHTHEW18-1.xls", "San Juan del Rio")
	d3 <- get_data("08CHTHEW23-1.xls", "Batán")
	d4 <- get_data("08CHTHEW27-1.xls", "Jamay")
	d5 <- get_data("08CHTHEW29-1.xls", "Durango")
	
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
	d <- d[!is.na(d$yield), ]
	carobiner::write_files(path, meta, d)
}
	
