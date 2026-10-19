# R script for "carob"
# license: GPL (>=3)

## ISSUES
## added varible IPAR = intercepted photosynthetic active radiation
#### location is confusing the dataset discription only mentions one study site, yet location data inside mentions two sites which are located in two different states

carob_script <- function(path) {

"
Agronomy Data, Kano, Nigeria under KSADP project

A 5-year development project titled; Kano State Agro-Pastoral Development Project (KSADP). The project aims to reach 450,000 smallholder farmers and enhance crop production and livestock productivity in Kano State
"

	uri <- "doi:10.25502/gp8a-2c31/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Kano State Agro-Pastoral Development Project (KSADP)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield;flowering_days;maturity_days;harvest_index", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-09-25",
		carob_completion = 90,	
		carob_effort = 5
	)
	
	f1 <- ff[basename(ff) == "data_dictionary.csv"]
	f2 <- ff[basename(ff) == "maize-experimental-data.csv"]

	#r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	d <- data.frame(
	  country = "Nigeria",
	  location = r2$loc,
	  plot_id = as.character(r2$Plot_No),
	  variety = r2$Variety,
	  planting_date = as.character(as.Date(r2$Planting_Date, format = "%m/%d/%Y")),
	  yield = r2$yield,
	  #treatment = r2$ID,
	  plant_height = r2$PLHT,
	  maturity_days = r2$maturity,
	  flowering_days = r2$DYSK50,
	  yield_moisture = r2$MC,
	  harvest_index = r2$HI,
	  rep = r2$rep,
	  LAI = r2$LAI,
	  IPAR = r2$IPAR, ### added variable intercepted photosynthetic active radiation
	  seed_weight = r2$swt100 * 10,
	  SPAD = r2$SPAD,
	  N_fertilizer = as.numeric(sub("N", "", r2$Nrate)),
	  crop = "maize"
	)
	
	d$location[d$location == "Buk"] <- "Bayero University"		
	d$harvest_date <- NA
	d$trial_id <- "1"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- NA	

	### The dataset discription says the study was done in Kano state, yet the data has two locations Buk and Zaria. With Buk = University Kano but Zaria is Kuduna State
	 ## 
	geo <- data.frame(
	    adm1 = c("Kaduna", "Kano"),
	    adm2 = c("Zaria", "Kano"),
	    location = c("Zaria", "Bayero University"), # assuming BUK Agricultue Faculty
	    longitude = c(7.7143, 8.423),
	    latitude = c(11.0231, 11.973),
	    geo_uncertainty = c(16966, NA),
	    geo_source = c("GADM 4.1, adm2", "Google Maps")
	)

	d <- merge(d, geo, by = "location", all.x = TRUE) 
	d$geo_from_source <- FALSE
		 
   d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   d$fertilizer_type <- NA
   
   d$yield_part <- "grain"
   d$yield_moisture <- as.numeric(NA)
   d$yield_isfresh <- TRUE
   
	carobiner::write_files(path, meta, d)
}


