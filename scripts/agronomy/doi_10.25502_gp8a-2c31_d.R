# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them
## added varible PAR = photosyntehically active radiation intercepted
#### location is confusing the dataset discription only mentions one study site, yet location data inside mentions two sites which are located in two different states

carob_script <- function(path) {

"
Agronomy Data, Kano, Nigeria under KSADP project

A 5-year development project titled; Kano State Agro-Pastoral Development Project (KSADP). The project aims to reach 450,000 smallholder farmers and enhance crop production and livestock productivity in Kano State
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.25502/gp8a-2c31/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Kano State Agro-Pastoral Development Project (KSADP)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "fertilizer_used;fertilizer_amount",
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


	d1 <- data.frame(
	  country = "Nigeria",
	  adm2 = "Kano",
	  location = r2$loc,
	  plot_id = as.character(r2$Plot_No),
	  variety = r2$Variety,
	  planting_date = format(as.Date(r2$Planting_Date, format = "%m/%d/%Y"), "%Y-%m-%d"),
	  yield = r2$yield,
	  #treatment = r2$ID,
	  plant_height = r2$PLHT,
	  maturity_days = r2$maturity,
	  flowering_days = r2$DYSK50,
	  yield_moisture = r2$MC,
	  harvest_index = r2$HI,
	  rep = r2$rep,
	  LAI = r2$LAI,
	  PAR = r2$IPAR, ### added variable intercepted photosynthetic active radiation
	  seed_weight = r2$swt100,
	  SPAD = r2$SPAD,
	  fertilizer_amount = r2$Nrate,
	  fertilizer_used = TRUE,
	  crop = "maize"
	)
	
	
	d1$fertilizer_amount <- as.numeric(sub("N", "", d1$fertilizer_amount))
	
	d <- d1
	
	d$harvest_date <- NA
	d$trial_id <- "1"
	
## about the data (TRUE/FALSE)
	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- NA
	

## see carobiner::geocode
	### The dataset discription says the study was done in Kano state, yet if you check in the variable location the study seems to have been done in two different locations Buk and Zaria. With Buk =  could be Bayero University Kano but Zaria is not found in Kano state it is in Kuduna State
	## geo_cordinates displayed here were obtained from GADM adm2 = Kano State in Nigeria
	  d$longitude <- 8.4775
	  d$latitude <- 11.9405
	  d$geo_uncertainty <-  4791
	  d$geo_source <- "GADM 4.1, adm2"
	  d$geo_from_source <- FALSE
	  
	  
   d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   d$N_fertilizer <- as.numeric(d$fertilizer_amount)
   d$fertilizer_type <- NA
   
   d$yield_part <- "grain"
   d$yield_moisture <- as.numeric(NA)
   d$yield_isfresh <- TRUE
   
	carobiner::write_files(path, meta, d)
}


