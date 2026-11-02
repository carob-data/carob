# R script for "carob"
# license: GPL (>=3)


## REJECTED
# data was standardised as part of doi:10.25502/bf6e-0181/d

## ISSUES

carob_script <- function(path) {

"
Datasets on soil characteristics of fertilized improved and local varieties of cassava grown in the highlands of South Kivu, DR Congo

The use of mineral fertilizer and organic inputs with an improved and local variety of cassava allow firstly to identify nutrient limitations to cassava production, and secondly to investigate the effects of variety and combined application of mineral and organic inputs on cassava growth and yields in the highland conditions of the Democratic Republic of Congo (DR Congo). Data on growth parameters, yields and yield components of the improved and local varieties of cassava, economic analysis and soil parameters, collected during two growing cycles of cassava are presented. The data support a research article which is under review “Increased cassava growth and yields through improved variety use and fertilizer application in the highlands of South Kivu, Democratic Republic of Congo” [1]. Data on plant height and diameter was measured throughout the growing period of the crop while the data on the storage root, stem, tradable storage root and non-tradable storage root was determined at 12 months after planting (MAP) of the field experiments. The economic analysis was performed using a simplified financial analysis where the additional benefits were calculated relative to the respective control treatments while the total costs included the purchasing prices of fertilizer and the additional net benefits, the revenue from the increased storage root yields due to fertilizer application. The value cost ratio (VCR) was calculated as the additional net benefits over the cost of fertilizer purchase.
"

	uri <- "doi:10.25502/wp27-2883/d"
	group <- "reject"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
	  data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
	  treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-10-01",
	  carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "data_soil-characteristic_field-experiment.csv"]

	r1 <- read.csv(f1)

	d <- data.frame(
		country = "Democratic Republic of the Congo",
		adm1 = "Sud-Kivu",
		adm2 = r1$Site,
		location = r1$Village,
		season = r1$Season,
		rep = r1$Replicate,
		soil_pH = r1$pH._CaCl2_2H2O,
		soil_C_total = r1$C,
		soil_N = r1$N * 10000,                 #convert from % to mg/kg
		soil_P = r1$P,
		soil_K_exch = r1$K,
		soil_Mg_exch = r1$Mg,
		soil_Ca_exch = r1$Ca,
		soil_Na = r1$Na * 229.9,               #convert from cmolc kg-1 to mg/kg
		soil_Mn = r1$Mn * 274.7,               #convert from cmolc kg-1 to mg/kg
		soil_CEC = r1$CEC,
		soil_clay = r1$Clay,
		soil_sand = r1$Sand,
		soil_silt = r1$Silt
	)
  
	d$location <- gsub("Muhogoza", "Muhongoza", d$location)
	
	d$trial_id <- as.character(1)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <-FALSE
	d$longitude <- NA 
	d$latitude <- NA
  d$geo_from_source <- FALSE
  d$longitude[d$location == "Kasheke"] <- 28.8594
  d$latitude[d$location == "Kasheke"] <- -2.1386                                                                       #https://www.geonames.org/search.html?q=Kasheke&country=CD
  d$longitude[d$location == "Munanira"] <- 28.9072
  d$latitude[d$location == "Munanira"] <- -2.8061                                                                      #https://www.geonames.org/search.html?q=Munanira&country=CD
  d$longitude[d$adm2 == "Kalehe" & d$location %in% c("Cibanda","Cibandja","Muhongoza")] <- 28.9167
  d$latitude[d$adm2 == "Kalehe" & d$location %in% c("Cibanda","Cibandja","Muhongoza")] <- -2.1                      #https://www.geonames.org/search.html?q=Kalehe&country=CD
  
  d$planting_date <- d$harvest_date  <- d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
  d$fertilizer_type <- d$crop <- d$yield_part <- d$yield_isfresh <- d$yield_moisture <- NA
  d <- unique(d)
  
	carobiner::write_files(path, meta, d)
}



