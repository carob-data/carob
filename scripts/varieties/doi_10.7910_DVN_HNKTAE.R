# R script for "carob"
# license: GPL (>=3)



carob_script <- function(path) {

"
Advanced drought tolerant sorghum hybrids at Melkassa 2020

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for advanced drought tolerant hybrids evaluated at Melkassa, Ethiopia in 2020
"

	uri <- "doi:10.7910/DVN/HNKTAE"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;seed_weight;plant_height;flowering_days;maturity_days",
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-09",
		carob_completion = 90,		
		carob_effort = 1
	)
	
	f1 <- ff[basename(ff) == "Advanced drought tolerant sorghum hybrids at Melkassa 2020.xlsx"]
	r1 <- carobiner::read.excel(f1)

	d <- data.frame(
	  country = "Ethiopia",
	  adm1 = NA,
	  adm2 = "East Shewa Zone",
	  adm3 = "Adama",
	  location = (r1$Site),
	  plot_id = as.character(r1$Plot),
	  treatment = r1$Genotype,
	  #variety = as.character(r1$Genotype),
	  plot_area = NA,        
	  plant_height = r1$PHTMean,
	  maturity_days = r1$DTM,
	  flowering_days = r1$DTF,
	  variety_pedigree = r1$Pedigree,
	  yield = r1$`Yield g/plant`,
	  seed_weight = as.numeric(r1$`1000GW`),           
	  crop = "sorghum"
	)
	
	d$location[d$location == "MK"] <- "Melkassa"
	d$planting_date <- "2020"
	d$harvest_date <- "2020"
	d$trial_id <- "1"
	d$on_farm <- TRUE 
	d$is_survey <- FALSE 
	d$irrigated <- FALSE
	
## GADM for record keeping 
	#Melkassa not yet available in GADM so i used coordinates from google maps
	#geo <- data.frame(
	  #adm3 = c("Adama"),
	  #longitude = c(39.2677),
	  #latitude = c(8.4654),
	  #geo_uncertainty = c(29155),
	  #geo_source = c("GADM 4.1, adm3")
	#)
	
	d$longitude = 39.350
	d$latitude = 8.400
	d$geo_uncertainty = 29155 #geo_uncertainity for this location was replaced from adm3
	d$geo_source = "GADM 4.1, adm3" #only geo_uncertainty was from this source
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	carobiner::write_files(path, meta, d)
}


