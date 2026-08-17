# R script for "carob"
# license: GPL (>=3)

## ISSUES
# the unit of soil is given /dm3

carob_script <- function(path) {

"
Feasibility of transference of inoculation-related technologies: A case study of evaluation of soybean rhizobial strains under the agro-climatic conditions of Brazil and Mozambique

Science-based “research-in-development” project focused on putting nitrogen fixation to work for smallholder farmers growing legume crops in Africa
"
	uri <- "doi:10.25502/sjtv-pq11/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1016/j.agee.2017.06.037",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-12",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "chibeba_et_al_2017_tech_transfer_metadata.xlsx"]
	f2 <- ff[basename(ff) == "chibeba_et_al_2017_tech_transfer_plant__yield_data_brazil.csv"]
	f3 <- ff[basename(ff) == "chibeba_et_al_2017_tech_transfer_plant__yield_data_mozambique.csv"]
	f4 <- ff[basename(ff) == "chibeba_et_al_2017_tech_transfer_site_charateristics.csv"]

	#r1a <- carobiner::read.excel(f1, sheet="Site_Characterization_Metadata")
	#r1b <- carobiner::read.excel(f1, sheet="Abbr")
	#r1c <- carobiner::read.excel(f1, sheet="Plant_&_Yield_Metadata_Brazil")
	#r1d <- carobiner::read.excel(f1, sheet="Plant_&_Yield_Metadata_Mozam")
	r1 <- read.csv(f2, na= "")
	r2 <- read.csv(f3, na= "")
	r3 <- read.csv(f4)
	
### process	
	d1 <- data.frame(
	  country = "Brazil",
		location = r1$Location,
		planting_date = substr(r1$Crop.Season, 1, 4),
		seed_treatment = trimws(r1$Treatment),
		nodule_NN = r1$NN,
		#nodule_weight = r1$NDW, ## mg/plant
		rep = r1$Block,
		#fwy_residue = r1$SDW, ## mg/plant
		#residue_N = r1$TNS, # mg/plant
		yield =  as.numeric(trimws(gsub(",", "", r1$GY))),
		seed_weight = r1$GDW*10,
		trial_id = "1"
	)
	
	d1 <- d1[!is.na(d1$yield),]

	d2 <- data.frame(
		country = "Mozambique",
		seed_treatment = trimws(r2$Treatment),
		location = r2$Location,
		planting_date = substr(r2$Crop_Season, 1, 4),
		rep = r2$Block,
		nodule_NN = r2$NN, 
		#nodule_weight = r2$NDW, # mg/plant
		#fwy_residue = r2$SDW, # g/plant
		fwy_total = as.numeric(trimws(gsub(",", "", r2$AGB))),
		yield = as.numeric(trimws(gsub(",", "", r2$GY))),
		seed_weight = r2$GDW*10,
		trial_id = "2"
	)
	
	d2 <- d2[!is.na(d2$yield),]
	
	d <- carobiner::bindr(d1, d2)
	
	d3 <- data.frame(
	  country = r3$Country,
	  location = r3$Location,
	  planting_date = substr(r3$Crop.Season, 1, 4),
	  latitude = r3$Latitute,
	  longitude = r3$Longitude,
	  elevation = trimws(r3$Altitude_m),
	  geo_from_source = TRUE,
	  #r4$Rhizobia_MPN,
	  soil_pH_CaCl2 = r3$pH_CaCl2,
	  soil_SOM = r3$SOM_g_per_dm3/10,
	  soil_SOC = r3$Organic_P_mg_per_dm3/10000,
	  soil_K_exch = r3$K_cmolc_per_dm3,
	  soil_Ca_exch = r3$Ca_cmolc_per_dm3,
	  soil_Mg_exch = r3$Mg_cmolc_per_dm3,
	  soil_acidity_sat = r3$EA_cmolc_per_dm3,
	  soil_silt = r3$Silt_g_per_kg/10,
	  soil_sand = r3$Sand_g_per_kg/10,
	  soil_clay = r3$Clay_g_per_kg/10,
	  temp = rowMeans(r3[, paste0("Temp_C_soybean_growth_stage_", c("VE", "VC", "V6", "V5", "V4", "V3", "V2", "V1", "R8", "R7", "R6", "R5", "R4", "R3", "R1"))], na.rm = TRUE) ,
	  rain = rowSums(r3[, paste0("Rainfall_mm_soybean_growth_stage_",c("VE", "VC", "V6", "V5", "V4", "V3", "V2", "V1", "R8", "R7", "R6", "R5", "R4", "R3", "R1"))], na.rm = TRUE)
	)
	
	####
	d3 <- d3[!is.na(d3$soil_pH_CaCl2),]
	
	d <- merge(d, d3, by= c("location", "country", "planting_date"), all.x = TRUE)

	## Fixing long and lat
	i <- grepl("W", d$longitude)
	d$longitude[i] <- -(as.numeric(substr(d$longitude[i], 1, 2)) + as.numeric(substr(d$longitude[i], 4, 5))/60)
	i <- grepl("S", d$latitude)
	d$latitude[i] <- -(as.numeric(substr(d$latitude[i], 1, 2)) + as.numeric(substr(d$latitude[i], 4, 5))/60)
	d$latitude <- as.numeric(d$latitude)
	d$longitude <- as.numeric(d$longitude)
	d$elevation <- as.numeric(gsub(",", "", d$elevation))
	
	### Fixing missing coordinate 
	i <- grepl("Ponta Grossa", d$location)
	d$latitude[i] <- -25.1397	
	d$longitude[i] <- -50.0808
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "GADM 4.1, adm2"
	d$uncertainty[i] <- 40685
	####
	i <- grepl("Mozambique", d$country) 
	d$latitude[i] <- -d$latitude[i]
	## conflit coordinate
	## from publication
	i <- grepl("Nkhame", d$location)
	d$latitude[i] <- -14.633 
	d$longitude[i] <- 33.55
	i <- grepl("Ntengo", d$location)
	d$latitude[i] <- -14.55
	d$longitude[i] <- 34.1833
	
	
	d$crop <- "soybean"
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_moisture <- NA
	d$yield_part <- "grain"
	d$irrigated <- NA
	d$yield_isfresh <- TRUE
	inoculated <- TRUE
	innoculant <- "Rhizobia"
	d$harvest_date <- NA_character_
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	

	carobiner::write_files(path, meta, d)
}

