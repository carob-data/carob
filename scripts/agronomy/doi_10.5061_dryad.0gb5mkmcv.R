# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Data for: Effect of cover cropping and biosolarization on eggplant growth, soil pests, and soil nitrogen

This dataset includes raw data values used to determine the effects of cover cropping and biosolarization on subsequent eggplant growth and yield, soil pest populations, and soil nitrogen. This includes data on eggplant health, eggplant height, eggplant above-ground plant mass, eggplant fruit mass, soil total nitrogen, soil nitrate, soil ammonium, weed mass, and eggplant crown rot disease incidence.
"

	uri <- "doi:10.5061/dryad.0gb5mkmcv"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	
	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		data_organization = "UCD",
		publication = "doi:10.1016/j.jafr.2025.102160",
		project = NA,
		carob_date = "2026-08-11",
		design = NA,
  	data_type = "experiment",
		notes = NA,
		treatment_vars = "cover_crop;biosolarized_used",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 80	,
		carob_effort = 2
		
	)
	

	f1 <- ff[basename(ff) == "Crown_rot_incidence.xlsx"]
	f2 <- ff[basename(ff) == "Fruit_data.xlsx"]
	f3 <- ff[basename(ff) == "Plant_health.xlsx"]
	f4 <- ff[basename(ff) == "Plant_height.xlsx"]
	f5 <- ff[basename(ff) == "Plant_mass.xlsx"]
	f6 <- ff[basename(ff) == "Soil_nitrogen.xlsx"]
	f7 <- ff[basename(ff) == "Weed_density.xlsx"]
	f8 <- ff[basename(ff) == "README.md"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2, sheet="Cumulative fruit mass")
	r3 <- carobiner::read.excel(f3, sheet="Heath over time")
	r4 <- carobiner::read.excel(f4, sheet="Height over time")
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6, sheet="Mineral nitrogen")
	r7 <- carobiner::read.excel(f6, sheet="Total nitrogen")
	r8 <- carobiner::read.excel(f7)
	
	d1 <- data.frame(
	  treatment = r1$Treatment,
	  rep = as.integer(r1$`Replicate plot`),
	  disease_incidence = as.character(r1$`Crown rot incidence rate (%)`)
	)
	
	d2 <- data.frame(
	  treatment = r2$Treatment,
	  rep = as.integer(r2$`Replicate plot`),
	  DAP = as.integer(r2$`Weeks Post Transplant`*7),
	  plot_area = 15*15 , #m2
	  yield = (r2$`Cumulative fruit mass (kg)`/15*15)*10000
	)
	
	d <- merge(d1, d2, by = c("treatment", "rep"), all = TRUE)
	
	d3 <- data.frame(
	  treatment = r3$Treatment,
	  rep = as.integer(r3$`Replicate plot`),
	  DAP = as.integer(r3$Week*7),
	  plant_health_index = r3$`Health rating`
	)
	
	### 
	d <- merge(d, d3, by = c("treatment", "DAP", "rep"), all = TRUE)
	
	####
	d4 <- data.frame(
	  treatment = r4$Treatment,
	  rep = as.integer(r4$`Replicate plot`),
	  DAP = as.integer(r4$weeks*7),
	  plant_height = r4$`height (m)`*100
	)
	
	####
	d <- merge(d, d4, by = c("treatment", "DAP", "rep"), all = TRUE)
	
	d5 <- data.frame(
	  treatment = r5$Treatment,
	  rep = as.integer(r5$`Replicate plot`),
	  plot_area = 15*15 , #m2
	  fwy_total = (r5$`Plant mass (kg)`/15*15)*10000
	)
	
	####
	d <- merge(d, d5, by = c("treatment", "rep", "plot_area"), all = TRUE)
	
	####
	d6 <- data.frame(
	  treatment = r6$Treatment,
	  rep = as.integer(r6$`Replicate plot`),
	  soil_NH4 = r6$`NH4-N (ppm)`,
	  soil_NO3 = r6$`NO3-N (ppm)`
	)
	
	d <- merge(d, d6, by = c("treatment", "rep"), all = TRUE)
	
	d7 <- data.frame(
	  treatment = r7$Treatment,
	  rep = as.integer(r7$`Replicate plot`),
	  soil_N_total = r7$`total nitrogen (ppm)`
	)
	
	d <- merge(d, d7, by = c("treatment", "rep"), all = TRUE)
	
	d8 <- data.frame(
	  treatment = r8$Treatment,
	  rep = as.integer(r8$`Replicate plot`),
	  weed_density = r8$`weed density (kg/hectare)`
	)
	
	d <- merge(d, d8, by = c("treatment", "rep"), all = TRUE)
	
	####
	d$cover_crop <- ifelse(grepl("B. juncea-V. villosa", d$treatment), "brown mustard;hairy vetch", 
	                ifelse(grepl("B. juncea monoculture", d$treatment), "brown mustard", "none"))
	d$biosolarized_used <- grepl("biosolarized|solarized", d$treatment)
	
	#####
	d$crop <- "eggplant"
	## from publication
	d$country <- "United States" 
	d$location <- "UC Davis Plant Pathology Research Farm"
	d$yield_part <- "fruit"
	d$geo_from_source <- TRUE
	d$latitude <- 38.5194239
	d$longitude <- -121.7684936
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$trial_id <-
	d$yield_moisture <- NA
	d$irrigated <- NA
	d$planting_date <- "2023"
	d$harvest_date <- NA_character_
	d$yield_isfresh <- TRUE
	  
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA) 

	carobiner::write_files(path, meta, d)
}


