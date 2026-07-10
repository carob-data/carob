# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Advanced drought tolerant sorghum hybrids at Shiraro 2019

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for advanced drought tolerant hybrids evaluated at Shiraro, Ethiopia in 2019
"

	uri <- "doi:10.7910/DVN/HIJIHU"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = "NA",
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;seed_weight;plant_height;flowering_days;maturity_days", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-10",
		carob_completion = 90,	
		carob_effort = 1
	)
	

	f <- ff[basename(ff) == "Advanced drought tolerant sorghum hybrids at Shiraro 2019.xlsx"]

	r <- carobiner::read.excel(f)

	d <- data.frame(
	  country = "Ethiopia",
	  adm1 = NA,
	  adm2 = NA,
	  adm3 = "Tahtay Adiyabo",
	  location = r$Site,
	  plot_id = as.character(r$Plot),
	  treatment = r$Genotype,
	  #variety = as.character(r$Genotype)
	  plant_height = r$PHT,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  yield = r$`Yield Kg/Ha`,
	  seed_weight = r$`100GW`*10,
	  crop = "sorghum"
	)
	
	d$location[d$location == "SH"] <- "Shiraro"

	d$planting_date <- "2019"
	d$harvest_date <- "2019"
	d$trial_id <- "1"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <-FALSE
	
	## GADM for record keeping
	#Shiraro is not yet available in GADM so i used coordinates from google maps
	#geo <- data.frame(
	  #adm3 = c("Tahtay Adiyabo"),
	  #longitude = c(37.7897),
	  #latitude = c(14.4267),
	  #geo_uncertainty = c(49096),
	  #geo_source = c("GADM 4.1, adm3")
	#)
	
	
	d$longitude <- 37.933
	d$latitude <- 14.400
	d$geo_uncertainty <- 49096 #geo_uncertainity for this location was replaced from adm3
	d$geo_source <- "GADM 4.1, adm3" #only geo_uncertainty was taken from this source
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA


	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	carobiner::write_files(path, meta, d)
}

