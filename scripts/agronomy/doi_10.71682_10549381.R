# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Details on the fertilizer used are missing 

carob_script <- function(path) {

"
Weed diversity, richness, evenness, and their relationship to various cropping systems' biomass, tested on-station with/without fertilizer application on two soils in Zimbabwe between 2022 and 2023

This database contains weed density, weed biomass, crop biomass, richness, evenness, and diversity data from on-station trials in Zimbabwe conducted at Domboshava Training Centre (DTC; 17.62°S,31.17° E) and the University of Zimbabwe farm (UZ: 17.73°S, 31.020 E), characterised by different soil types. It tested maize monocropping, maize-legume rotations, and intercropping with various layouts (traditional intercropping and double row strip cropping), with and without fertilizer application. Two contrasting legumes, cowpea and pigeon pea, were tested in the cropping systems.  The experiment was established during the 2019/2020 growing season, but data was collected from the 2021/22 to 2022/23 season to allow the rotations to develop.  The experiment was carried out under Conservation Agriculture and rainfed conditions. The dataset is divided into three parts: (1) field measurement data, including treatments, weed biomass, and crop biomass; (2) calculations at the cropping system level (density, richness, Pielou, and Shannon diversity), and (3) weed species abundance (all the weeds identified and collected including those that were difficult to identify and termed 'Other'.)
"
	uri <- "doi:10.71682/10549381"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT; UZIM",
		publication = NA,
		project = NA,
		carob_date = "2026-05-05",
		design = NA,
		data_type = "on-station experiment",
		treatment_vars = "intercrops; crop_rotation",
		response_vars = "fwy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Masamba et al_weeds_data 19_12_2025.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="NOTES")
	r2 <- carobiner::read.excel(f1, sheet="WEED SPECIES")
	r3 <- carobiner::read.excel(f1, sheet="DATA")

	d <- data.frame(
		planting_date = substr(r3$year, 1, 4),
		location = r3$site,
		trial_id = r3$plot,
		rep = as.integer(gsub("DTC\\.|UZ\\.", "", r3$replicate)),
		intercrops =ifelse(grepl("MZPP1|MZPP2", r3$cropping.system), "pigeon pea",
		                   ifelse(grepl("MZCP2", r3$cropping.system), "cowpea", "none")) ,
		intercrop_type = ifelse(grepl("MZCP2|MZPP2", r3$cropping.system), "strip", 
		                       ifelse(grepl("MZPP1", r3$cropping.system), "unknown", "none")) ,
		crop_rotation = ifelse(grepl("MZ-CP", r3$cropping.system), "maize;cowpea", 
		                       ifelse(grepl("MZ-PP", r3$cropping.system), "maize;pigeon pea", "none")),
		
		treatment = r3$fertilizer,
		crop = trimws(gsub("\\+ cowpea|\\+cowpea|\\+ pigeon pea", "", r3$crop)),
		fwy_total = r3$crop.biomass,
		weed_biomass = r3$weed.biomass,
		weed_density = r3$density,
		yield = as.numeric(NA),
		country = "Zimbabwe",
		on_farm = FALSE, 
		is_survey = FALSE,
		yield_part = "none", 
		yield_moisture = as.numeric(NA), 
		irrigated = NA, 
		geo_from_source = TRUE, ## From description
		yield_isfresh = TRUE	
	)
	
	### Fixing crop names 
	d$crop <- gsub("cowpa", "cowpea", d$crop)
	d$crop <- gsub("pigeonpea", "pigeon pea", d$crop)
	
	### Adding Lon and Lat coordinate
	
	geo <- data.frame(
	   location = c("DTC", "UZ"),
	   longitude = c(31.17, 31.020),
	   latitude = c(-17.62, -17.73)
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	

	carobiner::write_files(path, meta, d)
}


