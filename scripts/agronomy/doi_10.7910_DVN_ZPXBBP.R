# R script for "carob"


carob_script <- function(path) {

"In rural Mali shortage of livestock feed is a challenging phenomenon worsening day by day, particularly in the villages of Kani and Noumpinesso. The significant decrease in crops yield and livestock are due to persistent and continuous land degradation and over grazing. Soil water erosion and inappropriate or ineffective farming systems led to land degradation over the many years. A purposeful growing of fodder plant (fast growing trees species) and crops in interacting combinations for a range of benefits would be required using a technology that is easily adaptable by the rural farming communities. Contour ridge (CR) technology is a holistic approach that protects farmlands from erosion; increases soil moisture, 	nutrient availability for crops and associated fast growing trees. The CR technology in combination with forage and improved crop production systems were applied in Kani and Noumpinesso villages of southern Mali.

Data was taken in 40 fields (replications), for the two years of research we took data in CR plot and NCR plot for each replication on different crops (cotton, maize, sorghum, and millet) growth and yield. Experimental design was couple design as crops were seedling in two neighboring plots (CR plot and NCR plot) for each replication. Five yield squares were placed in each plot to take data. For the 40 replications crop used depended on the owner of farm for each replication. Crops used were sorghum, millet, cotton and maize."


	uri <- "doi:10.7910/DVN/ZPXBBP"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "IER", #Institut d’Economie Rurale
		publication = "doi:10.5296/jas.v9i2.18513",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method",
		response_vars = "none", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-07-23"
	)
	
	process_data <-function(fname, crop, yield_part) {
	  f <- ff[basename(ff) == fname]
	  r <- read.csv(f)
	  
	  colnames(r) <- gsub("Farmers.code", "Farmers", colnames(r))
	  colnames(r) <- gsub("Height_m|Height_|Height.m", "height", colnames(r))
	  colnames(r) <- gsub("Grain_kgPerha|Grain.kg.ha|Cotton.yield.kg.ha", "yield_kgPerha", colnames(r))
	  colnames(r) <- gsub("Biomass_kgPerha|Biomass_.kgPerha", "Biomass.kg.ha", colnames(r))
	  
	  data.frame( 
	    trial_id = as.character(r$Farmers), 
	    land_prep_method = r$Treatment, 
	    plant_height = r$height * 100, 
	    yield = r$yield_kgPerha, 
	    fwy_total = r$Biomass.kg.ha, 
	    crop=crop, 
	    yield_part=yield_part
	  )
	}

	d1 <- process_data("005_cotton.csv", "cotton", "seed")
	d2 <- process_data("006_maize.csv", "maize", "grain")
	d3 <- process_data("007_millet.csv", "millet", "grain")
	d4 <- process_data("008_sorghum.csv", "sorghum", "grain")
	
	d <- carobiner::bindr(d1, d2, d3, d4)
	d$on_farm = TRUE 
    d$is_survey = FALSE 
	d$irrigated = FALSE

	d$country <- "Mali"
	d$site <- "Koutiala"
	d$adm1 <- "Nafanga"
	d$location <- "Kani"
	d$latitude <- 12.2453
	d$longitude <- -5.1828
	d$geo_from_source = FALSE
	d$elevation <- 384

   d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$planting_date <- "2017"
	d$harvest_date <- "2018"
	
   d$land_prep_method[d$land_prep_method=="CB"] <- "ridge tillage" 
   d$land_prep_method[d$land_prep_method=="NCB"] <- "conventional"
   d$land_prep_method[d$land_prep_method==""] <- "unknown"

	#duplicates in crop=="cotton"
	d <- unique(d)
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}



