# R script for "carob"


carob_script <- function(path) {

"The project proposes to use win-win public-private partnership approaches to disseminate improved legume seeds and complementary crop management practices developed under PARTI collaboration over the past six years. Through PARTI partnerships, 5 varieties of soybean were released. Most of the varieties are drought tolerant, resistant to endemic pests and diseases, have end-user preferred traits, and show significant increases in yields on farmers’ fields."
  
	uri <- "doi:10.25502/HNKM-Y645/D"
	group <- "agronomy"
	ff	 <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa", 
		publication= NA, 
		data_organization = "IITA;ICRAF;WUR", 
		carob_contributor="Effie Ochieng'", 
		carob_date="2023-07-12",
		data_type="on-farm experiment",
		response_vars= "yield",
		treatment_vars="N_fertilizer;P_fertilizer;inoculated"				
	)

	f <- ff[basename(ff) == "Canon data.csv"]
	r1 <- read.csv(f)
	f <- ff[basename(ff) == "Biomass Analysis.csv"]
	r2 <- read.csv(f)
		
	from1 <- c("Country", "Loc", "Treatment", "Variety", "pH_value", "Nitrogen", "Rep", "Yld_FW_kg_ha", "Harvested_Biomass_kg_ha", "Days_to_Flowering_R1", "Days_to_Full_Maturity_R8", "Days_to_Harvest", "Season")
	d1 <- carobiner::change_names(r1[,from1], from1,
		c("country", "adm1", "treatment", "variety", "P_fertilizer", "N_fertilizer", "rep", "yield", "fwy_residue", "flowering_days", "maturity_days", "harvest_days", "season"))
	
	#d1$inoculated <- ifelse(d1$Inoculant == "Yes", TRUE, FALSE)
	d1$inoculated <- r1$Inoculant == "Yes"

	d1$harvest_days[d1$harvest_days == 0] <- NA
	
	d1$seed_weight <- 1000 * r1$Seed_Weiht_sqm_g / r1$Seeds_sqm # for 1000 seeds
	d1$plant_density <- 10000 * r1$PLST #to get plant population/ha
	d1$plant_height <- r1$Avg_plant_height_cm # EGB: correcting back to cm
	
	d1$trial_id <- paste(1:nrow(d1), d1$adm1, d1$treatment, sep = "_")
	
	# second data set
	## note that P-fertilizer was labeled as pH_value
	from2 <- c("Country", "Loc", "Treatment", "Variety", "pH_value", "Nitrogen", "Rep", "Yld_FW_kg_ha", "Harvested_Biomass_kg_ha", "NOD_WT", "Season")
	d2 <- carobiner::change_names(r2[ from2], from2, 
		c("country", "adm1", "treatment", "variety", "P_fertilizer", "N_fertilizer", "rep", "yield", "fwy_residue", "nodule_weight", "season"))
	d2$plant_density <- 10000 * r2$PLST
	d2$trial_id <- paste(1:nrow(d2), d2$adm1, d2$treatment, sep = "_")

	d2$inoculated <- grepl("Inoc", d2$treatment)

	d1$nodule_weight <- NA
	d2$flowering_days <- d2$seed_weight <- d2$plant_height <- d2$maturity_days <- d2$harvest_days <- NA
	
	dd <- rbind(d1, d2)		
	
	geo <- data.frame(
		country = c("Malawi", "Mozambique", "Mozambique", "Zambia", "Zambia", "Malawi", "Mozambique"), 
		adm1 = c("Bvumbwe", "Angonia", "Ruace", "Chipata", "Lusaka", "Chitedze", "Namarripe"), 
		longitude = c(35.0267, 34.1445, 36.7011, 32.6458, 29.3143, 33.6538, 38.92), 
		latitude = c(-15.9428, -14.7690, -15.1963, -13.6391, -15.3066, -13.9788, -15.23056),
		geo_from_source = FALSE
	)
	
	dd <- merge(dd, geo, by = c("country", "adm1"), all.x = TRUE, sort = FALSE)
	
	#start and end date info obtained from the dictonary 
	dd$planting_date <- ifelse(dd$season =="Y1617S", "2016", "2017")
	dd$harvest_date	 <- ifelse(dd$season =="Y1617S", "2017", "2018")
	dd$yield_part <- "seed"
	dd$on_farm <- TRUE
	dd$is_survey <- FALSE
	dd$crop <- "soybean"
	dd$K_fertilizer <- 0

	#what is the nodule_weight unit you are using? 
	# efyrouwa: nodule weight here is dry weight in milligrams 
	# RH miligram per ?. We need to express it per unit area
	
	# EGB:
	# Removing 1 single observation without yield...
	dd <- dd[!is.na(dd$yield), ]
	

	dd$irrigated <- NA
	
	dd$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, dd, path=path)

}
	
	 
