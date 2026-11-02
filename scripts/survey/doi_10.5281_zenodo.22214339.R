# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Data supporting: Conservation Agriculture effects on soil biological functioning and crop productivity in smallholder maize-bean systems of central Mozambique

Dataset and reproducible analysis files supporting the study on Conservation Agriculture, soil organic carbon, microbial biomass carbon, and crop productivity in smallholder farming systems of central Mozambique. The repository contains the datasets, statistical results, R scripts, and documentation used to generate the tables reported in the associated research manuscript. The analyses examine socioeconomic characteristics, soil properties, crop yield changes, maize and bean productivity, microbial biomass carbon, and the relationship between conservation-intensity management and maize yield. The dataset includes information from 360 smallholder farmers from central Mozambique, including farmers practicing Conservation Agriculture and conventional agriculture. Soil variables include soil organic carbon stock and microbial biomass carbon, together with crop productivity measurements. The repository is organized by analytical table. Each table folder contains the relevant data, results, scripts, and README documentation where applicable. The R scripts provide reproducible workflows for the statistical analyses and generation of the reported results. The repository is intended to support transparency, reproducibility, and reuse of the research findings. &nbsp;
"


	uri <- "doi:10.5281/zenodo.22214339"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		data_organization = "HARAU;UGRE;UEM;IIAM", # UEM: Eduardo Mondlane University, Mozambique
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "land_prep_method",
		response_vars = "yield;soil_SOC;soil_N;soil_MBC", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-22",
		carob_completion = 80,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "Table_2_Analysis_Dataset.csv"]
	f2 <- ff[basename(ff) == "Table_3_Analysis_Dataset.csv"]
	f3 <- ff[basename(ff) == "Table_4_Analysis_Dataset.csv"]
	f4 <- ff[basename(ff) == "Table_5_Analysis_Dataset.csv"]
	f5 <- ff[basename(ff) == "Table_6_Analysis_Dataset.csv"]


	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)

	#### Process
	d1 <- data.frame(
		hhid = as.character(r1$Farmer_ID),
		age = r1$Age,
		farmland = r1$Farmsize,
		#education = r1$EduLevel,
		#civil_status = r1$Marital_status
		farmer_gender = c("male", "female")[r1$Gender]
	)
	
	#### Effects of Conservation Agriculture on soil properties, microbial biomass, and crop productivity
	
	d2 <- data.frame(
	  hhid = as.character(r2$ID),
	  treatment = r2$Management,
	  depth_bottom = as.numeric(gsub("-", "", substr(r2$Depth_factor, 3,5))) ,
	  depth_top = as.numeric(gsub("-", "", substr(r2$Depth_factor, 1,2))) ,
	  soil_C_stock = r2$SOC_stock*100, # from t/ha to g/m2
	  soil_N = r2$N,
	  soil_MBC = r2$MBC_mgC_kg,
	  soil_bd = r2$Bulk_Density,
	  yield_maize = r2$Maize_Harvest_Ha,
	  yield_Bean = r2$BeanYield_Campaign_Ha
	)
	
	### merge d1 and d2
	dd <- merge(d1, d2, by= "hhid", all = TRUE)
	dd <- reshape(dd, varying = c("yield_maize", "yield_Bean"), v.names = "yield", timevar = "crop", times = c("maize", "common bean"), direction = "long")
	dd$id <- NULL
	
	##### Changes in crop yield over time under CA and Non_CA management categories.
	d3 <- data.frame(
	  hhid = as.character(r3$Farmer_ID),
	  treatment = r3$Management,
	  yield_maize2 = r3$Maize_Harvest10_Ha,
	  yield_maize1 = r3$Maize_Harvest_Ha,
	  yield_Bean2 = r3$BeanYield_Campaign10_Ha*1000,
	  yield_Bean1 = r3$BeanYield_Campaign_Ha*1000
	)
	
	d3 <- reshape(d3, varying = c("yield_maize1", "yield_maize2", "yield_Bean1", "yield_Bean2"), v.names = "yield", 
	              timevar = "CA_years",
	              direction = "long")
	d3$crop <- c(rep("maize", 2), rep("common bean", 2))[d3$CA_years]
	d3$CA_years <- c(1, 10, 1, 10)[d3$CA_years]
	d3$id <- NULL
	
	### merge d1 and d3
	
	dd1 <- merge(d3, d1, by = "hhid", all = TRUE)
	
####### 
	d4 <- data.frame(
	  treatment = r4$Tilla_Pract,
	  soil_C_stock = r4$SOC_stock*100, # from t/ha to g/m2
	  soil_MBC = r4$MBC_mgC_kg,
	  soil_bd = r4$Bulk_Density,
	  soil_N = r4$N,
	  soil_P = r4$POlsen,
	  soil_clay = r4$Clay,
	  soil_sand = r4$Sand,
	  location = r4$District
	)
	### drop negative value on soil clay
	
	d4$soil_clay[d4$soil_clay < 0] <- NA
	
	### merge dd and d4 (adding location)
	dd <- merge(dd, d4, by = c("treatment", "soil_C_stock", "soil_N", "soil_MBC", "soil_bd"), all.x  = TRUE)
  
	### Adding location 
	dd1  <- merge(dd1, unique(dd[, c("hhid", "location")]), by= "hhid", all.x = TRUE)
	
	### combine dd and dd1
	d <- carobiner::bindr(dd, dd1)
	d$land_prep_method <- ifelse(grepl("^CA$", d$treatment), "minimum tillage", "conventional") 
	
	### drop hhid = 306 with does not have location
	d <- d[!is.na(d$location),]
	
	#### Adding lon and lat coordinate 
	
	geo <- data.frame(
	  location = c("Gondola", "Sussundenga", "Vanduzi"),
	  longitude = c(33.6608, 33.2628, 33.3921),
	  latitude = c(-19.1505, -19.6709, -18.9457),
	  geo_uncertainty = c(57936, 69543, NA),
	  geo_source = c(rep("GADM 4.1, adm2", 2), "Google Maps"),
	  geo_from_source = FALSE
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	d$is_survey <- TRUE
	d$on_farm <- FALSE
	d$trial_id <- "1"
	d$yield_moisture <- NA
	d$yield_part <- "none"
	d$country <- "Mozambique"
	d$irrigated <- NA
	d$planting_date <- NA
	d$harvest_date <-  NA 
	d$yield_isfresh <- TRUE
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	

	carobiner::write_files(path, meta, d)
}

