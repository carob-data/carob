# R script for "carob"

## ISSUES
# efyrouwa: how do we incorporate weed information??

carob_script <- function(path) {

"Weed development is one of the major constraints to cereal cropping systems in Southern Africa with potential severe crop losses. Understanding weed community responses to different conservation agriculture (CA) components (i.e., no-tillage, NT; crop rotation, R; and mulching, M) and/or their combinations is crucial in Southern Africa where farmers apply different combinations depending on local context. Here, for the first time, we assessed how weed density, community diversity and structure respond to different combinations of CA components[conventional tillage (CT), CT+M, CT+R, CT+M+R, NT, NT+M, NT+R, NT+M+R]. The study was carried out over three seasons at two locations with contrasting soil textures i.e., clayish, and sandy. At the sandy location, across seasons, weed density (number of individuals per unit area) and community diversity (distribution of individuals within the species) were significantly and positively affected by precipitation and not by cropping system. Weed richness (number of species) was affected by the interaction of season and cropping system, with the highest number of weed species being recorded in the NT+M+R system in the seasons with medium to high precipitation. At the clayish location, an opposite pattern was observed, and weed density was lower in    seasons with medium-high precipitation than under low precipitation. Weed community diversity was 50% higher under NT+M than under CT+R, whereas weed species richness decreased with the increase of precipitation. At both locations, the implementation of rotation and mulching either in NT or CT systems resulted in the modification of the structure of weed community with respect to CT and NT alone, and these CA combinations were associated with highest maize grain yield. Overall, eight weed species common to both locations were responsible for most of the community   structure differences among cropping systems. Structural equation modelling showed that at the sandy location precipitation did not affect grain yield, but positively affected weed density, diversity, evenness, and richness. By contrast, at the clay location, precipitation positively affected grain yield, but did not modify weed density and evenness, and reduced weed community diversity and richness. At this location, weed density negatively affected grain yield. The differential weed-crop relationship supports the need to find a site-specific equilibrium between the control of weeds and the maintenance of their diversity"

	uri <- "hdl:11529/10548829"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		project= NA,
		publication= "doi:10.1016/j.fcr.2022.108724",
		data_organization = "CIMMYT;SSSA",
		data_type="on-farm experiment", 
		carob_contributor="Hope Mazungunye"  ,
		carob_date="2023-09-17",
		modified_by = "Effie Ochieng'",
		treatment_vars = NA,
		response_vars = "yield"
	)

 # path <- ("C:/carob/wd/data/raw/maize_trials")

	# f <- "C:/carob/scripts/maize_trials/path/to/your/carob/folder/data/raw/maize_trials/doi_11529_10548829"
  # library("readxl")
	##DATA_DTC_UZ_2019_2020_2021 <- read_excel("path/to/your/carob/folder/data/raw/maize_trials/doi_11529_10548829/DATA DTC UZ - 2019 2020 2021.xlsx", 
	   #                                      +     sheet = "Raw Data")
	# r <- read_excel(f)
	# r <- DATA_DTC_UZ_2019_2020_2021 
	
	
	# efyrouwa: read the file as below instead, path should never be in the script
	
	f <- ff[basename(ff) == "DATA DTC UZ - 2019 2020 2021.xlsx"]
	r <- carobiner::read.excel(f, sheet = 3)
	
  ## process file
	r$site <- r$Location
	r$country <- "Zimbabwe"
	r$adm1 <- ifelse(r$site == "DTC", "Mashonaland_east", "Harare")
	r$adm2 <- ifelse(r$site == "DTC","Goromonzi", "Harare")
	r$latitude <- ifelse(r$site =="DTC", -17.4833, -17.7829)
	r$longitude <- ifelse(r$site =="DTC", 31.1000, 31.0569)
	r$crop <- "maize"
	r$dmy_total <- r$Biomass
	r$yield <- r$Grain
	r$planting_date <- as.character(r$Season)
	r$rep <- r$Replicate
	
	# efyrouwa: define the treatments
	g <- r$System
	g <- gsub("CT","conventional_tillage",g)
	g <- gsub("\\+M", "_plus_mulch", g)
	g <- gsub("\\+R", "_plus_rotation", g)
	g <- gsub("NT", "no_tillage", g)
	r$treatment <- g
	r$yield_part <- "grain"
	r$trial_id <- paste(seq(1:nrow(r)),r$treatment)
	r$on_farm <- TRUE
	
	# efyrouwa: Go through the reference to get more information, mostly in the materials and methods
	# The soil at DTC has clay, sand, and silt contents of 220 g kg−1, 730 g kg−1, and 50 g kg−1, 
	# respectively, SOC of 7.3 g kg−1.
	# The soil at UZ has clay, sand, and silt contents of 400 g kg−1, 390 g kg−1, and 210 g kg−1, respectively, 
	#  SOC content of 16.8 g kg−1
	r$soil_clay <- ifelse(r$site == "DTC",22,40)
	r$soil_sand <- ifelse(r$site == "DTC",73,39)
	r$soil_silt <- ifelse(r$site == "DTC",5, 21)
	r$soil_SOC  <- ifelse(r$site == "DTC",0.73,1.68)
	
	# treatments involving rotation, plots were split into half and maize was sown in a 
	# one-year rotation with cowpea (with phases of the rotation present in each year) 
	# while for monocropping treatments, sole maize was sown. 
	r$crop_rotation <- ifelse(grepl("rotation",r$treatment), "cowpea",NA)
	
	# Maize was sown at an interrow spacing of 90 cm and an intra-row spacing of 25 cm
	r$row_spacing <- 90
	r$plant_spacing <- 25
	
	# At sowing, both maize and cowpea received a basal fertilizer at the rate of 
	# 11.6 kg N ha−1, 10.1 kg P ha−1, 9.6 kg K ha−1
	r$N_fertilizer <- 11.6
	r$P_fertilizer <- 10.1
	r$K_fertilizer <- 9.6
  
	
	d <- r[,c("trial_id", "country","adm1","adm2","latitude","longitude","treatment","crop","dmy_total", "yield","yield_part","planting_date", "on_farm", "soil_clay","soil_sand","soil_silt","soil_SOC","row_spacing","plant_spacing","N_fertilizer","P_fertilizer","K_fertilizer")] 
	

	d$geo_from_source <- FALSE
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$irrigated <- NA
  	carobiner::write_files(meta, d, path=path)
 }
 
