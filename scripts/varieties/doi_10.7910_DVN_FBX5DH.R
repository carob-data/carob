# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Cassava Breeding Trials - Edaphoclimatic Zone 2: Lowland Tropics; Acid Soils; Medium to High Precipitation

Cassava germplasm development at CIAT is centered on the development of improved gene pools for specific edapho-climatic zones with importance for cassava production. The most relevant ecosystems are the semi-and and sub-humid tropics, for which the majority of efforts are devoted. The main selection activity is conducted in sites selected to represent the conditions of the target ecosystem. For each zone, a recurrent selection program with a progressive set of stages is followed. As the stages progress more emphasis is given to traits of lower heritability, because more planting material for each genotype is available and the evaluation can be conducted in bigger plots with replications. Certain selection criteria are of general Importance across ecosystems (i.e. yield potential, dry matter content), while others are specific for each ecosystem (i e. specific pests and/or diseases).     This dataset provides CIAT cassava breeding trial data for activities conducted in the Acid soil savannas; moderate to long dry season; low relative humidity during dry season, Zone.
"

	uri <- "doi:10.7910/DVN/FBX5DH"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=7,
		data_organization = "CIAT",
		publication = "handle:10568/68900",
		project = NA,
		carob_date = "2026-03-12",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "2.Cassava-trials-amount.xls"]
	f2 <- ff[basename(ff) == "3.Cassava-traits-dictionary.xls"]
	f3 <- ff[basename(ff) == "4.01.All-Type-Trials_Locations.xlsx"]
	f4 <- ff[basename(ff) == "5.01.Clonal-Evaluation_Locations.xlsx"]
	f5 <- ff[basename(ff) == "6.01.Preliminary-Yield_Trials_Locations.xlsx"]
	f6 <- ff[basename(ff) == "7.01.Advanced-Yield_Trials_Locations.xlsx"]
	f7 <- ff[basename(ff) == "9.01.Regional_Trials_Locations.xlsx"]
	f8 <- ff[basename(ff) == "4.02.All-Type-Trials_Data.csv"]
	f9 <- ff[basename(ff) == "5.02.Clonal-Evaluation_Data.csv"]
	f10 <- ff[basename(ff) == "6.02.Preliminary-Yield_Trials_Data.csv"]
	f11 <- ff[basename(ff) == "7.02.Advanced-Yield_Trials_Data.csv"]
	f12 <- ff[basename(ff) == "9.02.Regional_Trials_Data.csv"]

	r1 <- carobiner::read.excel(f1, na= "")
	r2 <- carobiner::read.excel(f2, na= "")
	r3 <- carobiner::read.excel(f3,  na= "")
	r4 <- carobiner::read.excel(f4, na= "")
	r5 <- carobiner::read.excel(f5, na= "")
	r6 <- carobiner::read.excel(f6, na= "")
	r7 <- carobiner::read.excel(f7, na= "")
	r8 <- read.csv(f8, na= "")
	r9 <- read.csv(f9, na= "")
	r10 <- read.csv(f10, na= "")
	r11 <- read.csv(f11, na= "")
	r12 <- read.csv(f12, na= "")

	
### process 
	
	d1a <- data.frame(
		trial_id = r3$TRIAL_NAME,
		location = r3$LOCATION_NAME,
		longitude = r3$LONGITUD,
		latitude = r3$LATITUDE,
		elevation = r3$ALTITUDE,
		planting_date = r3$PLANT_DATE,
		harvest_date = r3$HARVEST_DATE
	)
	
	d1b <- data.frame(
	  trial_id = r8$TRIAL,
	  variety = r8$DESIGNATION,
	  variety_pedigree = r8$CROSS,
	  variety_code = as.character(r8$GID),
	  rep = r8$REP_NO,
	  harvest_index = r8$HI,
	  yield = r8$YIELDH,
	  plant_height = r8$PHEIGH,
	  yield_moisture = r8$DMATTE
	)
	
d1 <- merge(d1a, d1b, by="trial_id", all = TRUE)	
	
	d2a <- data.frame(
	  trial_id = r4$TRIAL_NAME,
	  location = r4$LOCATION_NAME,
	  longitude = r4$LONGITUD,
	  latitude = r4$LATITUDE,
	  elevation = r4$ALTITUDE,
	  planting_date = r4$PLANT_DATE,
	  harvest_date = r4$HARVEST_DATE
	)

	d2b <- data.frame(
	  trial_id = r9$TRIAL,
	  variety = r9$DESIGNATION,
	  variety_pedigree = r9$CROSS,
	  variety_code = as.character(r9$GID),
	  rep = r9$REP_NO,
	  harvest_index = r9$HI,
	  yield = r9$YIELDH,
	  plant_height = r9$PHEIGH,
	  yield_moisture = r9$DMATTE
	)
	
	d2 <- merge(d2a, d2b, by="trial_id", all = TRUE)
	
	####
	d3a <- data.frame(
	  trial_id = r5$TRIAL,
	  location = r5$LOCATION_NAME,
	  longitude = r5$LONGITUD,
	  latitude = r5$LATITUDE,
	  elevation = r5$ALTITUDE,
	  planting_date = r5$PLANT_DATE,
	  harvest_date = r5$HARVEST_DATE
	)
	
	
	d3b <- data.frame(
	  trial_id = r10$TRIAL,
	  variety = r10$DESIGNATION,
	  variety_pedigree = r10$CROSS,
	  variety_code = as.character(r10$GID),
	  rep = r10$REP_NO,
	  harvest_index = r10$HI,
	  yield = r10$YIELDH,
	  plant_height = r10$PHEIGH,
	  yield_moisture = r10$DMATTE
	)
	
	d3 <- merge(d3a, d3b, by="trial_id", all = TRUE)
	
	####
	
	####
	d4a <- data.frame(
	  trial_id = r6$TRIAL,
	  location = r6$LOCATION_NAME,
	  longitude = r6$LONGITUD,
	  latitude = r6$LATITUDE,
	  elevation = r6$ALTITUDE,
	  planting_date = r6$PLANT_DATE,
	  harvest_date = r6$HARVEST_DATE
	)
	
	####
	d4b <- data.frame(
	  trial_id = r11$TRIAL,
	  variety = r11$DESIGNATION,
	  variety_pedigree = r11$CROSS,
	  variety_code = as.character(r11$GI),
	  rep = r11$REP_NO,
	  harvest_index = r11$HI,
	  yield = r11$YIELDH,
	  plant_height = r11$PHEIGH,
	  yield_moisture = r11$DMATTE
	)
	
	d4 <- merge(d4a, d4b, by="trial_id", all = TRUE)
	
	
	d5a <- data.frame(
	  trial_id = r7$TRIAL,
	  location = r7$LOCATION_NAME,
	  longitude = r7$LONGITUD,
	  latitude = r7$LATITUDE,
	  elevation = r7$ALTITUDE,
	  planting_date = r7$PLANT_DATE,
	  harvest_date = r7$HARVEST_DATE
	)
	
	
	d5b <- data.frame(
	  trial_id = r12$TRIAL,
	  variety = r12$DESIGNATION,
	  variety_pedigree = r12$CROSS,
	  variety_code = as.character(r12$GID),
	  rep = r12$REP_NO,
	  harvest_index = r12$HI,
	  yield = r12$YIELDH,
	  plant_height = r12$PHEIGH,
	  yield_moisture = r12$DMATTE
	)
	
	d5 <- merge(d5a, d5b, by="trial_id", all = TRUE)
	
	#### combine d1, d2, d3, d4 and d5
	
	d <- carobiner::bindr(d1, d2, d3, d4, d5)
	### drop rows with NA in yield 
	d <- d[!is.na(d$yield),]
	### Fixing Lon and lat coordinate 
	i <- d$longitude
	j <- d$latitude
	d$latitude <- as.numeric(substr(i,1, 2))+ as.numeric(substr(i,3, 4))/60
	d$longitude <- -(as.numeric(substr(j,1, 2))+ as.numeric(substr(j,3, 4))/60)
	#### data type
	d$yield <- d$yield*1000 ## kg/ha
	d$planting_date <- as.character(d$planting_date)
	d$harvest_date <- as.character(d$harvest_date)
	d$variety <- trimws(d$variety)
	d$variety_pedigree <- trimws(d$variety_pedigree)
	d$location <- carobiner::fix_name(d$location, "title")
	
	d$country <- "Colombia"
	d$on_farm <- TRUE
	d$crop <- "cassava"
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	d$yield_part <- "roots"
	d$yield_isfresh <- TRUE
	d$irrigated <- NA
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	## drop two rows with yield_moisture more than 100% 
	d <- d[which(d$yield_moisture <=100),]
	
	## remove duplicate records
	d <- unique(d)
	
carobiner::write_files(path, meta, d)

}


