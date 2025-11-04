# R script for "carob"


carob_script <- function(path) {

"The data set evaluates adaptability and suitability of groundnut varieties to different ecozones."

	uri <- "doi:10.7910/DVN/MY0ATH"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4, 
		data_organization = "IITA;GMOFA;SARI", 
		publication = NA, 
		project = NA, 
		data_type = "experiment", 
		response_vars = "yield",
		treatment_vars = "P_fertilizer;variety;longitude;latitude", 
		carob_contributor = "Mitchelle Njukuya", 
		carob_date = "2024-06-20"
	)
	
##### PROCESS data records

	f <- ff[basename(ff) == "003_phosphorusRateonGroundnutMother_Ghana.xls"]
	f1 <- ff[basename(ff) == "002_phosphorusRateonGroundnut_Ghana.xls"]
	f2 <- ff[basename(ff) == "001_phosphorusRatesonGroundnutMorther_Ghana_2014-2015.xls"]
	
	r1 <- carobiner::read.excel(f, sheet = "RAW 2014")
	r2 <- carobiner::read.excel(f, sheet = "RAW 2015")
	r3 <- carobiner::read.excel(f1, sheet = "RAW 2014")
	r4<- carobiner::read.excel(f1, sheet = "RAW DATA 2015")
	r5 <- carobiner::read.excel(f2, sheet = "SAS 2014")
	r6 <- carobiner::read.excel(f2, sheet = "SAS 2015")

## process file(s)

	d1 <- data.frame(
		date="2014", 
		site=r1$DSTRT, 
		location=r1$CMMTY...2, 
		P_fertilizer=r1$PRATE, 
		variety=r1$VARTY, 
		plant_height=r1$`PLNTH (cm)`, 
		flowering_days=r1$DFFLW, 
		yield=r1$`GYD/ha`
	
	)
	
	d2 <- data.frame(
	  date="2015", 
	  site=r2$DSTRT, 
	  location=r2$CMMTY...2, 
	  P_fertilizer=r2$PRATE, 
	  variety=r2$VARTY, 
	  plant_height=r2$PLNTH, 
	  yield=r2$`GYD /ha`, 
	  fwy_residue=r2$`FODDER /ha`, 
	  dmy_total=r2$TBMSS
	)
	#fixing names as given in label sheet
	d1$site <- c("Kassena Nankana", "Bongo")[d1$site]
	d2$site <- c("Kassena Nankana", "Bongo")[d2$site]
	d1$variety <- c("chinese", "cri-azivivi", "obolo", "manipinta", "yenyawoso")[d1$variety]
	d2$variety <-c("chinese", "cri-azivivi", "obolo", "manipinta", "yenyawoso")[d2$variety]
	
	d3 <- data.frame(
	  date="2014", 
	  site=r3$DSTRT, 
	  location=r3$CMMTY...2, 
	  P_fertilizer=r3$PRATE, 
	  variety=r3$VARTY, 
	  plant_height=r3$PLNTH, 
	  flowering_days=r3$DFFLW, 
	  yield=r3$GYD...17, 
	  fwy_residue=r3$FODDER...13, 
	  dmy_total=r3$TBMSS
	)
	
	d4 <- data.frame(
	  date="2015", 
	  site=r4$DSTRT, 
	  location=r4$CMMTY, 
	  P_fertilizer=r4$PRATE, 
	  variety=r4$VAR, 
	  plant_height=r4$PLNTH, 
	  flowering_days=NA, 
	  yield=r4$GYD...10, 
	  fwy_residue=r4$STOVER...12, 
	  dmy_total=r4$TBMSS
	)

	#fixing names as given in label sheet
	d3$site <- c("Tolon", "Savelugu")[d3$site]
	d4$site <- c("Tolon", "Savelugu")[d4$site]
	d4$location <- c("Tingoli", "Gbanjong", "Tibogunayili", "Duko")[d4$location]
	d3$variety <- c("chinese", "cri-azivivi", "obolo", "manipinta", "yenyawoso")[d3$variety]
	d4$variety <-  c("chinese", "cri-azivivi", "obolo", "manipinta", "yenyawoso")[d4$variety]
	
	d5 <- data.frame(
	  date="2014", 
	  site=r5$DSTRT, 
	  location=r5$CMMTY, 
	  P_fertilizer=r5$PRATE, 
	  variety=r5$VARTY, 
	  plant_height=r5$PLNTH, 
	  flowering_days=r5$DFFLW, 
	  yield=r5$GRAIN, 
	  fwy_residue=r5$FODDER, 
	  dmy_total=r5$TBMM
	)
	
	d6 <- data.frame(
	  date="2015", 
	  site=r6$DSTRT, 
	  location=r6$CMMTY, 
	  P_fertilizer=r6$PRATE, 
	  variety=r6$VARTY, 
	  plant_height=r6$PLNTH, 
	  flowering_days=NA, 
	  yield=r6$GRAIN, 
	  fwy_residue=r6$FODDER, 
	  dmy_total=r6$TBMM
	)

	#fixing names as given in label sheet
	d5$site <- c("Nadowli", "Wa West")[d5$site]
	d6$site <- c("Nadowli", "Wa West")[d6$site]
	d5$location <- c("Goroyori", "Gyilli", "Zanko", "Pase")[d5$location]
	d6$location <- c("Goroyori", "Gyilli", "Zanko", "Pase")[d6$location]
	d5$variety <- c("G1-Sari Chinese", "G2-Cri-Azivivi", "G3-Obolo", "G4-Manipinta", "G5-Yenyawoso")[d5$variety]
	d6$variety <- c("G1-Sari Chinese", "G2-Cri-Azivivi", "G3-Obolo", "G4-Manipinta", "G5-Yenyawoso")[d6$variety]	

	d <- carobiner::bindr(d1, d2, d3, d4, d5, d6)	
	
	d$P_fertilizer <- c(60, 90)[d$P_fertilizer]
# is this correct: ?
	##CN : I thing set to NA might be a good , as the data do not provide any information on the N and K used. 
	d$N_fertilizer <- 0
	d$K_fertilizer <- 0
	
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA # FALSE ?
	d$inoculated <- FALSE
	d$crop <- "groundnut"
	d$yield_part <- "seed"
	d$trial_id <- "1"
	d$planting_date <- d$date
	

##### Location #####
	d$country <- "Ghana"
	##CN 
	## fix location names
	p <- carobiner::fix_name(d$location)
	p <- gsub("Goroyori", "Gorogori", p)
	p <- gsub("Gyilli", "Gyilly", p)
	p <- gsub("Nyangua", "Nyangai", p)
	d$location <- p
	geo <- data.frame(
	  location=c("Bonia", "Nyangai", "Samboligo", "Tingoli", "Gbanjong", "Tibogunayili", "Duko", "Gorogori", "Gyilly", "Zanko", "Pase"), 
	  latitude=c(10.9078, 10.8950, 10.9078, 9.3750, 9.4319, 9.6242, 9.5636, 10.0606, 10.0606, 10.0676, 10.0367), 
	  longitude=c(-0.8081, -1.0939, -0.8081, -1.0095, -1.0658, -0.8253, -0.8357, -2.5017, -2.5017, -2.5959, -2.7109)
	)
	d$geo_from_source <- FALSE
	d <- merge(d, geo, by="location", all.x = TRUE)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}


