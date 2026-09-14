# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. i intentionally ommitted other files and other variables because there is no dictionary for the full names and units, so i only standardized the more obvious variables
#2. (coordinates in wrong country: Nigeria/Mali, Chad/Senegal, Zambia/Malawi)-   i cannot verify the if the coordinates are wrong or the country is wrong because i have no adm1,2 or 3 to cross reference the points. 
#3. datespan: 279 records with harvest_date within 45 days of planting_date-  these records have their days recorded before the planting date
#4. datespan: 1 harvest_date more than 366 days after planting_date- 1 date record which has a harvest date 466days after planting

carob_script <- function(path) {

"
Pan African Trials Network

This repository contains a comprehensive dataset and full analysis pipeline for understanding soybean cultivar responses to diverse African agroecologies using multi-environment trials (METs) from 2015 to 2024/25.Key features:292 trials across 138 locations in 21 countries366 soybean varietiesAgronomic + nutritional + environmental (soil, weather, management) dataIncludes environmental covariates for enviromicsFacilitates genotype × environment × management (G×E×M) modeling and recommendation
"

	uri <- "doi:10.6084/m9.figshare.30257359"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=NA,
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-09-10",
		carob_completion = 90,	
		carob_effort = 6
	)

	f2 <- ff[basename(ff) == "Covamb.csv"]
	f3 <- ff[basename(ff) == "data.csv"]
	f5 <- ff[basename(ff) == "Malawi_covamb.csv"]
	f6 <- ff[basename(ff) == "Malawi_data.csv"]

	r2 <- read.csv(f2, sep = ";")
	r3 <- read.csv(f3, sep = ";")
	r5 <- read.csv(f5, sep = ";")
	r6 <- read.csv(f6, sep = ";")

	d1 <- data.frame(
	  country=r3$COUNTRY,
	  ID=r3$env,
	  rep=r3$rep,
	  variety_code=r3$gen,
	  flowering_days=r3$FLW_DAYS,
	  flower_color=tolower(r3$FLW_CL),
	  plant_height=r3$PH_R8,
	  seed_weight=r3$W100G,
	  yield=r3$GY,
	  grain_protein=r3$PROT,
	  grain_oil=r3$OIL,#grain oil content
	  planting_date=as.character(as.Date(r3$SOWING, format = "%d/%m/%Y")),
	  harvest_date=as.character(as.Date(r3$HARVEST, format = "%d/%m/%Y")),
	  elevation=r3$ELEV
	)
	
	d1$irrigated <- !(r3$RAINFED=="Rainfed")
	
	loc1 <- unique(data.frame(
	  ID = r2$env,
	  latitude = r2$LAT,
	  longitude = r2$LON
	))
	
	d1 <- merge(d1,loc1,by="ID", all.x=TRUE)
	
	d2 <- data.frame(
	  country=r6$COUNTRY,
	  ID=r6$env,
	  rep=r6$rep,
	  variety_code=r6$gen,
	  flowering_days=r6$FLW_DAYS,
	  flower_color=r6$FLW_CL,
	  plant_height=r6$PH_R8,
	  seed_weight=r6$W100G*10,
	  yield=r6$GY,
	  grain_protein=r6$PROT,
	  grain_oil=r6$OIL,
	  planting_date=as.character(as.Date(r6$SOWING, format = "%d/%m/%Y")),
	  harvest_date=as.character(as.Date(r6$HARVEST, format = "%d/%m/%Y")),
	  elevation=r6$ELEV
	)

	d2$irrigated <- r6$RAINFED=="Irrigation"
	
	loc2 <- unique(data.frame(
	  ID = r5$envi,
	  latitude = r5$LAT,
	  longitude = r5$LON
	))
	
	#matching values format for merge
	d2$ID <- sub("^E0+", "E", d2$ID)
	loc2$ID <- sub("^E0+", "E", loc2$ID)
	
	d2 <- merge(d2,loc2,by="ID", all.x = TRUE)
	
	#filling in for the remaining NAs
	r6$env <- sub("^E0+", "E", r6$env)
	idx <- is.na(d2$longitude)
	d2$longitude[idx] <- r6$LON[match(d2$ID[idx], r6$env)]
	d2$latitude[idx] <- r6$LAT[match(d2$ID[idx], r6$env)]
	
	d <- rbind(d1,d2) 
	
	d$crop <- "soybean"
	d$trial_id <- paste(d$country,d$ID,sep = ";")
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$yield_part <- "seed"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	d$ID <- NULL
	d$country <- gsub("DRC","Democratic Republic of the Congo",d$country)
	d$longitude[d$country == "Senegal"] <- -abs(d$longitude[d$country == "Senegal"])
	d$yield[d$yield < 0] <- NA #eliminating negative yield values since its we cant observe a negative yield in a field
	

	carobiner::write_files(path, meta, d)
}
