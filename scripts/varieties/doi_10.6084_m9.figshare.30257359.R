# R script for "carob"
# license: GPL (>=3)

# dictionary in doi_10.6084_m9.figshare.30257359\Pan_African_Trials_Network-main\metadata\docs\Supplementary.pdf

## ISSUES
# (coordinates in wrong country: Zambia/Malawi cannot be fixed as there is no location description

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

	r3 <- read.csv(f3, sep = ";")
	# r6 <- read.csv(f6, sep = ";")

	d <- data.frame(
	  country=r3$COUNTRY,
	  trial_id=paste0(r3$COUNTRY, "_", r3$loc),
	  rep=r3$rep,
	  variety_code=r3$gen,
	  flowering_days=r3$FLW_DAYS,
	  flower_color=tolower(r3$FLW_CL),
	  plant_height=r3$PH_R8,
	  seed_weight=r3$W100G,
	  yield=r3$GY,
	  protein=r3$PROT,
	  oil_content=r3$OIL,#grain oil content
	  planting_date=as.character(as.Date(r3$SOWING, format = "%d/%m/%Y")),
	  harvest_date=as.character(as.Date(r3$HARVEST, format = "%d/%m/%Y")),
	  latitude = r3$LAT,
	  longitude = r3$LON,
	  elevation=r3$ELEV,
	  irrigated = r3$RAINFED != "Rainfed"
	)

    # Malawi, seems subset of d1 (but records are not a perfect match)
	# d2 <- data.frame(
	#  country=r6$COUNTRY,
	#  trial_id=paste0(r6$COUNTRY, "_", r6$loc),
	#  rep=r6$rep,
	#  variety_code=r6$gen,
	#  flowering_days=r6$FLW_DAYS,
	#  flower_color=tolower(r6$FLW_CL),
	#  plant_height=r6$PH_R8,
	#  seed_weight=r6$W100G*10,
 	#  yield=r6$GY,
	#  protein=r6$PROT,
	#  oil_content=r6$OIL,
	#  planting_date=as.character(as.Date(r6$SOWING, format = "%d/%m/%Y")),
	#  harvest_date=as.character(as.Date(r6$HARVEST, format = "%d/%m/%Y")),
	#  latitude = r6$LAT,
	#  longitude = r6$LON,
	#  elevation=r6$ELEV,
	#  irrigated = r6$RAINFED != "Rainfed"
	#)
	
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
	d$country <- gsub("DRC","Democratic Republic of the Congo", d$country)
	i <- which(d$country %in% c("Senegal", "Mali"))
	d$longitude[i] <- -abs(d$longitude[i])
	# eliminating negative yield values
	d$yield[d$yield < 0] <- NA 
	d$flowering_days[d$flowering_days < 7] <- NA
	d$seed_weight[d$seed_weight < 1] <- NA

	# fixing bad harvest dates by comparing them to other records
	d$harvest_date[d$country == "Uganda" & d$harvest_date == "2021-12-16"] <- "2020-12-16"
    d$harvest_date[d$country == "Zambia" & d$harvest_date == "2020-05-12"] <- "2021-05-12"
    d$harvest_date[d$country == "Mali" & d$harvest_date == "2023-07-22"] <- "2023-11-22"
    d$harvest_date[d$country == "Mali" & d$harvest_date == "2023-08-19"] <- "2023-11-19"
    d$harvest_date[d$country == "Malawi" & d$harvest_date == "2024-03-31"] <- "2024-07-31"
    d$harvest_date[d$country == "Mozambique" & d$harvest_date == "2022-05-26"] <- "2023-05-26"
    d$harvest_date[d$country == "Mozambique" & d$harvest_date == "2022-04-26"] <- "2023-04-26"
    i <- d$country == "Mozambique"
	d$harvest_date[i] <- gsub("2022-11-|2022-12-", "2023-05-", d$harvest_date[i])
    #i <- i & d$planting_date == "2022-12-22"
	#d$harvest_date[i] <- gsub("2022-", "2023-", d$harvest_date[i])
 	
	carobiner::write_files(path, meta, d)
}
