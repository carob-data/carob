# R script for "carob"


## To do:
## check P fertilizer. It is very high for many records (400 kg/ha)


carob_script <- function(path){

"N2Africa was aimed at increasing biological nitrogen fixation and productivity of grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings which was aimed at increasing soil fertility.The trails were conducted in 11 African countries"

	uri <- "doi:10.25502/dn04-c035"
	group <- "agronomy"
	ff <- carobiner::get_data(uri,path,group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project = "N2Africa",
		publication = NA,
		data_type = "on-farm experiment",
		response_vars = "yield",
		treatment_vars = "inoculated; N_fertilizer; P_fertilizer; K_fertilizer; lime; gypsum",
		data_organization="AfricaRice;IITA;ICRAF;WUR",
		carob_contributor = "Effie Ochieng'",
		carob_date="2022-09-09"
	)
	
	#read the data
	f0 <- ff[basename(ff) == "a_general.csv"]
	d0 <- read.csv(f0)
	f1 <- ff[basename(ff)== "b_info_site_2.csv"]
	d1 <- read.csv(f1)
	f2 <- ff[basename(ff) =="c_use_of_package_2.csv" ]
	d2 <- read.csv(f2)
	f3 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	d3 <- read.csv(f3)
	f4 <- ff[basename(ff) == "e_harvest.csv"]
	d4 <- read.csv(f4)
	
	#processing the first dataset
	d0$trial_id <- d0$farm_id
	d0$adm2 <- carobiner::fix_name(d0$district, "title")
	d0$adm3 <- carobiner::fix_name(d0$sector_ward, "title")
	d0$longitude <- d0$gps_latitude
	d0$latitude <- -(d0$gps_longitude)
	d0$geo_from_source <- TRUE
	d0 <- d0[, c("trial_id", "adm2", "adm3", "latitude","longitude", "geo_from_source")]
	# EGB:
	# Completing lat long records...
	# g <- carobiner::geocode(country = "Mozambique",
	#          adm2 = del$adm2,
	#          adm3 = ifelse(is.na(del$adm3), del$adm2, del$adm3),
	#          location = ifelse(is.na(del$adm3), del$adm2, del$adm3))
	g <- data.frame(country = c("Mozambique", "Mozambique"),
	                adm2 = c("Mogovolas", "Gurue"),
	                adm3 = c("Nametil", "Gurue"),
	                location = c("Nametil", "Gurue"),
	                lon = c(39.3379, 36.9875),
	                lat = c(-15.7088, -15.4651),
	                geo_from_source = FALSE)
	i <- is.na(d0$longitude) | is.na(d0$latitude)
	d0[i & d0$adm2 == "Gurue", "longitude"] <- g[g$adm2 == "Gurue" & g$adm3 == "Gurue", "lon"]
	d0[i & d0$adm2 == "Mogovolas" & d0$adm3 == "Nametil", "longitude"] <- g[g$adm2 == "Mogovolas" & g$adm3 == "Nametil", "lon"]
	d0[i & d0$adm2 == "Gurue", "latitude"] <- g[g$adm2 == "Gurue" & g$adm3 == "Gurue", "lat"]
	d0[i & d0$adm2 == "Mogovolas" & d0$adm3 == "Nametil", "latitude"] <- g[g$adm2 == "Mogovolas" & g$adm3 == "Nametil", "lat"]
	d0$geo_from_source[d0$adm2 == "Gurue"] <- FALSE
	d0$geo_from_source[d0$adm2 == "Mogovolas" & d0$adm3 == "Nametil"] <- FALSE
	
	#processing the 2nd dataset
	d1$trial_id <- d1$farm_id
	d1$crop_rotation <- trimws(tolower(d1$main_crop_last_season))
	
	d1$crop_rotation[d1$crop_rotation == "maize, soybean"] <- "maize"
	d1$crop_rotation[d1$crop_rotation %in% c("soya", "soja", "soya bean", "soybean")] <- "soybean"
	d1$crop_rotation[d1$crop_rotation %in% c("ground nut", "ground nuts", "groundnuts")] <- "groundnut"
	d1$crop_rotation[d1$crop_rotation == "beans"] <- "common bean"
	d1$crop_rotation[d1$crop_rotation %in% c("", "no")] <- NA
	
	d1 <- d1[, c("trial_id","crop_rotation")]
	
	d4 <- d4[, c("farm_id","plot_no","crop_1_area_harvested","crop_1_weight_grain")]
	d2 <- merge(d2,d4, by = "farm_id")
	d2$trial_id <- d2$farm_id
	d2$crop <- d2$crop_1
	d2$variety <- d2$variety_1
	
	d2$inoculated <- ifelse(d2$inoculant_used %in% c("Biagro", "Y", "Yes"), TRUE, 
					ifelse(d2$inoculant_used %in% c("N", "no"), FALSE, NA))
	
	#cleaning fertilizer types
	d2$mineral_fert_type <- carobiner::fix_name(d2$mineral_fert_type, "lower")
	
	d2$mineral_fert_type[d2$mineral_fert_type %in% c("urea", "ureia", "ureia")] <- "urea"
	d2$mineral_fert_type[d2$mineral_fert_type %in% "yes"] <- "ssp"
	
	i <- grep("ssp", d2$mineral_fert_type)
	d2$mineral_fert_type[i] <- "SSP"
	
	i <- grep("no", d2$mineral_fert_type)
	d2$mineral_fert_type[i] <- "none"
	
	## how much lime was applied?
	#d2$lime <- 
	## how much gypsum was applied?
	#d2$gypsum <- 

	i <- d2$mineral_fert_type %in% c("lime", "cal")
	d2$mineral_fert_type[i] <- "lime"
	
	d2$fertilizer_type <- d2$mineral_fert_type

	d2$fertilizer_type[is.na(d2$fertilizer_type) & d2$mineral_fert_amount == 0] <- "none"

		
	d2$gypsum <- ifelse(d2$fertilizer_type == "gypsum", d2$mineral_fert_amount, 0)
	d2$gypsum <- 10000/as.numeric(d2$crop_1_area_harvested) * d2$gypsum
	
	d2$lime <- ifelse(d2$fertilizer_type == "lime", d2$mineral_fert_amount, 0)
	d2$lime <- 10000/as.numeric(d2$crop_1_area_harvested) * d2$lime
	
	d2$K_fertilizer <- 0

	d2$P_fertilizer <- ifelse(d2$fertilizer_type== "SSP", d2$mineral_fert_amount, 0) 
	d2$P_fertilizer <- (10000/d2$crop_1_area_harvested) * d2$P_fertilizer
	
	d2$N_fertilizer <- ifelse(d2$fertilizer_type== "urea", d2$mineral_fert_amount, 0) 
	d2$N_fertilizer <- (10000/d2$crop_1_area_harvested) * d2$N_fertilizer
	
	
	d2$crop_1_area_harvested <- as.numeric(d2$crop_1_area_harvested)
	d2$crop_1_weight_grain <- as.numeric(d2$crop_1_weight_grain)
	d2$yield <- (10000/d2$crop_1_area_harvested)*d2$crop_1_weight_grain
	
		
	d2 <- d2[, c("trial_id","crop","variety","inoculated","lime","gypsum","P_fertilizer", "K_fertilizer","N_fertilizer","fertilizer_type", "yield")]
	
	d3$trial_id <- d3$farm_id
	d3$planting_date <- ifelse(d3$date_planting_yyyy == 0, NA, paste(d3$date_planting_yyyy, sprintf("%02d", d3$date_planting_mm), sprintf("%02d", d3$date_planting_dd), sep = "-"))
	d3$harvest_date <- ifelse(d3$date_harvest_yyyy == 0, NA, paste(d3$date_harvest_yyyy, sprintf("%02d", d3$date_harvest_mm), sprintf("%02d", d3$date_harvest_dd), sep = "-"))
	
	d3 <- d3[,c("trial_id","planting_date","harvest_date")]
		
	d <- merge(d0, d1, "trial_id")
	d <- merge(d, d2, "trial_id")
	d <- merge(d, d3, "trial_id")
	
	d$country <- "Mozambique"
	d$crop <- carobiner::fix_name(d$crop, "lower")
	d$crop[grepl("soybean", d$crop)] <- "soybean"
	
	#standardizing the varieties
	v <- d$variety
	v <- carobiner::fix_name(v, "first")
	v <- gsub("TGX", "Tgx", v)
	v[ grepl("Santa", v, ignore.case=TRUE) ] <- "Santa"
	v[ grepl("Safari", v) ] <- "Safari"
	v[ grepl("Serenade", v) ] <- "Serenade"
	v[ grepl("Storm", v, ignore.case=TRUE) ] <- "Storm"
	v[ grepl("1740-2F", v) ] <- "Tgx-1740-2F"
	v[ grepl("1485", v) ] <- "Tgx-1485-1D"
	v <- gsub("Tgx ", "Tgx-", v)
	v <- gsub("Tgx1", "Tgx-1", v)
	
	d$variety <- v
	
	d$yield_part <- ifelse(d$crop == "groundnut", "pod", "seed")
	
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path=path, meta, d)
}
