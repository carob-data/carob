# R script for "carob"


carob_script <- function(path){

"N2Africa was aimed at increasing biological nitrogen fixation and productivity of grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings which was aimed at increasing soil fertility.The trails were conducted in 11 african countries"
  
	uri <- "doi:10.25502/DGQZ-YP49/D"
	group <- "agronomy"
	ff <- carobiner::get_data(uri,path,group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project = "N2Africa",
		publication = NA,
		carob_contributor = "Effie Ochieng'",
		carob_date="2022-08-06",
		data_type = "on-farm experiment",
		data_organization="IITA;ICRAF;WUR",
		response_vars = "yield",
		treatment_vars = "inoculated;P_fertilizer"		
	)
	 
	
	
	# read the data
	f <- ff[basename(ff) == "data.csv"]
	d <- read.csv(f)
	
	f1 <- ff[basename(ff) == "general.csv"]
	d1 <- read.csv(f1)
	
	f2 <- ff[basename(ff) == "soil_properties.csv"]
	d2 <- read.csv(f2) 
	
	d$crop <- ""
	d$crop[ grep("_CB_", d$experiment_id) ] <- "common bean" # climbing
	d$crop[ grep("_BB_", d$experiment_id) ] <- "common bean" # bush
	d$crop[ grep("_SB_", d$experiment_id) ] <- "soybean"
	
	#process data sets separately identifying the variables of interest
	d$trial_id <- d$experiment_id
	d$rep <- d$replication_no
	d$on_farm <- TRUE
	d$planting_date <- as.character(as.Date(paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")))
	d$harvest_date <- as.character(as.Date(paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")))
	d$treatment <- paste(d$main_treatment, d$sub_treatment_inoc, d$sub_treatment_fert, sep = "_")
	
	#adding fertilizer information
	f <- paste(d$main_treatment, d$sub_treatment_inoc, d$sub_treatment_fert, collapse=" ")
	f <- toupper(f)
	d$K_fertilizer <- d$P_fertilizer <- d$N_fertilizer <- 0
	# this could be further simplified as the fertilizer combination is always the same.
	d$K_fertilizer[grepl("KCL", f)] <- 30
	d$P_fertilizer[grepl("TSP", f)] <- 30
	d$N_fertilizer[grepl("UREA", f)] <- 60
	d$N_splits <- 0L
	d$N_splits[d$N_fertilizer > 0] <- 2L
	d$inoculated <- d$sub_treatment_inoc == "Inoculated"
	d$inoculated[d$sub_treatment_inoc == "Sub-treatment (Inocu"] <- TRUE
	 
	d$fertilizer_type <- ""
	d$fertilizer_type[d$N_fertilizer > 0] <- c("TSP; KCl; urea")
	
	d$yield <- d$grain_yield_ha_calc
	
	#changing the character variables to numeric using lapply
	
	i <- c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules", "nodule_dry_weight")
	d[, i] <- lapply(d[, i], as.numeric)
	
	d$dmy_roots <- d$root_dry_weight_roots_no_nodules
	d$dmy_total <- (d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules +
							d$nodule_dry_weight)
	
	#subset the processed variables
	
	d <- d[,c("trial_id","rep","on_farm","planting_date","harvest_date","treatment","dmy_roots","dmy_total", "crop", "K_fertilizer","P_fertilizer","N_fertilizer","N_splits", "fertilizer_type", "inoculated", "yield")]

	d1$trial_id <- d1$experiment_id
	d1$country <- "Kenya"

	as <- carobiner::fix_name(sapply(strsplit(d1$action_site, "-"), \(i) i[1]), "title")
	as[as=="Marcel Olela"] <- NA
	as <- gsub(" Patrick Obwanga", "", as)
	as <- gsub(": Alice Obiero", "", as)
	d1$location <- as
	d1$adm1 <- carobiner::fix_name(d1$mandate_area_name, "title")
	d1$longitude <- d1$gps_longitude_dec
	d1$latitude <- d1$gps_latitude_dec
	d1$elevation <- d1$gps_altitude_dec
	d1$elevation[d1$elevation < -99] <- NA
	d1$geo_from_source <- TRUE
	
	#subset the processed variables
	d1 <- d1 [, c("trial_id", "country", "location", "adm1", "longitude", "latitude", "elevation", "geo_from_source")]
	
	d2$trial_id <- d2$experiment_id
	d2$soil_pH <- d2$ph
	d2$soil_pH[d2$soil_pH == 0] <- NA
	# all zero  d2$soil_K <- d2$k
	d2$soil_sand <- d2$sand
	d2$soil_clay <- d2$clay
	d2$soil_SOC <- d2$tot_carbon
	d2$soil_N <- d2$tot_nitrogen 
	d2$soil_N[d2$soil_N == 0] <- NA
	
	#subset the processed variables
	d2 <- d2[, c("trial_id", "soil_pH", "soil_sand","soil_clay","soil_SOC","soil_N")]
	
	# combining the processed data sets to one
	s <- merge(d, d1, by = "trial_id")
	q <- merge(s, d2, by = "trial_id")
	
	q$yield_part <- "seed"
	
	q$latitude[q$location == "Migori"] <- -1.0
	
	i <- q$location == "Nyabeda"
	q$longitude[i] <- 34.3997
	q$latitude[i] <- 0.12862
	i <- q$location == "Bondo"
	q$latitude[i] <- -0.082
	i <- q$adm1 == "Nyanza" & is.na(q$location)
	q$longitude[i] <- 34.74
	q$latitude[i] <- -0.55

	q$is_survey <- FALSE
	q$irrigated <- NA
	
	
	i <- (q$planting_date == "2012-10-12") & (q$harvest_date == "2012-02-18")
	q$harvest_date[i] <- "2013-02-18"
	i <- (q$planting_date == "2013-08-23") & (q$harvest_date == "2013-01-16")
	q$planting_date[i] <- "2012-08-23"
		
	# all scripts should end like this
	carobiner::write_files(meta, q, path=path)
}  
  
  
