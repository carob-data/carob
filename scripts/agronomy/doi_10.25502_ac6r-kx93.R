# R script for "carob"


"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."


carob_script <- function(path){

	uri <- "doi:10.25502/ac6r-kx93"
	group <- "agronomy"
	ff <- carobiner::get_data(uri,path,group)

#dataset level data

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication = "doi:10.1016/j.agee.2017.08.015",
		carob_contributor = "Andrew Sila",
		carob_date="2023-07-17",
		data_type = "on-farm experiment",
		data_organization="IITA;WUR",
		modified_by = "Eduardo Garcia Bendito",
		last_modified = "2024-03-07",
		response_vars = "yield",
		treatment_vars = "inoculated;P_fertilizer"	
	)

	p_year <- 2014 #planting year
	h_year <- 2014 # harvest year

	
	# read the experiment data table
	f0 <- ff[basename(ff) == "experiment.csv"]
	d0 <- data.frame(read.csv2(f0, sep = ","))
	
	f1 <- ff[basename(ff) == "general.csv"]
	d1 <- data.frame(read.csv2(f1, sep = ","))
	
	f2 <- ff[basename(ff) == "labour_income_and_assets.csv"]
	d2 <- data.frame(read.csv2(f2, sep = ","))
	
	f3 <- ff[basename(ff) == "land_crops_and_livestock.csv"]
	d3 <- data.frame(read.csv2(f3, sep = ","))
	
	f4 <- ff[basename(ff) == "nutrition.csv"]
	d4 <- data.frame(read.csv2(f4, sep = ","))
	
	f5 <- ff[basename(ff) == "production.csv"]
	d5 <- data.frame(read.csv2(f5, sep = ","))
	
	f6 <- ff[basename(ff) == "variable_definitions.csv"]
	d6 <- data.frame(read.csv2(f6, sep = ","))

	# d <- d0, d1, d3, d4, d5
	
	d0$trial_id <- d0$experiment_id
	
	# Get experiment treatments from d0 table
	trts <- grep("name_treatment_", colnames(d0), perl = TRUE)
	d0_trt <- d0[,trts[1:5]] # Extracts for the five experiment plots
	d0_trt <- cbind(d0$farm_id, d0_trt,d0$trial_id)
	colnames(d0_trt) <- c("farm_id", colnames(d0_trt[-1]))
	treatment <- c(d0_trt[,2],d0_trt[,3],d0_trt[,4],d0_trt[,5],d0_trt[,6])
	farm_id <- rep(d0_trt[,1],5)
	trial_id <- rep(d0_trt[,7],5)
	d0_trt <- cbind(farm_id, treatment, trial_id)
	names(d0_trt) <- c('farm_id', 'treatment', 'trial_id')
	d0_trt <- as.data.frame(d0_trt)
	
	d0_trt$treatment <-  gsub(" fertilizer", "", d0_trt$treatment)
	d0_trt$treatment <-  gsub("fertiliser", "", d0_trt$treatment)
	d0_trt$treatment <-  gsub(" only", "", d0_trt$treatment)
	d0_trt$treatment <-  gsub(" phosphorus and inoculant ", "phosphorus + inoculant ", d0_trt$treatment)
	d0_trt$treatment <-  gsub("phosphorus and inoculant ", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("phosphorus + inoculant ", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("p + inoculant", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("inoculant and phosphorus", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("inoculant and p-", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("inoculant and phosphorus ", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("p + inoculant", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("inoculant + p-", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("phosphorus and inoculant", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("phosphorus + inoculant", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("p + inoculant", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("phosphorus + inoculant ", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("inoculant + p-", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("phosphorus + inoculant  ", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("p + inoculant", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("inoculant + p-", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("phosphorus + inoculant ", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("p + inoculant", "phosphorus + inoculant", d0_trt$treatment)
	d0_trt$treatment <-  gsub("inoculant + p-", "phosphorus + inoculant", d0_trt$treatment)
	
	p <- which(d0_trt$treatment == 'p + inoculant' | d0_trt$treatment == 'inoculant + p-' | d0_trt$treatment=='phosphorus + inoculant ')
	d0_trt$treatment[p] <- "phosphorus + inoculant"
	
	p <- which(d0_trt$treatment == 'p-' | d0_trt$treatment == ' phosphorus' | d0_trt$treatment=='phosphorus '| d0_trt$treatment=='p'| d0_trt$treatment=='p-' |d0_trt$treatment == "p- ")
	d0_trt$treatment[p] <- "phosphorus"
	
	p <- which(d0_trt$treatment == 'inoculant ' | d0_trt$treatment == 'i-' | d0_trt$treatment == ' inoculant')
	d0_trt$treatment[p] <- "inoculant"
	
	p <- which(d0_trt$treatment == 'control ')
	d0_trt$treatment[p] <- "control"
	
	p <- which(d0_trt$treatment == 'farmers practice')
	d0_trt$treatment[p] <- "farmer practice"
	
	unique(d0_trt$treatment)
	
	d0_trt$fertilizer <- rep(NA, nrow(d0_trt))
	d0_trt$P_fertilizer <- rep(0, nrow(d0_trt))
	d0_trt$N_fertilizer <- rep(0, nrow(d0_trt))
	d0_trt$K_fertilizer <- rep(0, nrow(d0_trt))
	phosphorus <- grep("phosphorus",d0_trt$treatment)
	d0_trt$fertilizer[phosphorus] <- 'NPK'
	d0_trt$P_fertilizer[phosphorus] <- '19.23'
	
	# Include a field for whether inoculated or not
	ino <- grep("inoc",d0_trt$treatment)
	d0_trt$inoculated <- rep('no', nrow(d0_trt))
	d0_trt$inoculated[ino] <- 'yes'
	d0_trt$SN <- rep(d0$SN,5)

	# add plot no for the d0_trt
	plot <- NULL
	for (i in 1:5){
	ploti <- rep(i,16)
	plot <- c(plot, ploti)
	}
	d0_trt <- cbind(d0_trt, plot)
	d0_trt$ssid <- paste(d0_trt$farm_id,'.',d0_trt$SN, '.', d0_trt$plot)
	
	
	# reporting table1 : d0_trt
	
	
	# Crop 1
	a <- which(colnames(d0) == 'width_of_harvested_plot_crop_1_plot_1')
	b <- which(colnames(d0) == 'above_ground_biomass_weight_husks_stover_res_crop_1_plot_8.kg')

		dab <-  d0[,a:b]
	
	 dabia <- NULL

		for (i in 0:7){
	  dabi <- dab[,(c(6*i)+1):(c(6*i)+6)]
	  colnames(dabi) <-  letters[1:6]
	  dabia <- rbind(dabia,dabi)
	}
	
	hd <- c('width_of_harvested_plot_crop_1_plot_2.m',                      
	'depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_2.m',
	'number_of_rows_in_plot_crop_1_plot_2',
	'grain_weight_kg_shelled_grain_crop_1_plot_2.kg',
	'pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_2.kg',
	'above_ground_biomass_weight_husks_stover_res_crop_1_plot_2.kg'
	)
	
	plots <- NULL
	for(i in 1:8){
	  plot <- rep(i,16)
	  plots <- c(plots, plot)
	}
	
	# SN <- NULL
	# for(i in 1:16){
	#   SNi <- rep(i,8)
	#   SN <- c(SN, SNi)
	# }
	SN <- rep(1:16,8)
  dabia_crop1 <- cbind(d0$farm_id,plots, SN, dabia)
  
  hd <- gsub("_plot_2", '', hd)
  hd <- gsub("_crop_1", '', hd)
  
  colnames(dabia_crop1) <- c('farm_id', 'plot', 'SN', hd)
  
  dabia_crop1$ssid <- paste(dabia_crop1$farm_id,'.',dabia_crop1$SN, '.', dabia_crop1$plot )

  
  dabia_crop1$plot_size <- as.numeric(dabia_crop1$width_of_harvested_plot.m) * as.numeric(dabia_crop1$depth_of_harvested_plot_perpen_dicular_to_rows.m)
  
  # get the grain and biomass yield/ha
  dabia_crop1$yield <- round(10000 * as.numeric(dabia_crop1$grain_weight_kg_shelled_grain.kg) / dabia_crop1$plot_size,3)
  dabia_crop1$dmy_total <- round(10000 * as.numeric(dabia_crop1$above_ground_biomass_weight_husks_stover_res.kg) / dabia_crop1$plot_size,3)
  
  dabia_crop1 <- dabia_crop1[,c('ssid','farm_id','plot','yield', 'dmy_total')]
  
  #dabia_crop1$SN <- rep(d0$SN,8)
  
  
  #2  reporting table 2: dabia_crop1
  
  # Repeat for crop_2
  a <- which(colnames(d0) == 'width_of_harvested_plot_crop_2_plot_1.m')
  b <- which(colnames(d0) == 'above_ground_biomass_weight_husks_stover_res_crop_2_plot_8.kg')
  dab <-  d0[,a:b]
  dabia <- NULL
  
  for (i in 0:7){
    dabi <- dab[,(c(6*i)+1):(c(6*i)+6)]
    colnames(dabi) <-  letters[1:6]
    dabia <- rbind(dabia,dabi)
  }
  
  hd <- c('width_of_harvested_plot_crop_1_plot_2.m',                      
          'depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_2.m',
          'number_of_rows_in_plot_crop_1_plot_2',
          'grain_weight_kg_shelled_grain_crop_1_plot_2.kg',
          'pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_2.kg',
          'above_ground_biomass_weight_husks_stover_res_crop_1_plot_2.kg'
  )
  
  plots <- NULL
  for(i in 1:8){
    plot <- rep(i,16)
    plots <- c(plots, plot)
  }
  dabia_crop2 <- cbind(d0$farm_id,plots, dabia)
  
  hd <- gsub("plot_2_", '', hd)
  hd <- gsub("crop_1_", '', hd)
  
  colnames(dabia_crop2) <- c('farm_id', 'plot', hd)
  
  #No data to report for dabia_crop2
  
  # Get shelled_grain_crop_weight
  z <- grep('shelled_grain_crop',colnames(d0))
  t(d0[1:6,z])
  
  # Get shelled_grain_crop_weight
  z <- grep('crop',colnames(d0))
  t(d0[1:6,z])
  
  # Get stover weight
  z <- grep('stover_res_crop',colnames(d0))
  t(d0[1:6,z])
  
  # Get experimental_treatment
  z <- grep('experimental_treatments_',colnames(d0))
  t(d0[1:6,z])
  
  a <- which(colnames(d0) == 'grain_weight_kg_shelled_grain_crop_1_plot_1')
  b <- which(colnames(d0) == 'above_ground_biomass_weight_husks_stover_res_crop_1_plot_8.kg')
  

	d1$country <- d1$country
	d1$adm2 <- d1$district
	d1$adm3 <- d1$sector_ward
	d1$adm3 <- carobiner::fix_name(d1$adm3)
	d1$location <- d1$village
	d1$latitude <- d1$gps_latitude_field
	d1$longitude <- d1$gps_longitude_field

	d1 <- d1[,c("SN","farm_id", "country", "adm2", "location", "latitude", "longitude")]
	
	# Go to table 3 from land_crops_and_livestock.csv file
	
	d3a <- d3[,c("SN",'farm_id', 'main_crop_1', 'yield_main_crop_1', 'yield_main_crop_1_unit', 'area_main_crop_1', 'area_main_crop_1_unit.ha')]
	d3b <- d3[,c("SN",'farm_id', 'main_crop_2', 'yield_main_crop_2', 'yield_main_crop_2_unit.kg_per_ha', 'area_main_crop_2', 'area_main_crop_2_unit.ha')]
	d3c <- d3[,c("SN",'farm_id', 'main_crop_3', 'yield_main_crop_3', 'yield_main_crop_3_unit.kg_per_ha', 'area_main_crop_3', 'area_main_crop_3_unit.ha')]
	
	# Rename main crop units
	d3a$yield_main_crop_1_unit[7] = 'kg'
	d3b$yield_main_crop_2_unit[7] = 'kg'
	d3c$yield_main_crop_3_unit[7] = 'kg'
	
	# Rename column names for d3a-c and rbind
	colnames(d3a) <-  gsub("_1","", colnames(d3a))
	colnames(d3b) <-  colnames(d3a)
	colnames(d3c) <-  colnames(d3a)
	
	d3 <- rbind(d3a, d3b[,1:7], d3c[,1:7])
	
	d3$main_crop <- tolower(carobiner::fix_name(d3$main_crop))
	
	# adding the fertilizer information
	d0$N_fertilizer <- 0
	d0$P_fertilizer <- 0
	d0$K_fertilizer <- 0
	# get fields 1 to 5 data from d5
	
	planting_date <- paste0(d5$date_of_planting_dd,'/', d5$date_of_planting_mm)
	
	harvest_date <- paste0(d5$date_of_final_harvest_dd,'/', d5$date_of_final_harvest_mm)
	
	planting_date <- as.Date(strptime(ifelse(planting_date == 'NA', NA, paste0(planting_date, '/', p_year)), "%d/%B/%Y"))
	
	
	harvest_date <- as.Date(strptime(ifelse(harvest_date == 'NA', NA, paste0(harvest_date, '/', h_year)), "%d/%B/%Y"))
	
	
	farm_id <- d5$farm_id
	
	SN <- d5$SN
	
	# 4 reporting table:
	
	p_date <- cbind(as.vector(SN),as.vector(farm_id), as.character(planting_date), as.character(harvest_date))
	colnames(p_date) <- c('SN', 'farm_id', 'planting_date', 'harvest_date')
	
	# Finally reporting with planting and harvesting dates
	
	d <- merge(dabia_crop1, d0_trt[,-c(1,10)], by = "ssid")
	
	# Merge with latlon
	
	d <- merge(d,d1[,-2], by = 'SN', all.x = TRUE)
	

	# Add planting and harvest dates
	d <- merge(d,p_date[,-2], by = 'SN', all.x = TRUE)
	
	# we can not merge d3 with main crop due so it has been left out.
	
	
	# Finally select the fields to be included
	#d <- d[, c("trial_id","on_farm","treatment","crop", "planting_date","harvest_date","N_fertilizer","P_fertilizer","K_fertilizer","yield","seed_weight","fwy_residue","dmy_total")]
	
	d$treatment[c(65,75)] <- NA
	d$P_fertilizer <- as.numeric(d$P_fertilizer)
	d$inoculated <- as.logical(d$inoculated)
	d$latitude <- as.numeric(d$latitude)
	d$longitude <- as.numeric(d$longitude)
	d$geo_from_source <- TRUE
	dikpong.lon <- d$longitude[d$location == "Dikpong"]
	dikpong.lat <- d$latitude[d$location == "Dikpong"]
	d$longitude[d$location == "Dikpong"] <- dikpong.lat
	d$latitude[d$location == "Dikpong"] <- dikpong.lon
	d$longitude[d$location %in% c("Kaleo", "Toure", "Sawaba per Savelugu", "Totenyili", "Agumisi", "Gumyogo", "Azumsapeliga", "Tambiigu", "Yong")] <- d$longitude[d$location %in% c("Kaleo", "Toure", "Sawaba per Savelugu", "Totenyili", "Agumisi", "Gumyogo", "Azumsapeliga", "Tambiigu", "Yong")] * -1
	d$geo_from_source[d$location %in% c("Kaleo", "Toure", "Sawaba per Savelugu", "Totenyili", "Agumisi", "Gumyogo", "Azumsapeliga", "Tambiigu", "Yong")] <- TRUE
	d$longitude[d$location == "Ombo"] <- -2.46
	d$longitude[d$location == "Bini"] <- -0.49
	d$latitude[d$location == "Bini"] <- 9.4
	d$longitude[d$location == "Bussie"] <- -2.5
	d$geo_from_source[d$location %in% c("Ombo", "Bini", "Bussie")] <- FALSE
	omit <- which(colnames(d) %in% c('SN', 'ssid', 'farm_id', 'plot', 'fertilizer'))
	d <- d[,-omit]
	
	# remove whitespace in trial_id
	#d$trial_id <- gsub("[[:space:]]", "", d$trial_id)
	d$trial_id <- ifelse (d$trial_id == "",NA, d$trial_id)
	d$crop = "soybean"
	d$yield_part <- "seed"
	
	
	d$on_farm <- TRUE
	d$is_survey <- NA 
	d$irrigated	<- NA
	
	carobiner::write_files(meta, d, path=path)
}



