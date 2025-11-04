# R script for "carob"

carob_script <- function(path) {

"Integrating more grain legumes as intercrops or rotational system can allow farmers to achieve high and stable yield under varying rainfall, with modest fertilizer investments. This is critical for resource poor farmers who have limited access to mineral fertilizers. In these experiments that were initiated in 2012, we investigate soil organic carbon (SOC) changes over time for treatments that range from an unfertilized control, maize fertilized with NP optimally every year and when legumes are integrated as intercrops or rotations with maize. Recently we have applied stability analysis to assess impacts of grain legume integration on maize grain yield, yield stability, nitrogen use efficiency (NUE) and ability to meet household protein requirements. More details on these experiments are outlined in the publications Smith et al. 2016 , Snapp et al. 2018 , and Chimonyo et al. 2019"

	uri <- "doi:10.7910/DVN/BUPNF4"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "MSU",
		publication = "doi:10.2135/cropsci2018.09.0532",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "intercrops;crop_rotation;fertilizer_used",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-11-29",
		notes = NA,
		design = "RCBD"
	)
	

	f1 <- ff[basename(ff) == "maize_SItrials_2018_19_harvest.csv"]
	f2 <- ff[basename(ff) == "soybean_SItrials_2018_19_harvest.csv"]
	f3 <- ff[basename(ff) == "groundnut_SItrials_2018_19_harvest.csv"]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

	d1 <- data.frame(
		location = r1$EPA,
		rep = r1$Rep,
		treatment = r1$Treatment,
		crop = r1$Crop,
		plot_area = r1$Net.plot..m2.,
		row_spacing = r1$Ridge.spacing..m. * 100,
		plant_density = r1$No.of.plants.plot * 2222,
		cob_density = r1$number.of.cobs.plot * 2222,
		yield = r1$Grain.weight..Kg.Ha.,
		dmy_residue = r1$Stover.dry.weight..Kg.Ha.,
		fwy_total = r1$Total.biomass.Kg.Ha.,
		treatment_code = r1$Treatment.number
	)
	
	d2 <- data.frame(
	  location = r2$EPA,
	  rep = r2$Rep,
	  treatment = r2$Treatment,
	  crop = r2$Crop,
	  plot_area = r2$Net.plot.area.m2.,
	  row_spacing = r2$Ridge.spacing..m. * 100,
	  plant_density = r2$No.of.plants.plot * 3333,
	  yield = r2$Grain.weight..Kg.Ha.,
	  dmy_residue = r2$Stover.dry.weight..Kg.Ha.,
	  fwy_total = NA,
	  cob_density = NA,
	  treatment_code = r2$Treatment.number
	  )
	
	d3 <- data.frame(
	  location = r3$EPA,
	  rep = r3$Rep,
	  treatment = r3$Treatment,
	  crop = r3$Crop,
	  plot_area = r3$Net.plot.area.m2.,
	  row_spacing = r3$Ridge.spacing..m. * 100,
	  plant_density = r3$No.of.plants.plot * 3333,
	  yield = r3$Grain.weight..Kg.Ha.,
	  dmy_residue = r3$Stover.dry.weight..Kg.Ha.,
	  fwy_total = r3$Total.biomass.Kg.Ha.,
	  cob_density = NA,
	  treatment_code = r3$Treatment.number
	  
	)
	# remove empty rows
	d3 <- d3[!is.na(d3$yield), ]
	
 d <- rbind(d1, d2, d3) 
 
 d$yield[d$yield=="#DIV/0!"] <- NA
 d$yield <- as.numeric(d$yield)
 d$fwy_total[d$fwy_total=="#DIV/0!"] <- NA
 d$fwy_total <- as.numeric(d$fwy_total)
 d$crop <- gsub("Groundnut", "groundnut", d$crop)
 d$crop <- gsub("Soya","soybean",d$crop)
 
 d$intercrops <- "none"
 d$intercrops[d$treatment_code==8 & d$crop=="groundnut"] <- "pigeon pea"
 d$intercrops[d$treatment_code==10 & d$crop=="soybean"] <- "pigeon pea"
 d$intercrops[d$treatment_code==7 & d$crop=="maize"] <- "pigeon pea"
 
 d$crop_rotation <- "none"
 d$crop_rotation[d$treatment_code==4 & d$crop=="groundnut"] <- "groundnut;maize"
 d$crop_rotation[d$treatment_code==4 & d$crop=="maize"] <- "groundnut;maize"
 d$crop_rotation[d$treatment_code==5 & d$crop=="soybean"] <- "maize;soybean"
 d$crop_rotation[d$treatment_code==5 & d$crop=="maize"] <- "maize;soybean"
 d$crop_rotation[d$treatment_code==6 & d$crop=="maize"] <- "cowpea;maize"
 d$crop_rotation[d$treatment_code==8 & d$crop=="groundnut"] <- "groundnut;maize"
 d$crop_rotation[d$treatment_code==8 & d$crop=="maize"] <- "maize;pigeon pea_groundnut"
 d$crop_rotation[d$treatment_code==9 & d$crop=="maize"] <- "maize;pigeon pea_cowpea"
 d$crop_rotation[d$treatment_code==10 & d$crop=="soybean"] <- "maize;soybean"
 d$crop_rotation[d$treatment_code==10 & d$crop=="maize"] <- "maize;pigeon pea_soybean"
 
 d$country <- "Malawi"
 d$location <- trimws(d$location)
 d$longitude[d$location=="Golomoti"] <- 34.6003
 d$latitude[d$location=="Golomoti"] <- -14.43
 d$longitude[d$location=="Nsipe"] <- 34.9
 d$latitude[d$location=="Nsipe"] <- -14.8667
 d$longitude[d$location=="Linthipe"] <- 34.1261
 d$latitude[d$location=="Linthipe"] <- -14.1778
 d$longitude[d$location=="Kandeu"] <- 34.6172
 d$latitude[d$location=="Kandeu"] <- -14.6039
 d$longitude[d$location=="Nsanama"] <- 35.5167
 d$latitude[d$location=="Nsanama"] <- -14.9667
 d$longitude[d$location=="Nyambi"] <- 35.6275
 d$latitude[d$location=="Nyambi"] <- -14.6594
 d$longitude[d$location=="Ntiya"] <- 35.3
 d$latitude[d$location=="Ntiya"] <- -15.3833
 
 d$elevation[d$location=="Ntiya"] <- 1082
 d$elevation[d$location=="Nyambi"] <- 1177
 d$elevation[d$location=="Nsanama"] <- 690
 d$elevation[d$location=="Nsipe"] <- 893
 d$elevation[d$location=="Linthipe"] <- 1220
 d$elevation[d$location=="Kandeu"] <- 878
 d$elevation[d$location=="Golomoti"] <- 1082
 
 d$trial_id <- d$location

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE

	#planting dates to be provided by author
	#d$planting_date <- as.character(as.Date(   ))
	#d$harvest_date  <- as.character(as.Date(    ))

 # Fertilizer data sourced from protocol 
   d$fertilizer_used <- d$treatment_code !=1
   d$P_fertilizer <- ifelse(d$fertilizer_used, 21/2.29, 0) 
   d$P_fertilizer[d$crop=="maize"] <- 10.5/2.29
   d$K_fertilizer <- 0
   d$N_fertilizer <- ifelse(d$fertilizer_used, 69, 0)
   d$N_fertilizer[d$crop=="maize"] <- 34.5
   d$fertilizer_type <- ifelse(d$fertilizer_used, "urea;unknown", "none")
   d$yield_part <- ifelse(d$crop=="maize","grain","seed")
   
  
   d$inoculated <- d$crop=="soybean"
   d$inoculant <- ifelse(d$inoculated, "Rhizobium japonicum", "none")
   
   d$treatment_code <- NULL
   
   d$plant_density[d$plant_density < 1000] <- NA
   d$cob_density[d$cob_density < 1000] <- NA
  d$rep <- as.integer(d$rep)
  d$planting_date <- as.character(NA)

  d <- d[!is.na(d$yield), ]
  
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}



