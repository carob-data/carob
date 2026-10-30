# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. out of bounds: plot_area (0, 12000)

carob_script <- function(path) {

"
Cassava Yield Study, Certified seed vs recycled seed performance 2021

Evaluation of yield benefits of certified cassava seed.
"

	uri <- "doi:10.25502/n5t4-f420/f"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-09-23",
		carob_completion = 90,	
		carob_effort = 4
	)

	f1 <- ff[basename(ff) == "best-cassava-yield-study-data-2021.csv"]
	f2 <- ff[basename(ff) == "data_dictionary.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	
	d <- data.frame(
	  country="Tanzania",
	  adm1=r1$Region,
	  adm2=trimws(r1$District),
	  variety=trimws(tolower(r1$Cultivar)),
	  seed_source=trimws(tolower(r1$Seed_Source)),
	  seed_recycle=r1$Num_recycles,#number of cycles the seed was reused
	  elevation=r1$Altitude,
	  plot_area=r1$Plot_Size,
	  harvest_index=r1$HI,
	  crop="cassava",
	  yield=r1$tFRY*1000,
	  yield_part="roots",
	  yield_isfresh=TRUE,#as stated in the dictionary
	  yield_moisture=NA
	)

	#Reshaping disease from wide to long
	d$record_id <- seq_len(nrow(d))
	
	##disease data in long format, kept separate from d
	d_lon <- data.frame(
	  record_id = rep(d$record_id, 2),
	  disease = rep(c("cassava brown streak", "cassava mosaic"), each = nrow(r1)),
	  disease_severity = as.character(c(r1$lCBSDSev, r1$CMD_sev)),
	  disease_incidence = as.character(c(r1$lCBSDinc, r1$CMDinc)),
	  severity_scale = NA
	)
	
	d$trial_id <- paste(d$adm2,seq_len(nrow(d)), sep = "_")
	
	d$on_farm <- NA#not specified in the description
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$geo_from_source <- FALSE #coordinates obtained from the metadata of the experiment webpage

	d$planting_date <- NA
	d$harvest_date  <- NA
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
  
  geo <- data.frame(
    adm2 = c("Biharamulo", "Bunda", "Butiama", "Sengerema", "Muheza", "Kibiti", "Bagamoyo", "Nyang'wale", "Geita"),
    longitude = c(31.2575, 33.8985, 33.9715, 32.5182, 38.7654, 38.6716, 38.4114, 32.6056, 32.1749),
    latitude = c(-2.7419, -2.0401, -1.6857, -2.5996, -5.1835, -7.9428, -6.3142, -3.1520, -2.8700),
    geo_uncertainty = c(69599, 77667, 44924, 73349, 36258, 97386, 83503, 35506, 79959),
    geo_source = c("GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2")
  )

  d <- merge(d,geo,by="adm2", all.x = TRUE)

	carobiner::write_files(path, meta, d, long = d_lon)
}

