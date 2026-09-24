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
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_source;variety",
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
	  cassava_brown_streak_sev=r1$lCBSDSev,
	  cassava_brown_streak_inc=r1$lCBSDinc,
	  cassava_mossaic_sev=r1$CMD_sev,
	  cassava_mossaic_inc=r1$CMDinc,
	  crop="cassava",
	  yield=r1$tFRY*1000,
	  yield_part="roots",
	  yield_isfresh=TRUE,#as stated in the dictionary
	  yield_moisture=NA
	)

	#Reshaping disease from wide to long
	d$row_id <- seq_len(nrow(d))
	
	#grouping and matching the order of columns
	sev_cols <- c("cassava_brown_streak_sev", "cassava_mossaic_sev")
	inc_cols <- c("cassava_brown_streak_inc", "cassava_mossaic_inc")
	
	disease_names <- c("cassava brown streak", "cassava mosaic")
	
	d <- reshape(
	  d,varying = list(disease_severity = sev_cols, disease_incidence = inc_cols),
	  v.names = c("disease_severity", "disease_incidence"),timevar = "disease",
	  times = disease_names,idvar = "row_id",direction = "long")
	
	rownames(d) <- NULL
	d$row_id <- NULL
	
	d$severity_scale <- NA
	d$disease_severity <- as.character(d$disease_severity)
	d$disease_incidence <- as.character(d$disease_severity)
	
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

	carobiner::write_files(path, meta, d)
}

