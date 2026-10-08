# R script for "carob"
# license: GPL (>=3)

## ISSUES



carob_script <- function(path) {

"
Maize (Zea mays L.) yields and water productivity as affected by cowpea (Vigna unguiculata (L.) Walp.) intercropping over five consecutive growing seasons in a semi-arid environment in Kenya.

This dataset contains raw data collected during an experiment on maize-cowpea intercropping over five consecutive seasons in semi-arid regions of Kenya. The data includes measurements of soil moisture, temperature, crop yields and other traits, plant populations, and weather conditions during the study period. This dataset also contains scritps that have been used to process the data and produce the result figures and tables for the research article:&nbsp;  Tuure, J., Mganga, K.Z., M&auml;kel&auml;, P.S.A., R&auml;s&auml;nen, M., Pellikka, P., Wachiye, S., Alakukku, L., 2025. Maize (Zea mays L.) yields and water productivity as affected by cowpea (Vigna unguiculata (L.) Walp.) intercropping over five consecutive growing seasons in a semi-arid environment in Kenya. Agricultural Water Management 319, 109779. https://doi.org/10.1016/j.agwat.2025.109779
"

	uri <- "doi:10.5281/zenodo.16930024"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
	  data_organization = "UH", # UH:University of Helsinki
		publication = "doi:10.1016/j.agwat.2025.109779",
		project = NA,
		carob_date = "2026-09-24",
		design = NA,
		data_type = "experiment",
		treatment_vars = "intercrops",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		notes = NA,
		carob_completion = 70,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "manualObservations.txt"]
	f2 <- ff[basename(ff) == "yieldsCowpea.txt"]
	f3 <- ff[basename(ff) == "yieldsMaize.txt"]
	f4 <- ff[basename(ff) == "weatherData.txt"]
	#f5 <- ff[basename(ff) == "weatherDataLongTerm.txt"]
	
	
	
####
	r1 <- read.table(f1, sep = ",", header = TRUE)
	r2 <- read.table(f2, sep = ",", header = TRUE)
	r3 <- read.table(f3, sep = ",", header = TRUE)
	r4 <- read.table(f4, sep = ",", header = TRUE)
	#r5 <- read.table(f5, sep = ",", header = TRUE)
	
#####	process
	d1 <- data.frame(
	  date = as.character(as.Date(r1$Date, "%d.%m.%Y")),
	  year = substr(as.character(as.Date(r1$Date, "%d.%m.%Y")), 1, 4),
	  plot_id = r1$PlotCode,
	  crop = tolower(r1$Crop),
	  plant_height = r1$PlantHeight,
	  SPAD = r1$SPAD,
	  treatment = r1$CropTreatment,
	  intercropped = grepl("Intercrop", r1$CropTreatment)
	)
	
	d1 <- aggregate(d1[c("plant_height", "SPAD")], d1[c("plot_id", "date", "year", "crop", "treatment", "intercropped")], mean, na.rm = TRUE)
	
	
	#### cowpea crop
	d2 <- data.frame(
	  year = substr(as.character(as.Date(r2$Date, "%d.%m.%Y")), 1, 4),
	  harvest_date = ifelse(!is.na(r2$finalYield) & r2$finalYield > 0, as.character(as.Date(r2$Date, "%d.%m.%Y")), NA),
	  plot_id = r2$PlotCode,
	  seed_weight = rowMeans(r2[, c("hundredgrainsWeight1","hundredgrainsWeight2", "hundredgrainsWeight3")])*10,
	  yield = r2$finalYield,
	  dmy_storage = r2$dryGrainYield,
	  treatment = r2$cropTreatment,
	  intercropped = grepl("Maize", r2$cropTreatment),
	  crop = "cowpea",
	  intercrops = ifelse(grepl("Maize", r2$cropTreatment), "maize", "none"),
	  plant_density = (r2$plantPopulation/25)*10000,
	  plot_area = 25, ## from publication
	  location = "Maktau",
	  country = "Kenya",
	  longitude = 38.19444,
	  latitude = - 3.508333
	)
	
	### maize crop
	
	d3 <- data.frame(
	  year = substr(as.character(as.Date(r3$Date, "%d.%m.%Y")), 1, 4),
	  harvest_date = ifelse(!is.na(r3$finalYield) & r3$finalYield > 0, as.character(as.Date(r3$Date, "%d.%m.%Y")), NA),
	  plot_id = r3$PlotCode,
	  seed_weight = rowMeans(r3[, c("hundredKernelsWeight1","hundredKernelsWeight2", "hundredKernelsWeight3")])*10,
	  yield = r3$finalYield,
	  dmy_storage = r3$dryGrainYield,
	  treatment = r3$cropTreatment,
	  intercropped = grepl("Cowpea", r3$cropTreatment),
	  crop = "maize",
	  intercrops = ifelse(grepl("Cowpea", r3$cropTreatment), "cowpea", "none"),
	  plant_density = (r3$plantPopulation/25)*10000,
	  plot_area = 25, ## from publication
	  location = "Maktau", # from publication
	  country = "Kenya",
	  longitude = 38.19444,
	  latitude = - 3.508333
	)
	
	d <- rbind(d2, d3) 
	d$record_id <- as.integer(1:nrow(d))
	d$trial_id <- ifelse(grepl("maize", d$crop), "1", "2")
	## from publication
	d$harvest_days <- c("2019"= 123 , "2020" = 135, "2021"= 111 )[d$year]
	d$planting_date <- as.character(as.Date(d$harvest_date)-d$harvest_days)
	d$planting_date[is.na(d$planting_date)] <- d$year[is.na(d$planting_date)]
	
	### Adding record_id in d1
	rec <- d[, c("plot_id", "intercropped", "crop", "year", "record_id")]
	rec <- rec[!duplicated(rec[, c("plot_id", "intercropped", "crop", "year")]),]
	d1 <- merge(d1, rec, by = c("plot_id", "intercropped", "crop", "year"), all.x = TRUE)
	d1 <- d1[, c("SPAD","date", "plant_height", "record_id")]
	
	d$year <- NULL
	
	#### weather data 
	dw <- data.frame(
	  date = as.character(as.Date(carobiner::eng_months_to_nr(substr(r4$Time, 1, 11)), "%d-%m-%Y")),
	  time = substr(r4$Time, 13, 20),
	  rhmn = r4$RH,
	  prec = r4$Rain,
	  wspd = r4$WS,
	  wdir = r4$WD,
	  srad = r4$Srad,
	  location = "Maktau",
	  country = "Kenya",
	  longitude = 38.19444,
	  latitude = - 3.508333
	)
	
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_isfresh <-  TRUE  
	d$yield_moisture <- NA
	d$yield_part <- "grain"
	d$geo_from_source <- TRUE
	d$irrigated <- NA
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	
	carobiner::write_files(path, meta, d, long = d1,wth = dw)
}

