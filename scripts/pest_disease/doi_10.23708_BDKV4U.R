# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them
## missing harvest_date
##confusing planting dates

carob_script <- function(path) {

"
Corn pest and climate monitoring dataset from Taita Hill, Maktau, Kenya, 2022-2024

This dataset corresponds to climate data and maize pests monitoring of Busseola fusca, Chilo partellus, and Spodoptera frugiperda between 2022 and 2024 at Taita Hills, Maktau, Kenya (3°25'33'S, 38°8'23'E, altitude 1090 masl). Trapping data include site name, survey date, species, number of individuals, pheromone change dates, and any notes. Climatic data include date, temperature, relative humidity and dew point. All date and time data are in UTC. Measurements were taken using a Hobo MX2301A sensor (ONSET(r)).
"


	uri <- "doi:10.23708/BDKV4U"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "insecticide_used",
		response_vars = "pest_species;pest_number", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-08-04",
		carob_completion = 80,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "bdd_kenya_maktau_bf.csv"]
	f2 <- ff[basename(ff) == "bdd_kenya_maktau_climate.csv"]
	f3 <- ff[basename(ff) == "bdd_kenya_maktau_cp.csv"]
	f4 <- ff[basename(ff) == "bdd_kenya_maktau_sf.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)


	
	d1 <- data.frame(
	  location = as.character(r1$site),
	  date = as.character(r1$date),
	  treatment = as.character(r1$pheromoneChange),
	  insecticide_dates = "2021-11-07",### date for pheromone installation
	  pest_species = as.character(r1$species),
	  pest_number = as.integer(r1$numberOfIndividuals),
	  insecticide_used = as.logical(r1$pheromoneChange),#### in this case the insecticide is the pheromone
	  yield = NA, 
	  crop = "maize"
	  )
	
	d2 <- data.frame(
	  temp = as.numeric(r2$temperature),
	  rhum = as.numeric(r2$rh),
	  date = as.character(r2$dateUTC),
	  dewp = as.numeric(r2$dewpoint)
	)
	
	
	d3 <- data.frame(
	  location = as.character(r3$site),
	  date = as.character(r3$date),
	  pest_species = as.character(r3$species),
	  pest_number = as.integer(r3$numberOfIndividuals),
	  insecticide_used = as.logical(r3$pheromoneChange)
	)
	
	d3$pest_species[d3$pest_species == "Chilo partellus" & d3$date == "2022-10-22"] <- "Busseola fusca" #### this was a mistake it has been indicated in the notes
	

	d4 <- data.frame(
	  location = as.character(r4$site),
	  date = as.character(r4$date),
	  pest_species = as.character(r4$species),
	  pest_number = as.integer(r4$numberOfIndividuals),
	  insecticide_used = as.logical(r4$pheromoneChange)
	)
	
	
	d5 <- rbind(d3,d4)
	d <- merge(d1, d2, by = "date", all = TRUE)
	d <- merge(d, d5, by = c("date", "location","pest_number","insecticide_used","pest_species"), all.x = TRUE)
	d <- merge(d, d5, by = c("date", "location","pest_number","insecticide_used","pest_species"), all.y = TRUE)
	
	
	#The dataset has three planting dates and 2 harvesting dates indicated in the r1$notes and r3$notes
	### We have two planting dates in the same year/season it might be replanted or it was gap filling but not clearly highlighted
	d$planting_date <- NA
	d$harvest_date <- NA
	#d$planting_date[1:4] <- as.Date(c("17-11-2022","29-03-2023","05-10-2023","17-03-2024"), format = "%Y-%m-%d")
	#d$harvest_date[1:4] <- as.Date(c("26-02-2023","14-07-2024"), format = "%Y-%m-%d")
	d$planting_date[1:4] <- format(as.Date(c("17-11-2022","29-03-2023","05-10-2023","17-03-2024"), format="%d-%m-%Y"), "%Y-%m-%d")
	d$harvest_date[1:2] <- format(as.Date(c("26-02-2023","14-07-2024"), format="%d-%m-%Y"),"%Y-%m-%d")
	
	d$country <- "Kenya"
	d$trial_id <- "1"

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	#### the dataset provided geo_cordinates for Taita Hills, Maktau, Kenya (3°25’33”S, 38°8’23”E, altitude 1090 masl). 
	#### geo_unceratinity was for Mwatate = adm3 for Kenya
	##### Mkatau is adm4 and not available on GADM
	d$longitude <- 38.140
	d$latitude = -3.426
	d$elevation <- 1090
  d$geo_uncertainty = 15792
	d$geo_source = "GADM 4.1, adm3"
	d$geo_from_source <- TRUE


	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)  
	d$fertilizer_type <- NA
	
	d$yield_part <- "none"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	
	
	carobiner::write_files(path, meta, d)
}


