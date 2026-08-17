# R script for "carob"
# license: GPL (>=3)

## ISSUES

## the presence of maize needs to be captured 

## new added variables pheromone_change and pheromone_date
## pheromone_change should not be required (pheromone_date suffices). 
## are the differences between the three species real? Or is that an error?
## needs an article to verify. 

carob_script <- function(path) {

"
Corn pest and climate monitoring dataset from Taita Hill, Maktau, Kenya, 2022-2024

This dataset corresponds to climate data and maize pests monitoring of Busseola fusca, Chilo partellus, and Spodoptera frugiperda between 2022 and 2024 at Taita Hills, Maktau, Kenya (3°25'33'S, 38°8'23'E, altitude 1090 masl). Trapping data include site name, survey date, species, number of individuals, pheromone change dates, and any notes. Climatic data include date, temperature, relative humidity and dew point. All date and time data are in UTC. Measurements were taken using a Hobo MX2301A sensor (ONSET(r)).
"

	uri <- "doi:10.23708/BDKV4U"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "pheromone_change",
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
	  date = r1$date,
	  #treatment = as.character(r1$pheromoneChange),
	  pheromone_change = r1$pheromoneChange,
	  pheromone_date = "2021-11-07",### date for pheromone installation
	  pest_species = r1$species,
	  pest_number = as.integer(r1$numberOfIndividuals)
	)
	
	
	wth <- data.frame(
	  location = "Mkatau",
	  longitude = 38.140,
	  latitude = -3.426,
	  elevation = 1090,
	  geo_from_source = TRUE,
	  temp = r2$temperature,
	  rhum = r2$rh,
	  date = substr(r2$dateUTC, 1, 10),
	  time = substr(r2$dateUTC, 12, 19),
	  timezone = "UTC",
	  dewp = r2$dewpoint
	)
	
	
	d3 <- data.frame(
	  date = r3$date,
	  pest_species = r3$species,
	  pest_number = r3$numberOfIndividuals,
	  pheromone_change = r3$pheromoneChange
	)
	
	#### this was a mistake it has been indicated in the notes
	d3$pest_species[d3$pest_species == "Chilo partellus" & d3$date == "2022-10-22"] <- "Busseola fusca" 
	
	d4 <- data.frame(
	  date = r4$date,
	  pest_species = r4$species,
	  pest_number = r4$numberOfIndividuals,
	  pheromone_change = r4$pheromoneChange
	)
	
	long <- carobiner::bindr(d1, d3, d4)
	long$field_id= "1"
	

	d <- data.frame(
		country = "Kenya",
		location = "Mkatau",
		longitude = 38.140,
		latitude = -3.426,
		elevation = 1090,
		geo_from_source = TRUE,
		crop = "maize",
		is_survey = TRUE,
		field_id= "1"
	)


	#The dataset has three planting dates and 2 harvesting dates indicated in the r1$notes and r3$notes
	### We have two planting dates in the same year/season it might be replanted or it was gap filling but not clearly highlighted

# If used, these need to follow the data, not just assign them randomly 

	#d$planting_date[1:4] <- format(as.Date(c("17-11-2022","29-03-2023","05-10-2023","17-03-2024"))
	#d$harvest_date[1:2] <- format(as.Date(c("26-02-2023","14-07-2024"), format="%d-%m-%Y"),"%Y-%m-%d")
	
	
	carobiner::write_files(path, meta, d, long=long, wth=wth)
}


