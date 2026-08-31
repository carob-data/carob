# R script for "carob"
# license: GPL (>=3)

## ISSUES
## time datatype still fails validation 

carob_script <- function(path) {

"
Corn pest and climate monitoring dataset from Taita Hill, Maktau, Kenya, 2022-2024

This dataset corresponds to climate data and maize pests monitoring of Busseola fusca, Chilo partellus, 
and Spodoptera frugiperda between 2022 and 2024 at Taita Hills, Maktau, Kenya (3°25'33'S, 38°8'23'E, altitude 1090 masl). 
Trapping data include site name, survey date, species, number of individuals, pheromone change dates, and any notes. 
Climatic data include date, temperature, relative humidity and dew point. 
All date and time data are in UTC. Measurements were taken using a Hobo MX2301A sensor (ONSET(r)).
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
		response_vars = "pest_species", 
		carob_contributor = "Illiana Kwenda",
		carob_modified_by = "Stella Muthoni",
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
	
	# source-flagged correction
	r3$species[r3$species == "Chilo partellus" & r3$date == "2022-10-22"] <- "Busseola fusca"
	
	# planting/harvest dates extracted from r1$notes/r3$notes, each
	# verified against the row it actually appears on.
	planting_dates <- as.Date(c("2021-11-27","2022-11-17","2023-03-29","2023-10-05","2024-03-17"))
	harvest_dates  <- as.Date(c("2023-02-26","2024-07-14"))
	
	tag_events <- function(df) {
	  df$planting_date <- as.character(ifelse(as.Date(df$date) %in% planting_dates, as.character(as.Date(df$date)), NA))
	  df$harvest_date  <- as.character(ifelse(as.Date(df$date) %in% harvest_dates, as.character(as.Date(df$date)), NA))
	  df
	}
	
	# --- d1: Busseola fusca ---
	d1 <- data.frame(
	  date        = r1$date,
	  pest_species = r1$species,
	  pheromone_change = as.character(r1$pheromoneChange),
	  pest_incidence = r1$numberOfIndividuals
	)
	d1 <- tag_events(d1)
	
	# --- d3: Chilo partellus ---
	d3 <- data.frame(
	  date        = r3$date,
	  pest_species = r3$species,
	  pheromone_change = as.character(r3$pheromoneChange),
	  pest_incidence = r3$numberOfIndividuals
	)
	d3 <- tag_events(d3)
	
	# --- d4: Spodoptera frugiperda ---
	d4 <- data.frame(
	  date        = r4$date,
	  pest_species = r4$species,
	  pheromone_change = as.character(r4$pheromoneChange),
	  pest_incidence = r4$numberOfIndividuals
	)
	
	d4 <- tag_events(d4)
	
	d_pest <- rbind(d1, d3, d4)
	d_pest$record_id <- seq_len(nrow(d_pest))   # survey data - each row is its own record  
	d_pest$pheromone_date <- NA
	
	last_change <- d_pest$date[1]
	for (i in 1:nrow(d_pest)) {
	  if (d_pest$pheromone_change[i]) {
	    last_change <- d_pest$date[i]
	  }
	  d_pest$pheromone_date[i] <- last_change
	}
	d_pest$record_id = 1L
	d_pest$pheromone_change <- NULL

	# --- d2: climate ---
	dt <- as.POSIXct(r2$dateUTC, tz = "UTC")
	wth <- data.frame(
	  country = "Kenya",
	  adm1 = "Taita-Taveta",
	  location  = "Maktau",
	  latitude  = -3.4258,
	  longitude = 38.1397,
	  elevation = 1090,
	  geo_from_source = TRUE,
	  date = as.character(as.Date(dt)),
	  time = format(dt, "%H:%M:%S", tz = "UTC"),
	  temp = r2$temperature,
	  rhum = r2$rh,
	  dewp = r2$dewpoint,
	  stringsAsFactors = FALSE
	)
	
	d <- data.frame(
	  country = "Kenya",
	  adm1 = "Taita-Taveta",
	  location  = "Maktau",
	  latitude  = -3.4258,
	  longitude = 38.1397,
	  elevation = 1090,
	  geo_from_source = TRUE,
	  crop = "maize",
	  on_farm = TRUE,
	  is_survey = TRUE,
	  record_id = 1L
	)
	
	carobiner::write_files(path, meta, wide=d, long=d_pest, wth=wth)
	
}


