# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them
#    bdd_kenya_nairobi_climate.csv has ~3.42 million rows (readings logged
#    multiple times per minute over 2022-2024) - read.csv() on this is slow
#    and memory-heavy. Left as base R read.csv(), but flagging for reviewers 

carob_script <- function(path) {
  
  "This dataset corresponds to climate data and maize pests monitoring of Busseola fusca, Chilo partellus, and Spodoptera frugiperda between 2022 and 2024 at Nairobi, Kenya (1°16’12”S, 36°48’17”E, altitude 1700 masl). Trapping data include site name, survey date, species, number of individuals, pheromone change dates, and any notes. Climatic data include date, temperature, relative humidity, and atmospheric pressure from a BME680 sensor acquired using a Raspberry Pi 3A+ single-board computer. All date and time data are in UTC. (2025-03-12)"
  
  uri <- "doi:10.23708/PFDYJQ"
  group <- "pest_disease"
  
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                  data_organization = NA,
                                  publication = NA,
                                  project = NA,
                                  data_type = "survey",
                                  treatment_vars = "pest_species;pest_number",
                                  response_vars ="none", 
                                  carob_completion = 100,
                                  carob_effort = 1,
                                  carob_contributor = "Mitchelle Njukuya",
                                  carob_date = "2026-08-12",
                                  notes = NA,
                                  design = NA
  )
  
  f1 <- ff[basename(ff) == "bdd_kenya_nairobi_bf.csv"]
  f2 <- ff[basename(ff) == "bdd_kenya_nairobi_cp.csv"]
  f3 <- ff[basename(ff) == "bdd_kenya_nairobi_climate.csv"]
  f4 <- ff[basename(ff) == "bdd_kenya_nairobi_sf.csv"]
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r3 <- read.csv(f3)
  r4 <- read.csv(f4)
  
  make_pest_df <- function(r) {
    data.frame(
      location = r$site,
      date = r$date,
      pest_species = r$species,
      pest_number = r$numberOfIndividuals,
      pheromone_change = r$pheromoneChange  #pheromone_change included because pheromone monitoring is one of the core tools in integrated pest management
    )
  }
  
  d1 <- make_pest_df(r1)
  d2 <- make_pest_df(r2)
  d3 <- make_pest_df(r4)
  
  d <- rbind(d1, d2, d3)
  
  # climate_data is kept separate from d and not merged: pest surveys (d) are 
  # recorded per trap visit (roughly daily/weekly), while climate readings are 
  # logged continuously (multiple readings per minute) by the automated sensor
  
  climate_data <- data.frame(
    sensor_id = "BME680",
    temp = r3$temperature,
    rhum = r3$rh,
    pressure = r3$pressure,
    date = as.character(as.Date(r3$dateUTC, format = "%Y-%m-%d %H:%M:%S")),
    time = substr(r3$dateUTC, 12, 19)
  )
  
  climate_data$location <- "Nairobi"
  climate_data$longitude <- 36.804722
  climate_data$latitude <- -1.270000
  climate_data$geo_from_source <- FALSE
  
  d$country <- "Kenya"
  d$adm1 <- "Nairobi"
  d$adm2 <- NA
  d$longitude <- 36.8047
  d$latitude <- -1.2700
  d$elevation <- 1700
  d$geo_from_source <- TRUE
  d$treatment <- NA
  d$crop <- "maize"
  d$yield_part <- "grain"
  d$trial_id <- as.character(as.integer(as.factor(1)))
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$planting_date <- NA
  d$harvest_date  <- NA
  d$yield <- d$yield_moisture <- d$yield_isfresh <- NA
  
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- d$lime <- d$fertilizer_type <- NA
  d <- unique(d)
  
  carobiner::write_files(path, meta, d, wth = climate_data)
}
