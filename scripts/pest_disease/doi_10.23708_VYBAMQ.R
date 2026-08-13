# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
  
  "This dataset corresponds to climate data and maize pests monitoring of Busseola fusca, Chilo partellus, and Spodoptera frugiperda between 2022 and 2024 at Mbita Points, Kenya (0°26'12.45 S, 34°12'21.70 E, altitude 1166 masl). Trapping data include site name, survey date, species, number of individuals, pheromone change dates, and any notes. Climatic data include date, temperature from a BME680 sensor, temperature from a DS18B20 sensor, relative humidity and atmospheric pressure from a BME680 sensor, and visible, IR and UV light from a SI1145 sensor. All date and time data are in UTC. Date were acquired using a Raspberry Pi 3A+ single-board computer."
  
  uri <- "doi:10.23708/VYBAMQ"
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
  
  f1 <- ff[basename(ff) == "bdd_kenya_mbitaPoints_bf.csv"]
  f3 <- ff[basename(ff) == "bdd_kenya_mbitaPoints_cp.csv"]
  f2 <- ff[basename(ff) == "bdd_kenya_mbitaPoints_climate.csv"]
  f4 <- ff[basename(ff) == "bdd_kenya_mbitaPoints_sf.csv"]
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
  d2 <- make_pest_df(r3)
  d3 <- make_pest_df(r4)
  
  d <- rbind(d1, d2, d3)
  
  # climate_data is kept separate from d and not merged: pest surveys (d) are 
  # recorded per trap visit (roughly daily/weekly), while climate readings are 
  # logged continuously (multiple readings per minute) by the automated sensor
  
  climate_data <- data.frame(
    sensor_id = "BME680",
    temp = r2$temperature,
    rhum = r2$rh,
    pressure = r2$pressure,
    date = as.character(as.Date(r2$dateUTC, format = "%Y-%m-%d %H:%M:%S")),
    time = substr(r2$dateUTC, 12, 19)
  )
  
  climate_data$location <- "Mbita"
  climate_data$longitude <-  34.2060
  climate_data$latitude <- -0.4368
  climate_data$geo_from_source <- TRUE
  
  d$country <- "Kenya"
  d$adm1 <- "Homa Bay County"
  d$adm2 <- NA
  d$longitude <- 34.2060
  d$latitude <- -0.4368
  d$elevation <- 1166
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
