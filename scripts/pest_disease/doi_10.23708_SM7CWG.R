# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them

carob_script <- function(path) {
  
  "This dataset corresponds to climate data and maize pests monitoring of Busseola fusca, Chilo partellus, and Spodoptera frugiperda between 2022 and 2024 at Msambweni (Kwale), Kenya (4°20’38”S, 39°29’21”E, altitude 20 masl). Trapping data include site name, survey date, species, number of individuals, pheromone change dates, and any notes. Climatic data are composed of three files: (i) bdd_kenya_muhaka_field_climate.csv with temperature, relative humidity and dew point from a Hobo MX2301A sensor (ONSET(r)) close to the traps ; (ii) bdd_kenya_muhaka_lab_climate.csv with temperature, relative humidity and dew point from a Hobo MX2301A sensor (ONSET(r)) from a nearby building ; (iii) bdd_kenya_muhaka_rpi_climate.csv with temperature from a BME680 sensor, temperature from a DS18B20 sensor, relative humidity and atmospheric pressure from a BME680 sensor, and visible, IR and UV light from a SI1145 sensor acquired using a Raspberry Pi 3A+ single-board computer in a nearby building. All date and time data are in UTC. (2025-03-12)"
  
  uri <- "doi:10.23708/SM7CWG"
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
                                  carob_effort = 3,
                                  carob_contributor = "Mitchelle Njukuya",
                                  carob_date = "2026-08-01",
                                  notes = NA,
                                  design = NA
  )
  
  f1 <- ff[basename(ff) == "bdd_kenya_muhaka_bf.csv"]
  f2 <- ff[basename(ff) == "bdd_kenya_muhaka_cp.csv"]
  f3 <- ff[basename(ff) == "bdd_kenya_muhaka_field_climate.csv"]
  f4 <- ff[basename(ff) == "bdd_kenya_muhaka_lab_climate.csv"]
  f5 <- ff[basename(ff) == "bdd_kenya_muhaka_rpi_climate.csv"]
  f6 <- ff[basename(ff) == "bdd_kenya_muhaka_sf.csv"]
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r3 <- read.csv(f3)
  r4 <- read.csv(f4)
  r5 <- read.csv(f5)
  r6 <- read.csv(f6)
 
  d1 <- data.frame(
    location = r1$site,
    date = r1$date,
    pest_species = r1$species,
    pest_number = r1$numberOfIndividuals,
    pheromone_change = r1$pheromoneChange  #pheromone_change included because pheromone monitoring is one of the core tools in integrated pest management
  )
  
  d2 <- data.frame(
    location = r2$site,
    date = r2$date,
    pest_species = r2$species,
    pest_number = r2$numberOfIndividuals,
    pheromone_change = r2$pheromoneChange  #pheromone_change included because pheromone monitoring is one of the core tools in integrated pest management
  )
   
  d3 <- data.frame(
    locality = "field climate_near trap",     #locality was based on placement of climate sensors in microenvironments (field, adjacent to traps; lab/building)
    sensor_id = "Hobo MX2301A",               #data on sensors was retained as a distinct variable because multiple sensor models (Hobo MX2301A, BME680, DS18B20, SI1145) independently measured overlapping variables     
    temp = r3$temperature,
    rhum = r3$rh,
    dwep = r3$dewpoint
  ) 
  
  r3$dateUTC <- as.POSIXct(r3$dateUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Create the date and time columns in d3
  d3$date <- as.character(as.Date(r3$dateUTC))
  d3$time <- format(r3$dateUTC, "%H:%M:%S") 
  
  d4 <- data.frame(
    locality = "lab climate_near building",     #locality was based on placement of climate sensors in microenvironments (field, adjacent to traps; lab/building)
    sensor_id = "Hobo MX2301A",                 #data on sensors was retained as a distinct variable because multiple sensor models (Hobo MX2301A, BME680, DS18B20, SI1145) independently measured overlapping variables     
    temp = r4$temperature,
    rhum = r4$rh,
    dwep = r4$dewpoint
  ) 
  
  r4$dateUTC <- as.POSIXct(r4$dateUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Create the date and time columns in d3
  d4$date <- as.character(as.Date(r4$dateUTC))
  d4$time <- format(r4$dateUTC, "%H:%M:%S") 

  d5 <- data.frame(
    locality = "rpi_near building",       # data acquired using a Raspberry Pi 3A+ single-board computer in a nearby building
    sensor_id = "BME680",                 # sensor_id retained as a distinct variable because multiple sensor models
    # (Hobo MX2301A, BME680, DS18B20, SI1145) independently measured overlapping variables
    temp = r5$temperature,
    rhum = r5$rh,
    pressure = r5$pressure,
    visible_light = r5$lightVisible,
    IR_light = r5$lightIR,
    UV_light = r5$lightUV
  )
  
  r5$dateUTC <- as.POSIXct(r5$dateUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  d5$date <- as.character(as.Date(r5$dateUTC))
  d5$time <- format(r5$dateUTC, "%H:%M:%S")
  
  d6 <- data.frame(
    locality = "rpi_near building",
    sensor_id = "DS18B20",                # DS18B20 only measures temperature — all other fields are NA, not borrowed from BME680
    temp = r5$temperatureDS18B20,
    rhum = NA,
    pressure = NA,
    visible_light = NA,
    IR_light = NA,
    UV_light = NA
  )
  d6$date <- as.character(as.Date(r5$dateUTC))
  d6$time <- format(r5$dateUTC, "%H:%M:%S")
  
  d7 <- data.frame(
    location = r6$site,
    date = r6$date,
    pest_species = r6$species,
    pest_number = r6$numberOfIndividuals,
    pheromone_change = r6$pheromoneChange  #pheromone_change included because pheromone monitoring is one of the core tools in integrated pest management
  )
  
  d8 <- rbind(d1, d2, d7) 
  d9 <- rbind(d3, d4)
  d10 <- rbind(d5, d6)
  
  # align columns so both have the same set before stacking
  d9_cols <- c("locality","sensor_id","temp","rhum","dwep","pressure",
                "visible_light","IR_light","UV_light","date","time")
  
  d9$pressure <- NA; d9$visible_light <- NA; d9$IR_light <- NA; d9$UV_light <- NA
  d10$dwep <- NA
  
  d9 <- d9[, d9_cols]
  d10 <- d10[, d9_cols]
  
  # climate_data is kept separate from d and not merged: pest surveys (d) are 
  # recorded per trap visit (roughly daily/weekly), while climate readings are logged 
  # every 10-30 min by automated sensors
  
  climate_data <- rbind(d9, d10)
  d <- d8

  d$country <- "Kenya"
  d$adm1 <- "Kwale"
  d$adm2 <- "Msambweni"
  d$longitude <- 39.48917
  d$latitude <- -4.34389
  d$elevation <- 20
  d$geo_from_source <- FALSE
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

  d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- d$S_fertilizer <- d$lime <- d$fertilizer_type <- NA
  d <- unique(d)
  
  carobiner::write_files(path, meta, d)
}

