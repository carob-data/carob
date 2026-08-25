# R script for "carob"
# license: GPL (>=3)

## ISSUES
## dataset does not have planting and harvest dates
##dataset has new variables ie light quality include UV light, infrared and visible spectrum
#### added pheromone_date = the last date when the pheromone was changed
carob_script <- function(path) {
  
  "
Corn pest and climate monitoring dataset from Mbita points, Kenya, 2022-2024

This dataset corresponds to climate data and maize pests monitoring of Busseola fusca, Chilo partellus, and Spodoptera frugiperda between 2022 and 2024 at Mbita Points, Kenya (0°26'12.45'S, 34°12'21.70'E, altitude 1166 masl). Trapping data include site name, survey date, species, number of individuals, pheromone change dates, and any notes. Climatic data include date, temperature from a BME680 sensor, temperature from a DS18B20 sensor, relative humidity and atmospheric pressure from a BME680 sensor, and visible, IR and UV light from a SI1145 sensor. All date and time data are in UTC. Date were acquired using a Raspberry Pi 3A+ single-board computer.
"
  
  uri <- "doi:10.23708/VYBAMQ"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                  data_organization = "IRD",
                                  publication = NA,
                                  project = NA,
                                  design = NA,
                                  data_type = "survey",
                                  treatment_vars = "pheromone_change",
                                  response_vars = "pest_species;pest_number", 
                                  carob_contributor = "Illiana Kwenda",
                                  carob_date = "2026-08-05",
                                  carob_completion = 80,	
                                  carob_effort = 2
  )
  
  
  f1 <- ff[basename(ff) == "bdd_kenya_mbitaPoints_bf.csv"]
  f2 <- ff[basename(ff) == "bdd_kenya_mbitaPoints_climate.csv"]
  f3 <- ff[basename(ff) == "bdd_kenya_mbitaPoints_cp.csv"]
  f4 <- ff[basename(ff) == "bdd_kenya_mbitaPoints_sf.csv"]
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r3 <- read.csv(f3)
  r4 <- read.csv(f4)
  
  
  d1 <- data.frame(
    date = r1$date,
    pest_species = r1$species,
    trapped_pest_count = r1$numberOfIndividuals,
    pheromone_change = r1$pheromoneChange ### indicates if the pherome was changes or not
    #treatment = as.character(r1$pheromoneChange)
  )
  
  ### dataset provided location and geo_cordinates Mbita Points, Kenya (0°26'12.45'S, 34°12'21.70'E, altitude 1166 masl).
  #### Mbita point is in adm4 hence not available in GADM. But geo_uncertainty was obtained from adm3 = Kasgunga
  
  
  wth <- data.frame(
    location = "Mbita Point",
    temp = r2$temperature,
    rhum = r2$rh,
    visible_spectrum = r2$lightVisible,
    IR = r2$lightIR, #### infared light
    UV = r2$lightUV, ### uv light
    vapr = r2$pressure,
    longitude = 34.206,
    latitude = -0.437,
    geo_from_source = TRUE,
    date = substr(r2$dateUTC, 1, 10),
    time = substr(r2$dateUTC, 12, 19),
    timezone = "UTC"
  )
  
  wth$time[wth$time == ""] <- NA
  
  d3 <- data.frame(
    date = r3$date,
    pest_species = r3$species,
    trapped_pest_count = r3$numberOfIndividuals,
    pheromone_change = r3$pheromoneChange
  )
  
  
  d4 <- data.frame(
    date = r4$date,
    pest_species = r4$species,
    trapped_pest_count = r4$numberOfIndividuals,
    pheromone_change = r4$pheromoneChange
  )
  
  
  long <- carobiner::bindr(d1, d3, d4)
  long$record_id <- seq_len(nrow(long))
  
  
  long$pheromone_date <- as.Date(NA)
  
  last_change <- NA
  
  for (i in 1:nrow(long)) {
    
    if (long$pheromone_change[i] == TRUE) {
      last_change <- long$date[i]
    }
    
    long$pheromone_date[i] <- last_change
  }
  
  long$hhid <- as.character(long$record_id)
  
  d <- data.frame(
    country = "Kenya",
    location = "Mbita point",
    longitude = 34.206,
    latitude = -0.437,
    geo_from_source = TRUE,
    crop = "maize",
    is_survey = TRUE,
    on_farm = FALSE,
    irrigated = FALSE
  )
  
  d <- d[rep(1, nrow(long)), ]
  d$record_id <- long$record_id
  
  carobiner::write_files(path, meta, d, long=long, wth=wth)
}