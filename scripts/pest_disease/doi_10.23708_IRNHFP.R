# R script for "carob"
# license: GPL (>=3)

##NOTES
# Data collection window in the raw files (2021-11-07 to 2024-07-31) starts
# earlier than the "2022-2024" stated in the dataset title/description.
# This is continuous pheromone-trap monitoring at one site (Dembwa), not a
# designed trial - no plots, no treatments, no replicates.

## Suggested new terms: trapped_pest_count, pheromone_change

###ISSUES
# numberOfIndividuals (trap catch count) has no direct terminag field -
# terminag's pest_number means number of pest SPECIES observed, not a count
# of individuals - so trap counts are kept in the general variable/value
# long-format columns instead, with variable="individuals_trapped".
# pheromoneChange (trap lure maintenance event) and notes (per-row free
# text) have no terminag field either - kept as interpreted columns.
# crop = "Maize" is inferred from the dataset description ("maize pests"),
# not an explicit column in the source data.

carob_script <- function(path) {
  
  "Corn pest and climate monitoring dataset from Taita Hill, Dembwa, Kenya, 2022-2024

This dataset corresponds to climate data and maize pests monitoring of
Busseola fusca, Chilo partellus, and Spodoptera frugiperda between 2022
and 2024 at Taita Hills, Dembwa, Kenya (3 26'48 S, 38 21'50 E, altitude
1090 masl). Trapping data include site name, survey date, species,
number of individuals, pheromone change dates, and any notes. Climatic
data include date, temperature, relative humidity and dew point. All
date and time data are in UTC. Measurements were taken using a Hobo
MX2301A sensor (ONSET)."
  
  uri <- "doi:10.23708/IRNHFP"
  group <- "pest_disease"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                  data_organization = "IRD",
                                  publication = NA,
                                  project = NA,
                                  design = "continuous pheromone-trap monitoring at a single site, 3 maize pest species, plus co-located weather station",
                                  data_type = "survey",
                                  treatment_vars = "pheromone_change",
                                  response_vars = "trapped_pest_count; pest_species",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-08-11",
                                  carob_completion = 70,
                                  carob_effort = 2
  )
  
  f1 <- ff[basename(ff) == "bdd_kenya_dembwa_taitaHills_bf.csv"]        # Busseola fusca trap counts
  f2 <- ff[basename(ff) == "bdd_kenya_dembwa_taitaHills_climate.csv"]  # climate: temp, rh, dewpoint, 30-min intervals
  f3 <- ff[basename(ff) == "bdd_kenya_dembwa_taitaHills_cp.csv"]       # Chilo partellus trap counts
  f4 <- ff[basename(ff) == "bdd_kenya_dembwa_taitaHills_sf.csv"]       # Spodoptera frugiperda trap counts
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r3 <- read.csv(f3)
  r4 <- read.csv(f4)
  
  # --- d1: Busseola fusca ---
  d1 <- data.frame(
    location    = r1$site,
    latitude    = -3.4467,    # from source description: 3 26'48" S
    longitude   = 38.3639,    # from source description: 38 21'50" E
    elevation   = 1090,
    geo_from_source = TRUE,
    is_survey   = TRUE,
    date        = as.character(as.Date(r1$date)),
    pest_species = r1$species,
    trapped_pest_count       = r1$numberOfIndividuals,
    pheromone_change = r1$pheromoneChange
  )
  
  # --- d3: Chilo partellus ---
  d3 <- data.frame(
    location    = r3$site,
    latitude    = -3.4467,
    longitude   = 38.3639,
    elevation   = 1090,
    geo_from_source = TRUE,
    is_survey   = TRUE,
    date        = as.character(as.Date(r3$date)),
    pest_species = r3$species,
    trapped_pest_count       = r3$numberOfIndividuals,
    pheromone_change = r3$pheromoneChange
  )
  
  # --- d4: Spodoptera frugiperda ---
  d4 <- data.frame(
    location    = r4$site,
    latitude    = -3.4467,
    longitude   = 38.3639,
    elevation   = 1090,
    geo_from_source = TRUE,
    is_survey   = TRUE,
    date        = as.character(as.Date(r4$date)),
    pest_species = r4$species,
    trapped_pest_count  = r4$numberOfIndividuals,
    pheromone_change = r4$pheromoneChange
  )
  
  d_pest <- rbind(d1, d3, d4)
  d_pest$record_id <- seq_len(nrow(d_pest))   # survey data - each row is its own record
  
  # --- d2: climate ---
  dt <- as.POSIXct(r2$dateUTC, tz = "UTC")
  d2 <- data.frame(
    location  = "Dembwa",
    latitude  = -3.4467,
    longitude = 38.3639,
    elevation = 1090,
    geo_from_source = TRUE,
    date = as.character(as.Date(dt)),
    time = format(dt, "%H:%M:%S"),   # plain "hh:mm:ss" string but still gives bad data type
    temp = r2$temperature,
    rhum = r2$rh,
    dewp = r2$dewpoint
  )
  d2$country <- "Kenya"
  d2$adm1 <- "Taita-Taveta"
  
  d <- d_pest
  d$country <- "Kenya"
  d$adm1 <- "Taita-Taveta"
  d$crop <- "maize"
  d$on_farm <- TRUE
  d$trial_id <- NA          # continuous monitoring, not divided into distinct trials
  d$yield <- NA
  d$yield_moisture <- NA
  d$yield_part <- NA
  d$yield_isfresh <- NA
  d$irrigated <- NA
  d$K_fertilizer <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$planting_date <- NA
  d$harvest_date <- NA
  
  carobiner::write_files(path, meta, wide=d, wth=d2)
}
