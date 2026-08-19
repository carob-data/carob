# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
  "
Farm-Level Rice Yield Data and Climate Change Impact Projections Across Rice Ecologies in West Africa (2012–2080)
  The Baseline Data set (16,049 records, 20 variables): Contains observed farm-level data collected between 2012 and 2020. 
  Each record corresponds to a unique plot (identified by a code) and includes baseline rice yield (t/ha), key climate variables 
  (maximum and minimum temperature, precipitation, relative humidity, solar radiation), site characteristics (elevation), soil properties 
  (available soil water capacity, total nitrogen, pH, soil organic carbon, clay and sand content), and fertilizer inputs (N, P, K).
  "
  
  uri <- "doi:10.5281/zenodo.21075805"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
                                  data_organization = "AfricaRice",
                                  publication = NA,
                                  project = NA,
                                  design = NA,
                                  data_type = "experiment",
                                  treatment_vars = NA,
                                  response_vars = NA, 
                                  notes = NA,
                                  carob_contributor = "Kora Simperegui",
                                  carob_date = "2026-08-19",
                                  carob_completion = 100,	
                                  carob_effort = 5
  )
  
  f <- ff[basename(ff) == "Dataset_modeling%20and%20impact%20assessment.xlsx"]
  r <- carobiner::read.excel(f)
  
  d <- data.frame(
    trial_id = r$Code,
    country=r$Country,
    crop="rice",
    yield=r$Baseline_Rice_Yield*1000,
    soil_N=r$Ntot,
    soil_SOC=r$SOC,
    # soil_pH=r$pH_H2O, These values do not make sens. They were not included
    soil_clay=r$Clay_cont,
    soil_sand=r$sand_cont,
    N_fertilizer = r$Ninput,
    P_fertilizer = r$Pinput,
    K_fertilizer = r$Kinput,
    irrigated = grepl("IL", r$Ecology),
    season = ifelse(r$Ecology=="IL-Dry","dry","wet"), # Four different ecologies are represented (Irrigated-Lowland(IL) in dry season(IL-DRY) and IL-Wet, Rainfed Lowland and Rainfed Upland)
    prec=r$Precipitation_baseline,
    tmin=r$Min_Temperature_baseline,
    tmax=r$Max_Temperature_baseline,
    rhum=r$Relative_humidity_baseline,
    srad=r$Solar_Radiation_baseline
    )
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$planting_date <- NA
  d$harvest_date  <- NA
  d$yield_part <- "grain"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- NA
  d$country <- gsub("Cote d'Ivoire","Côte d'Ivoire",d$country)
  d$country <- gsub("The Gambia","Gambia",d$country)
  
  
  carobiner::write_files(path, meta, d)
}
