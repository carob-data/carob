# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
  "
Effects of artisanal parboiling steaming time and variety 
on grain quality, mineral and digestive properties of rice
"
  
  uri <- "doi:10.7910/DVN/EI6VE8"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
                                  data_organization = "AfricaRice",
                                  publication = "doi:10.1002/fsn3.600",
                                  project = NA,
                                  design = NA,
                                  data_type = "experiment",
                                  treatment_vars = "variety",
                                  response_vars = "grain_protein; grain_P, grain_K; grain_Mg; grain_Na", 
                                  notes = NA,
                                  carob_contributor = "Kora Simperegui",
                                  carob_date = "2026-09-07",
                                  carob_completion = 100,	
                                  carob_effort = 2
  )
  
  
  f <- ff[basename(ff) == "Effects of artisanal parboiling.xls"]
  
  r <- carobiner::read.excel(f)
  
  
  d <- data.frame(
    trial_id = paste(r$Location, r$Year, sep="-"),
    date = as.character(r$Year),
    country = "Benin",
    rep = as.integer(ifelse(r$Rep == "R1",1, ifelse(r$Rep == "R2", 2, 3))),
    crop = "rice", 
    variety = r$Variety,
    grain_protein = r$Protein,
    grain_P = r$Phosphorus*10, # from % to mg/g (x10000 % to mg/kg)/1000(kg to g)
    grain_K = r$Potassium/1000, # from  mg/kg to mg/g
    grain_Mg = r$Magnesium/1000, # from  mg/kg to mg/g
    grain_Na = r$Sodium/1000, # from  mg/kg to mg/g
    is_survey = FALSE, 
    on_farm = FALSE, 
    planting_date = as.character(NA)
    )
  
  #adding the coordinates. Observation are all from Cotonou, Benin
  d$longitude <- 2.4182
  d$latitude <- 6.3758
  d$geo_from_source <- FALSE
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  d$irrigated <- d$harvest_date <- d$yield <- d$yield_moisture <- d$yield_isfresh  <- NA
  d$yield_part <- "grain"
  
  carobiner::write_files(path, meta, d)
}



