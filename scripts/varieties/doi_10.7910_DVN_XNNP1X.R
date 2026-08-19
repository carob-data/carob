# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
  "
Replication Data for: Genetic Diversity of improved varieties of intraspecific 
(O. sativaand O. glaberrima) and interspecific (O. sativa x O. glaberrima) rice
"
 
  uri <- "doi:10.7910/DVN/XNNP1X"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                  data_organization = "AfricaRice",
                                  publication = "doi:10.1007/s10722-017-0573-6",
                                  project = NA,
                                  design = NA,
                                  data_type = "experiment",
                                  treatment_vars = "variety",
                                  response_vars = "plant_height, grain_fill, heading_days", 
                                  notes = NA,
                                  carob_contributor = "Kora Simperegui",
                                  carob_date = "2026-08-18",
                                  carob_completion = 100,	
                                  carob_effort = 2
  )
  
  
  f <- ff[basename(ff) == "Genetic Diversity - Part 1.xlsx"]
  
  r <- carobiner::read.excel(f)
  
  ## process	
  d <- data.frame(
    trial_id = paste(r$Location, r$Variety, r$Bloc, r$Year, sep="-"), 
    block_id = as.character(r$Bloc),
    date = as.character(r$Year),
    country = "Benin",
    location = r$Location,
    crop = "rice", 
    variety = r$Variety,
    plant_height = r$PHt,
    grain_fill = r$GrnFlg,
    heading_days = r$HD,
    on_farm = FALSE, 
    is_survey = FALSE, 
    planting_date = as.character(NA)
  )
  
  #adding the coordinates. Observation are all from Cotonou, Benin
  d$longitude <- c(2.4182)
  d$latitude <- c(6.3758)
  d$geo_from_source <- FALSE
  
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  carobiner::write_files(path, meta, d)
}



