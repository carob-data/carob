# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
  "
Replication Data for: Estimation of outcrossing rates in intraspecific (Oryza sativa) 
and interspecific (Oryza sativa x Oryza glaberrima) rice under field conditions using 
using agro-morphological markers
"

  uri <- "doi:10.7910/DVN/GIOAPK"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
                                  data_organization = "AfricaRice",
                                  publication = "https://doi.org/10.1007/s10681-017-1872-x",
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
  
  
  f <- ff[basename(ff) == "Outcrossing trial.xlsx"]

  r <- carobiner::read.excel(f)
  
	
  d <- data.frame(
    trial_id = paste(r$Location, r$Cultivar, r$Bloc, r$Year, sep="-"), 
    block_id = r$Bloc,
    date = as.character(r$Year),
    country = "Benin",
    location = r$Location,
    crop = "rice", 
    variety = r$Cultivar,
    plant_height = r$PHt,
    grain_fill = r$GrnFlg,
    heading_days = r$Heading,
    on_farm = FALSE, 
    is_survey = FALSE, 
    planting_date = as.character(NA)
  )
  
  #adding the coordinates. Observation are all from Cotonou, Benin
  d$longitude <- 2.4182
  d$latitude <- 6.3758
  d$geo_from_source <- FALSE

  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  carobiner::write_files(path, meta, d)
}



