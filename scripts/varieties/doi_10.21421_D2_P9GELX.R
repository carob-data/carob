# R script for "carob"


carob_script <- function(path) {
   
   " Medium-Duration Groundnut for Multi-location evaluation comprises 15 breeding lines of groundnut and 3 released Varieties. The trial was conducted under rainfed condition and it was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses location which includes; Zango to confirm their " 
   
   uri <- "doi:10.21421/D2/P9GELX"

   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization = "ICRISAT", 
      publication =NA, 
      project = NA, 
      data_type = "experiment", 
      treatment_vars = "variety", 
      response_vars = "dmy_storage",
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-08-01"
   )
   
f <- ff[basename(ff)=="Data file of Medium duration lines for agronomic traits in the location Zango..xlsx"]

## read file
  r <- carobiner::read.excel(f)
  
## Process file    
  d <- data.frame(
     rep= as.integer(r$`Replication number`),
     variety= r$Variety,
     emergence_days= r$`Date of emergence`,
     flowering_days= r$`Date of first flower`,
     dmy_residue= r$DFWkgha,
     dmy_storage= r$DPWkgha,
     fwy_residue= r$Fykg_ha,
     seed_weight = as.numeric(r$HSW)* 10, ## 1000 seed weight
     shelling_percentage= r$`Shelling ratio`,
     maturity_days= r$DM
     
      )     
   
   d$country <- "Nigeria"
   d$location <- "Zango"
   d$crop <- "groundnut"
   d$trial_id <- "1"
   
   d$irrigated <- as.logical(NA)
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "pod"
   
   d$planting_date <- "2015"
   d$longitude  <- 8.5446236
   d$latitude  <- 12.95365
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)    
}


