# R script for "carob"


carob_script <- function(path) {
  
"The International Bread Wheat Screening Nursery (IBWSN) is designed to rapidly assess a large number of advanced generation (F3-F7) lines of spring bread wheat under Mega-environment 1 (ME1) which represents diversity for a wide range of latitudes, climates, daylengths, fertility conditions, water management, and (most importantly) disease conditions. The distribution of these nurseries is deliberately biased toward the major spring wheat regions of the world where the diseases of wheat are of high incidence. It is distributed to 180 locations and contains 300-450 entries. (2016)"

  uri <- "hdl:11529/10548040"
  group <- "varieties_wheat"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=5, minor=1,
    data_organization = "CIMMYT",
    publication= NA,
    project="International Bread Wheat Screening Nursery",
    data_type= "experiment",
    carob_contributor= "Fredy Chimire",
    carob_date="2024-08-13",
    response_vars = "yield",
    treatment_vars = "variety_code"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)	
  
  i <- d$location == "Ningxia Aaf"
  d$longitude[i] <- 106.2667 
  d$latitude[i] <- 38.46667
  
  i <- d$location == "Wad Medani- Gezira Res. Stn.-Arc"
  d$longitude[i] <- 33.48333 
  d$latitude[i] <- 14.4

  carobiner::write_files(path, meta, d)
}

