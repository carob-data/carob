# R script for "carob"

carob_script <- function(path) {
  
"The Harvest Plus Yield Trial (HPYT) contains spring bread wheat (Triticum aestivum) germplasm adapted to ME1 (Optimally Irrigated, low rainfall environment) and ME5 (Warmer area environment) environments. It has total 50 entries with 2 replications, white grain color and distributed to more than 70 locations. (2016)"
  
  uri <- "hdl:11529/10548445"
  group <- "varieties_wheat"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
    data_organization = "CIMMYT",
    publication= NA,
    project="Harvest Plus Advanced Nursery",
    data_type= "experiment",
    response_vars = "yield",
    treatment_vars = "variety_code",
    carob_contributor= "Fredy Chimire",
    carob_effort = NA,
    carob_date="2024-11-01"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)
  
    carobiner::write_files(path, meta, d$wide, d$long)
}
