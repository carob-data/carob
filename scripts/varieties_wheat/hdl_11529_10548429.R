# R script for "carob"

carob_script <- function(path) {
  
"2nd Harvest Plus Advanced Nursery
  
The Harvest Plus Advanced Nursery (HPAN) contains spring bread wheat (Triticum aestivum) germplasm adapted to ME1 (Optimally Irrigated, low rainfall environment) and ME5 (Warmer area environment) environments. It has 100-200 entries with a single replication, white grain color and distributed to more than 35 locations. (2020)  
"

  uri <- "hdl:11529/10548429"
  group <- "varieties_wheat"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
    data_organization = "CIMMYT",
    publication= NA,
    project="Harvest Plus Advanced Nursery",
    data_type= "experiment",
    carob_contributor= "Blessing Dzuda",
    carob_date="2025-11-12",
    response_vars = "yield",
    treatment_vars = "variety_code"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)	

  carobiner::write_files(path, meta, d)
}
