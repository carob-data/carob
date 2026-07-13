# R script for "carob"


carob_script <- function(path) {
  
"The Karnal Bunt Screening Nursery is a single replicate nursery that contains diverse spring bread wheat
(Triticum aestivum) germplasm adapted to ME1 (Optimally irrigated, low rainfall environment) with total 50-100 entries 
and white/red grain color. (2021)"

  uri <- "hdl:11529/10548727"
  group <- "varieties_wheat"
  ff <- carobiner::get_data(uri, path, group)

  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
    project="Karnal Bunt Screening Nursery",
    publication = NA,
    data_organization = "CIMMYT",
    carob_contributor="Mitchelle Njukuya",
    carob_effort = NA,
    carob_date="2024-05-02",   
    data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)
    carobiner::write_files(path, meta, d$wide, d$long)
}
