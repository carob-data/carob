Carob_script <- function(path) {
  
  "16th Acid Soil Wheat Screening Nursery
  
  The Acid-Soils Wheat Screening Nursery (ASWSN) contains spring bread wheat germplasm adapted to areas affected by low pH that are predominately in Brazil, the highlands of central Africa, and the Himalayas; the total estimated area is close to 2 million hectares. Disease and stress problems are similar to ME2. However, aluminum and manganese toxicities, plus phosphorus deficiency, are major constraints to production. Red grain is generally preferred, except in the Himalayas. High-level quality is demanded, especially in Latin America. (2004)"
  
  uri <- "hdl:11529/10548422"
  group <- "varieties_wheat"
  ff <- carobiner::get_data(uri, path, group)
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                  data_organization = "CIMMYT",
                                  publication=NA,
                                  project="Acid Soil Wheat Screening Nursery",
                                  data_type= "experiment",
                                  response_vars = "yield",
                                  treatment_vars = "variety_code",
                                  carob_contributor= "Blessing Dzuda",
                                  carob_date="2025-09-01",
                                  completion = 100,	
                                  design=NA,
                                  notes = NA
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)
  d$yield_moisture <- as.numeric(NA)
  carobiner::write_files(path, meta, d)
}
