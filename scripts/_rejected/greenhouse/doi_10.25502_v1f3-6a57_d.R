# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Rected the dataset because it is not a field experiment, and also there are no variables to standardize as it was done in the greenhouse


carob_script <- function(path) {
  
  "
Yam tuber production in the aeroponics - 2018

Propagation of clean seed yam in high ratio propagation techniques(aeroponics).
"
  
  uri <- "doi:10.25502/v1f3-6a57/d"
  group <- "rejected"
  ff  <- carobiner::get_data(uri, path, group)
  
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
       data_organization = "IITA",
       publication = NA,
       project = NA,
       design = NA,
       data_type = "experiment",
       treatment_vars = "none",
       response_vars = "", 
       notes = NA,
       carob_contributor = "Blessing Dzuda",
       carob_date = "2026-09-15",
       carob_completion = 0,	
       carob_effort = 1
  )
  
  
  f1 <- ff[basename(ff) == "2018_aerotuberharvestdataset-withvarieties.csv"]
  f2 <- ff[basename(ff) == "aerotuberharvestdataset_metadata.csv"]
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  
  carobiner::write_files(path, meta, d)
}

