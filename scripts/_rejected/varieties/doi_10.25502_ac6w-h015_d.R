# R script for "carob"
# license: GPL (>=3)

## REJECTED
# No location data at all - both coverage and coverage.country are blank.
# No plant/genotype trial data

carob_script <- function(path) {
  
  "
Vital readings of yam aeroponics nutrient solution

Seed yam production using high ratio propagation techniques.
"
  
  uri <- "doi:10.25502/ac6w-h015/d"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "IITA",
                                  publication = NA,
                                  project = NA,
                                  design = NA,
                                  data_type = NA,
                                  treatment_vars = "",
                                  response_vars = "",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-08-23",
                                  carob_completion = 100,
                                  carob_effort = 1
  )
  
  f1 <- ff[basename(ff) == "nutrientdata.csv"]
  f2 <- ff[basename(ff) == "nutrientdata_data_dictionary.csv"]
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  
}
