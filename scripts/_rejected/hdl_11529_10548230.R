# R script for "carob"
# license: GPL (>=3)

## REJECTED
## Upon investigation, this dataset cannot be standardized for carob.
##
## File 1: TAMASA_TZ_APS_CS_2016.xlsx (42 communities, 205 columns)
## - Contains community-level market prices (cereals, legumes, horticulture, 
##   roots, fertilizers) and labor costs (clearing, hoeing, planting, weeding,
##   harvesting) in Tanzanian Shillings (TSH)
## - No crop yield, fertilizer application rates or agronomic trial data
##
## File 2: TAMASA_TZ_APS_HH_2016.xlsx (607 households, 26 columns)
## - Contains only household location and ID variables
## - Actual crop/yield/management data was collected in separate linked 
##   crop cut files not included in this dataset
## - Column "MATCH_CODE" (described as "unique farm ID for matching to 
##   crop cut data") confirms this file is incomplete without the linked data
##
## Neither file contains core carob variables (yield, fertilizer rates, 
## planting dates, crop management). No standardization is possible.

carob_script <- function(path) {
  
  "
TAMASA Tanzania APS household data for 2016

This dataset consists of community-level data on 75 communities in 25 
maize-growing districts in Tanzania.
"
  
  uri <- "hdl:11529/10548230"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
                                  data_organization = "CIMMYT",
                                  publication = "",
                                  project = "TAMASA",
                                  design = NA, # survey, not experiment
                                  data_type = "survey",
                                  treatment_vars = "", # no treatments - market price survey
                                  response_vars = "", # no yield or agronomic response variables present
                                  notes = "Dataset rejected - see comments at top of script",
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-06-29",
                                  carob_completion = 0, # no variables could be standardized
                                  carob_effort = 1 # hours spent investigating
                                  )
  
  f1 <- ff[basename(ff) == "TAMASA_TZ_APS_CS_2016.xlsx"]
  f2 <- ff[basename(ff) == "TAMASA_TZ_APS_HH_2016.xlsx"]
  
  # f1: community survey - 42 communities, 205 columns
  # Contains market prices and labor costs only - no agronomic data
  r1c <- carobiner::read.excel(f1, sheet="data")
  
  # f2: household survey - 607 households, 26 columns
  # Contains location/ID variables only - crop data in missing linked files
  r2c <- carobiner::read.excel(f2, sheet="Data")
  
  # Location variables are available from both files but cannot be used
  # without corresponding yield/management data
  # latitude/longitude present in both files but no response variables to link them to
  
  # No further standardization possible - script ends here
}