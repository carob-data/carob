# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The only file get_data() retrieves is a DataCite citation/metadata
# record (10.25502487s-4d54d.xml) 

carob_script <- function(path) {
  
  "
Assessment of 20 yam clones (D. alata) in advanced performance trial in Ibadan, 2017/2018

Assessment of 20 yam clones (D. alata) for tuber appearance, disease resistance, and yield characteristics in 
Ibadan, 2017/2018
"
  
  uri <- "doi:10.25502/487s-4d54/d"
  group <- "varieties"
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
                                  carob_date = "2026-08-28",
                                  carob_completion = 100,
                                  carob_effort = 1
  )
  
  f1 <- ff[basename(ff) == "10.25502487s-4d54d.xml"]
  r1 <- xml2::read_xml(f1)
  
}
