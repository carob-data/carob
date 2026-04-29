# R script for "carob"


carob_script <- function(path) {
  
"The objective of this study was determinate the phenotypic stability for resistance to late blight of 15 late blight resistant B3 clones during 4 seasons in two locations in Peru. The experiments were performed under a randomized complete block designs (RCBD) with 4 replications of 40 hill-plots. All the plants were exposed to natural infection in the field and disease spread was enhanced by spreader rows planted systematically throughout the field and also by favorable weather conditions."
  
  uri <- "doi:10.21223/P3/4FTDO8"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
      data_organization = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable", 
      carob_contributor = "Henry Juarez",
      carob_date = "2024-09-13",
      notes = NA
  )
  
  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[grep("PTLB", basename(ff))]
  f <- f[grep("^0._|_Data_dictionary", basename(f), invert=TRUE)]
  
  d <- lapply(f, process)
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
  
}

