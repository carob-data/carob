# R script for "carob"

carob_script <- function(path) {
  
"
Group B1, cycle B1C5 of Population B (fifth cycle of recombination of the pure native Andigena group B1), is the result of a new population improvement strategy in the absence of R- genes started at CIP in 1990. The group B1 derives from the primitive cultivars of Solanum tuberosum ssp. andigena, known to be free of R genes.  These clones were planted in a randomized complete block design (RCBD) with 2-4 replicates at Comas, located at 2400 to 2788 masl in Junin situated in the Central mountain ranges in Peru.  The trials were established at Comas due to high disease pressure of late blight, disease endemic, highly favorable for disease development and severity during the rainy periods in these areas from 2005-2006.
"
  
  uri <- "doi:10.21223/P3/NQBNWX"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
    
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable;AUDPC;rAUDPC", 
    carob_contributor = "Henry Juarez",
    carob_date = "2024-09-13",
    notes = NA
  )
  
  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[!grepl("Dictionary_Lateblight.xls", basename(ff))]
  f <- f[grep("^0._|_Data_dictionary|_processed", basename(f), invert=TRUE)]
  d <- lapply(f, process, addvars="TTWP")
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
}

