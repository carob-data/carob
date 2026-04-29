# R script for "carob"

carob_script <- function(path) {
  
"
B3C2 is the second cycle of recombination of advanced B3C1 potato clones. These clones have high levels of  horizontal resistance to late blight in absence of R-genes, and they also have high tuber yield. These clones  were planted in a randomized complete block design (RCBD) with 2-4 replicates at Comas,  located at 2400 to 2788 masl in Junin situated in the Central mountain ranges in Peru.  The trials were established at Comas due to high disease pressure of late blight, disease endemic, highly favorable for disease development and severity during the rainy periods  in these areas from 1999 to 2005.
"
  
  uri <- "doi:10.21223/P3/MWOJGR"
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

