# R script for "carob"

carob_script <- function(path) {
  
"
B3C1 is the first cycle of recombination of advanced B3C0 potato clones. These clones have high levels of horizontal resistance to late blight in absence of R-genes and they also have high tuber yield. These clones were planted in a randomized complete block design (RCBD) with 2-4 replicates at Oxapampa, located at 1810 masl in Pasco-Peru in the Eastern mountain ranges facing the Amazon. The trials were established at Oxapampa due to the high disease pressure of late blight in these areas from 1999 to 2005.
"
  
  uri <- "doi:10.21223/P3/BOXOEZ"
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

