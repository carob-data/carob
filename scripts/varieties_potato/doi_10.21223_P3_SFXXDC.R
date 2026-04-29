# R script for "carob"

carob_script <- function(path) {
  
"
B3C0 is the initial cycle of recombination of group B3- Population B. This population has been derived from population A. R genes have been eliminated from this group and horizontal resistance has been retained. Currently, these clones are being used on the variety selection in developing countries. This group of clones  were planted under a randomized complete block design (RCBD) with 2 replicates in Oxapampa (1810 masl  Pasco-Peru)  the Eastern mountain ranges facing the Amazon. Oxampapa is similar to the target environments due to the high disease pressure of late blight present in the area and used to screen selected potato genotypes for resistance to this disease.
"
  
  uri <- "doi:10.21223/P3/SFXXDC"
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
  f <- f[grep("^0._|_Data_dictionary", basename(f), invert=TRUE)]
  d <- lapply(f, process, addvars="TTWP")
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
}

