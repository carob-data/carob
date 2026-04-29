# R script for "carob"

carob_script <- function(path) {
  
"In anticipation of the effects of global warming on potato cultivation in both tropical and subtropical environments. Since 2004, efforts have turned to the development of a new group in Population B with improved adaptation to warm environments, resistance to late blight and virus, mid-season maturity (90 day growing period under short day length conditions), adaptation to mid elevations, low glycoalkaloids content, along with economically important traits such as high tuber yield, quality for table and industry. And so group LBHT of population B was developed, denominated LBHT because of its late blight and heat tolerance. All trials were conducted in randomized complete block design (RCBD) with 2-4 replicates or in simple lattice design at Comas, located at 2400 to 2788 masl in Junin situated in the Central mountain ranges in Peru The trials were established at Comas due to high disease pressure of late blight, disease endemic, highly favorable for disease development and severity during the rainy periods in these areas from 2006 to 2007."
  
  uri <- "doi:10.21223/P3/OOQ73N"
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
  
  f <- ff[grep("-1COM", basename(ff))]
  f <- f[grep("^0._|_Data_dictionary", basename(f), invert=TRUE)]
  d <- lapply(f, process)
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
}

