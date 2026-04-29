# R script for "carob"


carob_script <- function(path) {
  
"In anticipation of the effects of global warming on potato cultivation in both tropical and subtropical environments. Since 2004, efforts have turned to the development of a new group in Population B with improved adaptation to warm environments, resistance to late blight and virus, mid-season maturity (90 day growing period under short day length conditions), adaptation to mid elevations, low glycoalkaloids content, along with economically important traits such as high tuber yield, quality for table and industry. And so group LBHT of population B was developed, denominated LBHT because of its late blight and heat tolerance. All trials were conducted in randomized complete block design (RCBD) with 3-4 replicates or in simple lattice design, using Désirée as a heat tolerant control and Amarilis as a non-tolerant to heat control at San Ramon, located at 828 masl in Junin-Peru,  a humid environment in mid elevation tropics."
  
  uri <- "doi:10.21223/P3/IO6NAV"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
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
  
  f <- ff[grep("_CIPSRM", basename(ff))]
  f <- f[grep("^0._|_Data_dictionary", basename(f), invert=TRUE)]
  d <- lapply(f, process)
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
 
}


