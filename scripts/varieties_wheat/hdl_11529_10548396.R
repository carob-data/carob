# R script for "carob"


carob_script <- function(path) {

"Within the framework of SATYN, two types of nurseries are produced: SATYN series with odd numbers are lines for drought-stressed areas, and SATYN series with even numbers are lines for heat stress conditions. These nurseries have been phenotyped in the major wheat-growing mega environments through the International Wheat Improvement Network (IWIN) and the Cereal System Initiative for South Asia (CSISA) network, which included a total of 136 environments (site-year combinations) in major spring wheat-growing countries such as Bangladesh, China, Egypt, India, Iran, Mexico, Nepal, and Pakistan. (2017)"

  uri <- "hdl:11529/10548396"
  group <- "varieties_wheat"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
    data_organization = "CIMMYT",
    publication= NA,
    project="Stress Adapted Trait Yield Nurseries",
    data_type= "experiment",
    response_vars = "yield",
    treatment_vars = "variety_code",
    carob_contributor= "Blessing Dzuda",
    carob_effort = NA,
    carob_date="2024-06-20"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)	
  
  d$previous_crop <- gsub("rice-wheat","rice",d$previous_crop)

  
    carobiner::write_files(path, meta, d$wide, d$long)
}
