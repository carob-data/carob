# R script for "carob"


carob_script <- function(path) {
  
"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Screening Nursery (HRWSN) contains spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall areas (Mega-environment 2). (2015)"

  uri <- "hdl:11529/10548267"
  group <- "varieties_wheat"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
    data_organization = "CIMMYT",
    publication= NA,
    project="High Rainfall Wheat Screening Nursery",
    data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety_code",
    carob_contributor= "Fredy Chimire",
    carob_effort = NA,
    carob_date="2024-04-29"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)	

    carobiner::write_files(path, meta, d$wide, d$long)
}


