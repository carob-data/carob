# R script for "carob"


carob_script <- function(path) {
  
"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2022)"
  
	uri <- "hdl:11529/10548943"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		project="High Rainfall Wheat Yield Trial",		 
		publication=NA,
		data_organization = "CIMMYT",
		data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code",
		carob_contributor="Robert Hijmans",
		carob_date="2026-07-12",	 
		carob_effort = 0.1,
		carob_completion = 75		
	)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	carobiner::write_files(path, meta, d$wide, d$long)
}


	
	
