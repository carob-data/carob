# R script for "carob"


carob_script <- function(path) {
  
"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2003)"

	uri <- "hdl:11529/10548246"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		project="High Temperature Wheat Yield Trial",
		publication = NA,
		data_organization = "CIMMYT",
		carob_contributor="Fredy Chimire",
 		data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code",
		carob_effort = NA,
		carob_date="2023-10-23"
    
	)
   
  	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
  
	carobiner::write_files(path, meta, d$wide, d$long)
}
