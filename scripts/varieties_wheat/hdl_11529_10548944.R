# R script for "carob"


"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2022)"

carob_script <- function(path) {

	uri <- "hdl:11529/10548944"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication=NA,
		project="High Temperature Wheat Yield Trial",
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety_code",
		carob_contributor= "Mary Njogu",
		carob_effort = NA,
		carob_date="2024-03-15"
	)
  
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	carobiner::write_files(path, meta, d)
}  

