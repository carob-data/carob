# R script for "carob"

carob_script <- function(path) {
  
"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Screening Nursery (HRWSN) contains spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall areas (Mega-environment 2). (2017)"
  
    
	uri <- "hdl:11529/10548251"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		project="High Rainfall Wheat Screening Nursery",		 
		publication=NA,
		data_organization = "CIMMYT",
		carob_contributor="Robert Hijmans",
		carob_date="2024-07-14",
		data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code"
	)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	
	carobiner::write_files(path, meta, d)
}

	
	
