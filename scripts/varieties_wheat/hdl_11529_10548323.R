# R script for "carob"


carob_script <- function(path) {
  
"The International Bread Wheat Screening Nursery (IBWSN) is designed to rapidly assess a large number of advanced generation (F3-F7) lines of spring bread wheat under Mega-environment 1 (ME1) which represents diversity for a wide range of latitudes, climates, daylengths, fertility conditions, water management, and (most importantly) disease conditions. The distribution of these nurseries is deliberately biased toward the major spring wheat regions of the world where the diseases of wheat are of high incidence. It is distributed to 180 locations and contains 300-450 entries. (2005)"
  
	uri <- "hdl:11529/10548323"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		project="International Bread Wheat Screening Nursery",
		publication = NA,
		data_organization = "CIMMYT",
		carob_contributor="Mitchelle Njukuya",
		carob_effort = NA,
		carob_date="2024-06-04",	 
		data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code"
	)
	
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	carobiner::write_files(path, meta, d$wide, d$long)
}
