# R script for "carob"


carob_script <- function(path) {

"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2017)"

	uri <- "hdl:11529/10548226"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		project="High Rainfall Wheat Yield Trial",
		publication = "doi:10.1016/j.fcr.2020.107742",
		data_organization = "CIMMYT",
		carob_contributor="Andrew Sila",
		carob_effort = NA,
		carob_date="2023-05-03",
		data_type="on-station experiment",
		response_vars = "yield",
		treatment_vars = "variety_code"
	)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	
	i <- d$location == "Ceneb, Bed Heat"
	d$planting_date[i] <- NA
	d$harvest_date[i] <- NA
	
	carobiner::write_files(path, meta, d)
}
