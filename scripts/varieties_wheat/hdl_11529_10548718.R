# R script for "carob"


carob_script <- function(path) {
  
"The WYCYT international nurseries are the result of research conducted to raise the yield potential of spring wheat through the strategic crossing of physiological traits related to source and sink potential in wheat. These trials have been phenotyped in the major wheat-growing mega environments through the International Wheat Improvement Network (IWIN) and the Cereal System Initiative for South Asia (CSISA) network, which included a total of 136 environments (site-year combinations) in major spring wheat-growing countries such as Bangladesh, China, Egypt, India, Iran, Mexico, Nepal, and Pakistan. (2021)"
  
	uri <- "hdl:11529/10548718"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication= NA,
		project="Wheat Yield Collaboration Yield Trial",
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety_code",
		carob_contributor= "Robert Hijmans",
		carob_effort = NA,
		carob_date="2024-09-22"
	)
	
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)	
	
	carobiner::write_files(path, meta, d)
}

