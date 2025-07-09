# R script for "carob"

carob_script <- function(path) { 
  
"The objective of the Landscape Diagnostic Survey (LDS) for wheat is to bridge the existing data-gap around current production practices of wheat, and also to help in evidence-based planning. The LDS is designed in a way that data is collected from randomly selected farmers spread uniformly within a KVK (government extension system)  domain/district. Data has been collected from farmers largest wheat plot for winter season of 2018. Survey questionnaire captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit (ODK) tool on mobile phone or tablet. (2019-12-31)"

	uri <- "hdl:11529/10548507"
	group <- "survey"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		project="CSISA",
		publication= NA,
		data_organization = "CIMMYT;ICAR;IRRI;DRC;UNIQ;CU",
		data_type="survey", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans; Effie Ochieng'",
		carob_date="2024-01-22"
	)
  	
	f <- ff[basename(ff) == "CSISA_IND_LDS_Whe_2018_Data.csv"]
	r <- read.csv(f)
	
	
	do_LCAS <- carobiner::get_function("do_LCAS", path, group)
	d <- do_LCAS(r)
	
    carobiner::write_files(path, meta, d)
}

