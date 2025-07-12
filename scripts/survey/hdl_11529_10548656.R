# R script for "carob"

carob_script <- function(path) { 
  
"Landscape Diagnostic Survey (LDS) data contains current rice production practices of rice applied by 8,355 farmers in eight states of India. The objective of collecting this data is to bridge the existing data-gap and to generate data-based evidence that can help in evidence-based planning. The LDS is designed in a way that data is collected from randomly selected farmers spread uniformly within a KVK (government extension system) domain/district. Survey questionnaire captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on rice establishment, fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit (ODK) tool on mobile phone or tablet. (2021-12-01)"

	uri <- "hdl:11529/10548656"
	group <- "survey"

	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		project="CSISA",
		publication= NA,
		data_organization = "CIMMYT;ICAR;IRRI;DRC;CU",
		data_type="survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-27"
	)
	  	  
	f <- ff[basename(ff)=="CSISA_IND_LDS_Rice_2018_Data.csv"]
	r <- read.csv(f)
	r <- carobiner::change_names(r, c("X", "X.1"), c("O.largestPlotGPS.Latitude", "O.largestPlotGPS.Longitude"))


	do_LCAS <- carobiner::get_function("do_LCAS", path, group)
	d <- do_LCAS(r)
	
    carobiner::write_files(path, meta, d)
}

