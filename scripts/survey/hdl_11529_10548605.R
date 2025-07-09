# R script for "carob"

## ISSUES


carob_script <- function(path) { 
  
"
Landscape Diagnostic Survey (LDS) for rice contains farmer's data on current production practices they applied for cultivating rice during 2017 monsoon season. The dataset contains 6857 farmers’ information captured from Bihar, Uttar Pradesh and Odisha states from eastern part of India. The objective of collecting this data is to bridge the existing data-gap and to generate data-based evidence that can help in evidence-based planning. The LDS is designed in a way that data is collected from randomly selected farmers spread uniformly within a KVK (government extension system) domain/district. Survey questionnaire captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on rice establishment, fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit (ODK) tool on mobile phone or tablet. (2021-08-05)"

	uri <- "hdl:11529/10548605"
	group <- "survey"

	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		project="CSISA",
		publication= NA,
		data_organization = "CIMMYT;CIMMYT;IRRI;ICAR;DRC;UNIQ;CU",
		data_type="survey", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-27"
	)
  
	f <- ff[basename(ff)=="CSISA_IND_LDS_Rice_2017_Data.csv"]
	r <- read.csv(f)


	do_LCAS <- carobiner::get_function("do_LCAS", path, group)
	d <- do_LCAS(r)

    carobiner::write_files(path, meta, d)
}

