# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path){
  
"
Replication Data for: MasAgro maize seed evaluation network yield data, 2011-2019

Every year, the MasAgro maize component of the Sustainable Modernization of Traditional Agriculture project (MasAgro) established a seed evaluation network in the three mega-environments of maize production in Mexico (Subtropic, Tropic and Highlands) to evaluate all materials developed by CIMMYT breeders for MasAgro, and assess their performance vis-à-vis public and private materials under development or readily available in the market. This dataset contains grain yield (GY) information (N=897) of subtropical, tropical and highlands' white (n=582) and yellow (n=315) hybrids evaluated from 2011 to 2019. Yield data were previously processed and analyzed by CIMMYT's Subtropical, Tropical and Highlands maize breeding programmes. Open-pollinated varieties (OPVs) were excluded to compare GY of only hybrids. Hybrids were classified into five categories based on the entity responsible for their development or the proprietary holder. These categories are: CIMMYT MasAgro (hybrids developed by CIMMYT for MasAgro, Public (hybrids developed by public NARS), Private national (hybrids developed by national companies using proprietary germplasm); and Multinational (hybrids developed by multinational companies). The dataset is used to compare GY of MasAgro, public, private national and multinational hybrids across all years (2011-2019), for each colour – yellow and white maize – and mega-environment – subtropic, tropics and highlands.
"

  
  uri <- "hdl:11529/10549157"
  group <- "varieties_maize"
  ff  <- carobiner::get_data(uri, path, group)
  
  ## metadata 
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "MasAgro",
		data_type = "experiment",
		treatment_vars = "variety_type",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-08-05",
		notes =NA, 
		design = NA
  )
  
  f <- ff[basename(ff) == "MasAgro_maize_seed_evaluation_network_2011-2019 yields.xlsx"]
  r <- carobiner::read.excel(f, sheet="Data")
  
  d <- data.frame(
    locality=r$mega_env,
	country = "Mexico",
	crop="maize",
	yield_part="grain",
    variety_code=r$var_code,
    planting_date=r$yr,
    yield=r$GY*1000,
	yield_moisture = 12.5
  )
  
  
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- FALSE

  d$trial_id <- paste0(d$locality, "_", d$year)
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)
  
  carobiner::write_files(path, meta, d)
}

