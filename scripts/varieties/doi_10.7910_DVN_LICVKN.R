# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
White sorghum hybrids at Erer 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 35 experimental white hybrids, a hybrid and an OPV check evaluated at Erer (Eastern Hararghe, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/LICVKN"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height;flowering_days;maturity_days", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-15",
		carob_completion = 100,	
		carob_effort = 1
	)
	
	f <- ff[basename(ff) == "White sorghum hybrids at Erer 2014.xlsx"]
	r <- carobiner::read.excel(f, sheet="Sheet1")
	
	d <- data.frame(
	  country = "Ethiopia" ,
	  adm1 = NA,
	  # Erer is north of Misraq Hararghe. Assuming Erer is correct then adm2 = Sitti
	  adm2 = "Sitti",
	  adm3 = r$Site, 
	  plot_id = as.character(r$Plot),
	  planting_date = as.character(r$Sown),
	  harvest_date = as.character(r$DateHarvest),
	  treatment = r$Genotype,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  rep = as.integer(r$Replicate),
	  plot_area = r$PlotArea,
	  plant_height = r$PHTMean,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  yield = r$`YieldKg/Ha`,
	  crop = "sorghum"
	)
	
	d$trial_id <- r$Type
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	
	d$longitude <- 41.492 
	d$latitude <- 10.1957
### The dataset description has Erer in Eastern Hararghe but it is in Sitti
	# uncertainty was computed  from Erer 
	d$geo_uncertainty <- 91072
	d$geo_source = "GADM 4.1, adm3"
	d$geo_from_source <- FALSE # !!!!

	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
		
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	carobiner::write_files(path, meta, d)
}

