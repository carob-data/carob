# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Advanced drought tolerant sorghum hybrids at Kobo 2018

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for advanced drought tolerant hybrids evaluated at Kobo (North Wollo, Ethiopia) in 2018
"

	uri <- "doi:10.7910/DVN/8NCQWH"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;seed_weight;plant_height;flowering_days;maturity_days;pest_severity;drought_stress;bird_damage",
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-09",
		carob_completion = 90,	
		carob_effort = 1
	)
	

	f <- ff[basename(ff) == "Advanced drought tolerant sorghum hybrids at Kobo 2018.xlsx"]

	r <- carobiner::read.excel(f)


	d <- data.frame(
	  country = "Ethiopia",
	  adm1 = NA,
	  adm2 = "North Wollo",
	  adm3 = "Kobo",
	  location = r$Site,
	  plot_id = as.character(r$Plot),
	  planting_date = r$Sown,
	  treatment = r$Genotype,
	  #variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  plot_area = NA,
	  plant_height = r$PHTMean,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  yield = r$`Yield Kg/Ha`,
	  seed_weight = r$`100GW` * 10,
	  pest_severity = as.character(r$InsectScore),
	  drought_stress = as.character(r$DroughtScore),
	  bird_damage = as.character(r$BirdDamage),
	  crop = "sorghum"
	)
	
	d$planting_date <- as.character(as.Date("2018-07-17"))
	d$harvest_date <- "2018"
	d$trial_id <- "1"
	d$on_farm <- TRUE 
	d$is_survey <- FALSE 
	d$irrigated <- FALSE
	
 
	d$longitude <- 39.643
	d$latitude <- 12.1038
	geo_uncertainty = 36657
	geo_source = "GADM 4.1, adm3"
	d$geo_from_source <- TRUE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
	
  d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	

	carobiner::write_files(path, meta, d)
}


