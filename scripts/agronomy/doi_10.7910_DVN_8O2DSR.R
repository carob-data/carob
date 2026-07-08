# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them
#geodata not available at the moment
# netplot area was very small 

carob_script <- function(path) {

"
Red sorghum hybrids at Mieso 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 36 experimental red hybrids, a hybrid and an OPV check evaluated at Mieso (Western Hararghe, Ethiopia) in 2014
"
  
	uri <- "doi:10.7910/DVN/8O2DSR"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;pest_severity;disease_severity;drought_stress;bird_damage;plant_height;maturity_days",
		carob_contributor = "Kwenda Illiana",
		carob_date = "2026-07-08",
		carob_completion = 90,	
		carob_effort = 5
	)
	
	f <- ff[basename(ff) == "Red sorghum hybrids at Mieso 2014.xlsx"]

	r <- carobiner::read.excel(f, sheet="Sheet1")
	
	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "West Hararghe",
	  adm3 = "Mieso",
	  location = r$Site,
	  plot_id = as.character(r$Plot),
	  planting_date = r$Sown,
	  harvest_date = r$DateHarvest,
	  plot_area = as.numeric(r$PlotArea),
	  treatment = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  plant_height = r$PHTMean,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  yield = r$`YieldKg/Ha`,
	  pest_severity = as.character(r$InsectScore),
	  disease_severity = as.character(r$DiseaseScore),
	  drought_stress = as.character(r$DroughtScore),
	  bird_damage = as.character(r$BirdDamage),
	  crop = "sorghum"
	)
	
	d$planting_date <- as.character(as.Date("2014-07-20"))
	d$harvest_date <- as.character(as.Date("2014-12-18"))
	d$trial_id <- paste(d$location,d$planting_date,sep = "_")
	d$on_farm <- TRUE 
	d$is_survey <- FALSE 
	d$irrigated <- FALSE

	d$longitude = 40.5638
	d$latitude = 9.1779
	d$geo_uncertainty 51603
	d$geo_source = "GADM 4.1, adm3"
	
	d$geo_from_source <- FALSE #!

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
	
	d$yield_part <- "grain"
	d$yield_isfresh <- NA
	d$yield_moisture <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}


