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
	group <- "agronomy"
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
		carob_date = "2026-07-01",
		carob_completion = 0,	
		carob_effort = 5
	)
	
	f1 <- ff[basename(ff) == "Red sorghum hybrids at Mieso 2014.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Sheet1")
	
	d <- data.frame(
	  country = "Ethiopia",
	  adm1 = NA,
	  adm2 = "West Hararghe",
	  adm3 = r1a$Site,
	  plot_id = as.character(r1a$Plot),
	  planting_date = r1a$Sown,
	  harvest_date = r1a$DateHarvest,
	  plot_area = as.numeric(r1a$PlotArea),
	  treatment = r1a$Genotype,
	  variety_pedigree = r1a$Pedigree,
	  plant_height = r1a$PHTMean,
	  maturity_days = r1a$DTM,
	  flowering_days = r1a$DTF,
	  yield = r1a$`YieldKg/Ha`,
	  pest_severity = as.character(r1a$InsectScore),
	  disease_severity = as.character(r1a$DiseaseScore),
	  drought_stress = as.character(r1a$DroughtScore),
	  bird_damage = as.character(r1a$BirdDamage),
	  crop = "sorghum"
	)
	
	d$planting_date <- as.character(as.Date("2014-07-20"))
	d$harvest_date <- as.character(as.Date("2014-12-18"))
	d$trial_id <- paste(d$adm3,d$planting_date,sep = "_")
	d$on_farm <- TRUE 
	d$is_survey <- FALSE 
	d$irrigated <- FALSE

	## see carobiner::geocode server not available at the moment
	d$longitude <- 40.750
	d$latitude <- 9.233
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	names(d)[names(d) == "fertlizer_type"] <- "fertilizer_type"
	d$fertilizer_type <- NA
	
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d)
}


