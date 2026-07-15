# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Red sorghum hybrids at Shoarobit 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 36 experimental red hybrids, a hybrid and an OPV check evaluated at Shoarobit (North Shewa, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/VBLWFP"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;seed_weight;plant_height;flowering_days;maturity_days;pest_severity;drought_stress;bird_damage", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-14",
		carob_completion = 90,	
		carob_effort = 1
	)
	
	f <- ff[basename(ff) == "Red sorghum hybrids at Shoarobit 2014.xlsx"]
	r <- carobiner::read.excel(f, sheet="Sheet1")
	
	d <- data.frame(
	  country = "Ethiopia",
	  adm1 = NA,
	  adm2 = "North Shewa",
	  adm3 = "Kewet",
	  location = r$Site,
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
	  emergence_days = r$DTE,
	  yield = r$`YieldKg/Ha`,
	  seed_weight = r$`1000GW`,
	  pest_severity = as.character(r$InsectScore),
	  drought_stress = as.character(r$DroughtScore),
	  bird_damage = as.character(r$BirdDamage),
	  disease_severity = as.character(r$DiseaseScore),
	  sl = r$StemLodging,
	  rl = r$RootLodging,
	  crop = "sorghum"
	)

	d$trial_id <- r$Type
	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- NA
	
	##geo_uncertainity was obtained from adm3 = kewet
	d$longitude <- 39.900
	d$latitude <- 10.000
	dgeo_uncertainty <- 25628
	geo_source = "Google Maps"
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
		
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	carobiner::write_files(path, meta, d)
}

