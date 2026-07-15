# R script for "carob"
# license: GPL (>=3)

## ISSUES
 #NA detected on yield

carob_script <- function(path) {

"
White sorghum hybrids at Shoarobit 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 35 experimental white hybrids, a hybrid and an OPV check evaluated at Shoarobit (North Shewa, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/BQFQ0Q"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height;seed_weight;maturity_days;spike_density;disease_severity;pest_severity;drought_stress", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-15",
		carob_completion = 100,	
		carob_effort = 5
	)
	

	f <- ff[basename(ff) == "White sorghum hybrids at Shoarobit 2014.xlsx"]

	r <- carobiner::read.excel(f, sheet="Sheet1")
	
	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "North Shewa",
	  adm3 = "Kewet",
	  location = r$Site,
	  treatment = r$Genotype,
	  rep = as.integer(r$Replicate),
	  planting_date = as.character(as.Date(r$Sown)),
	  harvest_date = as.character(as.Date(r$DateHarvest)),
	  plant_height = r$PHTMean,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  plot_id = as.character(r$Plot),
	  maturity_days = r$DTM,
	  disease_severity = as.character(r$DiseaseScore),
	  pest_severity = as.character(r$InsectScore),
	  drought_stress = as.character(r$DroughtScore),
	  bird_damage = as.character(r$BirdDamage),
	  rl = r$RootLodging,
	  sl = r$StemLodging,
	  yield = r$`YieldKg/Ha`,
	  seed_weight = as.numeric(r$`1000GW`),
	  plot_area = as.numeric(r$PlotArea),
	  spike_density = 10000 * r$`Heads/Plot` / r$PlotArea,
	  crop = "sorghum"
	)
	
	
	d$trial_id <- "1"
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	

	d$longitude <- 39.897
	d$latitude <- 9.995
	#geo_uncertainty from GADM adm3 = "Kewet",
	geo_uncertainty = 25628
	d$geo_source = "Google Maps"
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$fertilizer_type <- NA
	
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	

	carobiner::write_files(path, meta, d)
}


