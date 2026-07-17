# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
White sorghum hybrids at Kobo 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 35 experimental white hybrids, a hybrid and an OPV check evaluated at Kobo (North Wello, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/LQPKVI"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;seed_weight;plant_height;flowering_days;maturity_days;pest_severity;drought_stress;bird_damage;sl;rl", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-14",
		carob_completion = 90,	
		carob_effort = 1
	)

	f <- ff[basename(ff) == "White sorghum hybrids at Kobo 2014.xlsx"]
	r <- carobiner::read.excel(f, sheet="Sheet1")

	d <- data.frame(
	  country = "Ethiopia" ,
	  adm1 = NA,
	  adm2 = NA,
	  adm3 = "Kobo",
	  location = r$Site,
	  plot_id = as.character(r$Plot),
	  planting_date = as.character(r$Sown),
	  harvest_date = as.character(r$DateHarvest),
	  treatment = r$Genotype,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  variety_type = "white hybrid",
	  rep = as.integer(r$Replicate),
	  plot_area = r$PlotArea,
	  plant_height = r$PHTMean,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  emergence_date = r$DTE,
	  yield = r$`YieldKg/Ha`,
	  pest_severity = as.character(r$InsectScore),
	  drought_stress = as.character(r$DroughtScore),
	  bird_damage = as.character(r$BirdDamage),
	  disease_severity = as.character(r$DiseaseScore),
	  seed_weight = r$`1000GW`,
	  sl = r$StemLodging,
	  rl = r$RootLodging,
	  crop = "sorghum"
	)

	d$variety_type[d$variety == "ESH-3"] <- "hybrid"
	d$variety_type[d$variety == "Dekeba"] <- "OPV"
	
	d$trial_id <- r$Type
	d$on_farm <- TRUE 
	d$is_survey <- FALSE 
	d$irrigated <- NA
	
	d$longitude <- 39.643
	d$latitude <- 12.1038
	d$geo_uncertainty = 36657
	d$geo_source = "GADM 4.1, adm3"
	d$geo_from_source <- TRUE

	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
		
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d)
}

