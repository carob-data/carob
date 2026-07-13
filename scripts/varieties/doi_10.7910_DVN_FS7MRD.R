# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
White sorghum hybrids at Humera 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 35 experimental white hybrids, a hybrid and an OPV check evaluated at Humera (Western Tigrai, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/FS7MRD"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;seed_weight;plant_height;emergence_days;severity_scale;rl;sl;flowering_days;maturity_days;pest_severity;drought_stress;bird_damage", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-13",
		carob_completion = 90,	
		carob_effort = 1
	)
	

	f <- ff[basename(ff) == "White sorghum hybrids at Humera 2014.xlsx"]
	r <- carobiner::read.excel(f, sheet="Sheet1")
	
	d <- data.frame(
	  trial_id = "1",
	  country = "Ethiopia",
	  adm1 = "Western Tigrai",
	  adm2 = NA,
	  adm3 = "Kafta Humera",
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
	  pest_severity = r$InsectScore,
	  drought_stress = r$DroughtScore,
	  bird_damage = as.character(r$BirdDamage),
	  severity_scale = r$DiseaseScore,
	  rl = r$RootLodging,
	  sl = r$StemLodging,
	  crop = "sorghum"
	)
		
	d$trial_id <- "1"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$longitude <- 36.9402
	d$latitude <- 14.0476
	geo_uncertainty <- 65821
	geo_source = "GADM 4.1, adm3"
	d$geo_from_source <- TRUE

	d$on_farm <- TRUE 
	d$is_survey <- FALSE
	d$irrigated <- NA
		
	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
		
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}


