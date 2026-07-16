# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
White sorghum hybrids at Mieso 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 35 experimental red hybrids, a hybrid and an OPV check evaluated at Mieso (Western Hararghe, Ethiopia) in 2014
"
	uri <- "doi:10.7910/DVN/SCFBFD"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height;disease_severity;pest_severity;drought_stress",
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-13",
		carob_completion = 70,	
		carob_effort = 6
	)
	
	f <- ff[basename(ff) == "White sorghum hybrids at Mieso 2014.xlsx"]
	r <- carobiner::read.excel(f, sheet="Sheet1")

	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "Hararghe",
	  location = r$Site,
	  treatment = r$Genotype,
	  rep = as.integer(r$Replicate),
	  planting_date = as.character(as.Date(r$Sown)),
	  harvest_date = as.character(as.Date(r$DateHarvest)),
	  plant_height = r$PHTMean,
	  yield = r$`YieldKg/Ha`,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  variety_type = "red hybrids",
	  plot_id = as.character(r$Plot),
	  flowering_days = r$DTF,
	  maturity_days = r$DTM,
	  yield_part = "grain",
	  plot_area = as.numeric(r$PlotArea),
	  disease_severity = as.character(r$DiseaseScore),
	  pest_severity = as.character(r$InsectScore),
	  drought_stress = as.character(r$DroughtScore),
	  rl = r$RootLodging,
	  sl = r$StemLodging,
	  bird_damage = as.character(r$BirdDamage),
	  plant_density = 10000 * r$StandAtHarv / r$PlotArea,
	  spike_density = 10000 * r$`Heads/Plot` / r$PlotArea,
	  crop = "sorghum"
	)

	d$variety_type[d$variety == "ESH-3"] <- "hybrid"
	d$variety_type[d$variety == "Dekeba"] <- "OPV"
	
	d$trial_id <- "1"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$longitude <- 40.5638
	d$latitude <- 9.1779
	geo_uncertainty = 51603
	geo_source = "GADM 4.1, adm3"
	d$geo_from_source <- FALSE
	d$location[d$location == "MS"] <- "Mieso"
	
	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- d$fertilizer_type <- NA
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	
	carobiner::write_files(path, meta, d)
}


