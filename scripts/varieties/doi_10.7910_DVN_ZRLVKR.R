# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
White sorghum hybrids at Shiraro 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 36 experimental white hybrids, a hybrid and an OPV check evaluated at Shiraro (Western Tigrai, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/ZRLVKR"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "plant_height;yield;drought_stress;pest_severity;disease_severity", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-14",
		carob_completion = 100,	
		carob_effort = 4
	)
	
	f <- ff[basename(ff) == "White sorghum hybrids at Shiraro 2014.xlsx"]
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
	  variety_type = "white hybrids",
	  plot_id = as.character(r$Plot),
	  flowering_days = r$DTF,
	  maturity_days = r$DTM,
	  yield_part = "grain",
	  plot_area = as.numeric(r$PlotArea),
	  disease_severity = as.character(r$DiseaseScore),
	  pest_severity = as.character(r$InsectScore),
	  drought_stress = as.character(r$DroughtScore),
	  plant_density = 10000 * r$StandAtHarv / r$PlotArea,
	  spike_density = 10000 * r$`Heads/Plot` / r$PlotArea,
	  crop = "sorghum"
	)
	
	d$trial_id <- "1"

	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$longitude <- 37.773
	d$latitude <-  14.396
	# uncertainty from GADM adm3 = "Tahtay Adiyabo",
	d$geo_uncertainty = 49096
	d$geo_source = "Google Maps"
	d$geo_from_source <- FALSE
	d$location[d$location == "SH"] <- "Shiraro"
	
	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- d$fertilizer_type <- NA
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}


