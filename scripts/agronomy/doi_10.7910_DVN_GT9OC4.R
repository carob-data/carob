# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Red sorghum hybrids at Shiraro 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 36 experimental red hybrids, a hybrid and an OPV check evaluated at Sheraro (Western Tigrai, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/GT9OC4"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = "NA",
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;disease_severity;pest_severity;drought_stress", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-01",
		carob_completion = 0,	
		carob_effort = 5
	)

	f <- ff[basename(ff) == "Red sorghum hybrids at Shiraro 2014.xlsx"]

	r <- carobiner::read.excel(f, sheet="Sheet1")
	d <- data.frame(
	  country = "Ethiopia",
		adm1 = "Tigray",
		adm2 = "Western Tigray",
		location = r$Site,
		treatment = r$Genotype,
		rep = as.integer(r$Replicate),
		planting_date = as.character(as.Date(r$Sown)),
		harvest_date = as.character(as.Date(r$DateHarvest)),
		plant_height = r$PHTMean,
		yield = r$`YieldKg/Ha`,
		variety = r$Genotype,
		variety_pedigree = r$Pedigree,
		plot_id = as.character(r$Plot),
		flowering_days = r$DTF,
		maturity_days = r$DTM,
		yield_part = "grain",
		plot_area = as.numeric(r$PlotArea),
		disease_severity = as.character(r$DiseaseScore),
		pest_severity = as.character(r$InsectScore),
		drought_stress = as.character(r$DroughtScore),
		drought_stress = as.character(r$DroughtScore),
		plant_density = 10000 * r$StandAtHarv / r$PlotArea,
		spike_density = 10000 * r$`Heads/Plot` / r$PlotArea,
		crop = "sorghum"
	)
	
	d$trial_id <- "1"
	
	d$on_farm <- TRUE # how do you know?
	d$is_survey <- FALSE
	d$irrigated <- FALSE # how do you know?
	
	d$longitude <-  37.555
	d$latitude <-  14.240
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE  # how do you know?

	carobiner::write_files(path, meta, d)
}


