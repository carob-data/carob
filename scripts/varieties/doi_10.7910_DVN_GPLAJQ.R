# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Advanced drought tolerant sorghum hybrids at Kobo 2020

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for advanced drought tolerant hybrids evaluated at Kobo (North Wollo, Ethiopia) in 2020
"

	uri <- "doi:10.7910/DVN/GPLAJQ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;bird_damage;plant_height",
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-09",
		carob_completion = 70,	
		carob_effort = 4
	)
	
	f <- ff[basename(ff) == "Advanced drought tolerant sorghum hybrids at Kobo 2020.xlsx"]

	r <- carobiner::read.excel(f)
	d <- data.frame(
		country = "Ethiopia",
		adm2 = "North Wollo",
		location = r$Site,
		treatment = r$Genotype,
		rep = as.integer(r$Replicate),
		yield = r$`Yield Kg/Ha`,
		variety_pedigree = r$Pedigree,
		plot_id = as.character(r$Plot),
		plant_height = r$PHTMean,
		flowering_days = r$DTF,
		maturity_days = r$DTM,
		bird_damage = as.character(r$BirdDamage),
		seed_weight = as.numeric(r$`1000GW`),
		crop = "sorghum"
	)

	d$trial_id <- "1"	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$longitude <- 39.643
	d$latitude <- 12.1038
	d$geo_from_source <- FALSE

	d$planting_date <- as.character(NA)
	d$harvest_date <- as.character(NA)
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	
	carobiner::write_files(path, meta, d)
}


