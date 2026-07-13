# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Advanced drought tolerant sorghum hybrids at Mieso 2020

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for advanced drought tolerant hybrids evaluated at Mieso (Western Hararghe, Ethiopia) in 2020
"

	uri <- "doi:10.7910/DVN/XE1YPM"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;plant_height",
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-13",
		carob_completion = 80,	
		carob_effort = 5
	)
	
	f <- ff[basename(ff) == "Advanced drought tolerant sorghum hybrids at Mieso 2020.xlsx"]
	r <- carobiner::read.excel(f)
	
	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "Hararghe",
	  location = r$Site,
	  treatment = r$Genotype,
	  rep = as.integer(r$Replicate),
	  plant_height = r$PHTMean,
	  yield = r$`Yield Kg/Ha`,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  plot_id = as.character(r$Plot),
	  flowering_days = r$DTF,
	  maturity_days = r$DTM,
	  yield_part = "grain",
	  crop = "sorghum"
	)
	
	d$trial_id <- "1"	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$longitude = 40.5638
	d$latitude = 9.1779
	d$geo_uncertainty = 51603
	d$geo_source = "GADM 4.1, adm3"
	d$geo_from_source <- FALSE #!
	d$location[d$location == "MS"] <- "Mieso"

	d$planting_date <- as.character(NA)
	d$harvest_date <- as.character(NA)
	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- d$fertilizer_type <- NA

   d$yield_part <- "grain"
   d$yield_moisture <- as.numeric(NA)
   d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}


