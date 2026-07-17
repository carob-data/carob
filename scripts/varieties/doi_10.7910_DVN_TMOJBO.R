# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Drought hybrids Shiraro 2017

Data on agronomic traits of maturity, plant height, drought score, grain yield and plant aspect score collected for experimental drought tolerant hybrids evaluated at Shiraro (Western Tigray, Ethiopia) in 2017
"

	uri <- "doi:10.7910/DVN/TMOJBO"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;seed_weight;plant_height;flowering_days;maturity_days;drought_stress",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-17",
		carob_completion = 90,
		carob_effort = 1
	)

	f <- ff[basename(ff) == "Drought hybrids Shiraro 2017.csv"]
	r <- read.csv(f)
	r <- r[r$Genotype != "", ]

	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "Western Tigray",
	  adm3 = "Tahtay Adiyabo",
	  location = r$Site,
	  plot_id = as.character(r$Plot),
	  treatment = as.character(r$Genotype),
	  variety = as.character(r$Genotype),
	  variety_pedigree = as.character(r$Pedigree),
	  variety_type = "drought tolerant hybrid",
	  rep = as.integer(r$Replicate),
	  plant_height = r$PHT,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  yield = r$Yield_KgHa,
	  seed_weight = r$X1000GW,
	  drought_stress = as.character(r$DroughtScore),
	  bird_damage = as.character(r$BirdDamage),
	  crop = "sorghum"
	)

	## checks: two hybrid checks (ESH-1, ESH-4) and two OPV checks (Melkam, 2005MI5064)
	d$variety_type[d$variety %in% c("ESH-1", "ESH-4")] <- "hybrid"
	d$variety_type[d$variety %in% c("Melkam", "2005MI5064")] <- "OPV"

	d$location <- trimws(d$location)
	d$location[d$location == "Sheraro"] <- "Shiraro"
	d$planting_date <- "2017"
	d$harvest_date <- "2017"
	d$trial_id <- "1"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$longitude <- 37.773
	d$latitude <- 14.396
	d$geo_uncertainty <- 49096
	d$geo_source <- "GADM 4.1, adm3"
	d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	d$variety_pedigree[d$variety_pedigree == ""] <- NA
	carobiner::write_files(path, meta, d)
}
