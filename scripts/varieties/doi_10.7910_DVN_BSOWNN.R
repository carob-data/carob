# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
Dual Purpose hybrids Kobo 2017

Data on agronomic traits of maturity, plant height, drought score, grain yield and plant aspect score collected for 16 experimental dual purpose hybrids evaluated against 4 OPV checks at Kobo (North Wello, Ethiopia) in 2017
"

	uri <- "doi:10.7910/DVN/BSOWNN"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;seed_weight;plant_height;flowering_days;maturity_days;drought_stress;bird_damage", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-16",
		carob_completion = 100,	
		carob_effort = 1
	)

	f <- ff[basename(ff) == "Dual Purpose hybrids Kobo 2017.csv"]
	r <- read.csv(f)

	d <- data.frame(
	  country = "Ethiopia",
	  adm1 = NA,
	  adm2 = "Semien Wello", # same as North Wollo
	  adm3 = r$Site,
	  plot_id = as.character(r$Plot),
	  planting_date = format(as.Date(r$Sown, format = "%d/%m/%Y"), "%Y-%m-%d"),
	  treatment = as.character(r$Genotype),
	  variety = as.character(r$Genotype),
	  variety_pedigree = as.character(r$Pedigree),
	  variety_type = "dual purpose hybrid",
	  plot_area = NA,
	  plant_height = r$PHTMean,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  yield = r$`YieldKgHa`,
	  drought_stress = as.character(r$DroughtScore),
	  bird_damage = as.character(r$BirdDamage),
	  seed_weight = r$X100GW*10,
	  crop = "sorghum"
	)

	d$variety_type[d$variety %in% c("Melkam", "NTJ2", "A2267-2", "2005MI5064")] <- "OPV"
	
	d$harvest_date <- "2017"
	d$trial_id <- "1"
	d$on_farm <- TRUE 
	d$is_survey <- FALSE 
	d$irrigated <- NA
	
	# coordinates and geo_uncertainty was computed  from adm3 = Kobo
	d$longitude <- 39.643
	d$latitude <- 12.1038
	geo_uncertainty = 36657
	geo_source = "GADM 4.1, adm3"
	d$geo_from_source <- FALSE
	
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d)
}


