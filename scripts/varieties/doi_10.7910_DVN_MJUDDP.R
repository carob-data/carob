# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Stay-green hybrids  Mieso 2015

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for 60 experimental stay-green hybrids evaluated at Mieso (Western Hararghe, Ethiopia) in 2015
"

	uri <- "doi:10.7910/DVN/MJUDDP"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "yield;flowering_days;plant_height;maturity_days;spike_density;plant_density",
		response_vars = "variety", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-16",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f <- ff[basename(ff) == "Stay-green hybrids  Mieso 2015.xlsx"]

	r <- carobiner::read.excel(f)
	

	d <- data.frame(
	  country = "Ethiopia",
	  adm2 = "West Hararghe",
	  adm3 = r$Site,
	  plot_id = as.character(r$Plot),
	  planting_date = format(as.Date(r$Sown, format = "%d/%m/%Y"), "%Y-%m-%d"),
	  harvest_date = as.character(r$DateHarvest),
	  plot_area = as.numeric(r$PlotArea),
	  treatment = r$Genotype,
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  rep = as.integer(r$Replicate),
	  plant_height = r$PHTMean,
	  maturity_days = r$DTM,
	  flowering_days = r$DTF,
	  yield = r$`YieldKg/Ha`,
	  plant_density = 10000 * r$StandAtHarv / r$PlotArea,
	  spike_density = 10000 * r$`Heads/Plot` / r$PlotArea,
	  crop = "sorghum"
	)


	d$trial_id <- r$Type
	d$on_farm <- TRUE 
	d$is_survey <- FALSE 
	d$irrigated <- NA
	
	### coordinates and geo_uncertainty were obtained from adm3 = Mieso
	d$longitude <- 40.5638
	d$latitude <- 9.1779
	d$geo_uncertainty <- 51603 
	d$geo_source <- "GADM 4.1, adm3"
	
	d$geo_from_source <- FALSE #!
	
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
	
	d$yield_part <- "grain"
	d$yield_isfresh <- NA
	d$yield_moisture <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}

