# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one asian country.
"

	uri <- "doi:10.25502/20180830/1552/MA"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi.org/10.1016/j.jenvman.2017.06.058",
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		completion = 90,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-04-09",
		notes = NA,
		design = NA
	)
	
	f <- ff[basename(ff) == "gambia_international_maize_trials_regular2016.csv"]
	r <- read.csv(f)
	
	d <- data.frame(
	treatment=trimws(r$ENTRY),
	variety=trimws(r$ENTRY),
	country="Gambia",
	location=r$LOCATION,
	longitude=r$LONGITUDE,
	latitude=r$LATITUDE,
	planting_date=as.character(r$YEAR),
	yield=r$YIELD*1000,
	silking_days=r$DY_SK,
	anthesis_days=r$ANTHESIS,
	plant_height=r$PL_HT,
	ear_height=r$E_HT,
	asi=r$ASI,
	dmy_total=r$P_HARV)
	
	d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- as.numeric(NA)
  d$crop <- "maize"
	d$yield_part <- "grain"
	d$yield_moisture <-as.numeric(NA)


	carobiner::write_files(path, meta, d)
}

