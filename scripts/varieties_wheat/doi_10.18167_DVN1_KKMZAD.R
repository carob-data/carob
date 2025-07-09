# R script for "carob"


carob_script <- function(path) {

"Durum wheat dataset collected on HAO-DEMETER research station in Thessaloniki, Greece from 2018 to 2020. The experiment was set up to study 2 agronomic factors: 4 cultivars, 2 levels of fertilization. The dataset includes phenology, morphology, leaf area index, biomass, yield components."

	uri <- "doi:10.18167/DVN1/KKMZAD"
	group <- "varieties_wheat"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "DIMITRA;CIRAD",
		publication= NA,
		project=NA,
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		carob_contributor= "Hope Mazungunye",
		carob_date="2024-04-09"
	)
	
	f <- ff[basename(ff) == "Data.csv"]
	r <- read.csv(f)
				
	d <- data.frame(
		planting_date=as.character(r$year), 
		plant_density=r$V1 * 10000, 
		spike_density=r$V2 * 10000,
		heading_days=r$V4, 
		plant_height =r$V6, 
		dmy_total= r$V8*1000, 
		yield=r$V10*1000, 
		seed_weight= r$V11, 
		variety= trimws(r$cultivar),
		LAI=r$V13, # at anthesis
		rep=r$block,
		treatment = trimws(r$treatment) 
	)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$country <- "Greece"
	d$location <-  "Thessaloniki"
	d$site <- "HAO-DEMETER research station"
	
	d$longitude <- 22.998
	d$latitude <- 40.538
	d$geo_from_source <- FALSE
	
	d$crop <- "durum wheat"
	d$yield_part <- "grain"
	d$trial_id <- "1"

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(meta, d, path=path)
}



