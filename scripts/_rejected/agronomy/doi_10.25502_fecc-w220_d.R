# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Treatment variables not clear
# The script is put in rejected folder because it is not a field experiment, its an aeroponics system.
# added vine_length, plant_vigor, node_per_plant as variables 
# each plantlet had a unique ID, so added plant_id
# The dataset did not provide the exact location

carob_script <- function(path) {

"
Growth parameters of yam plantlets in aeroponics system

Seed yam production using high ratio propagation techniques.
"

	uri <- "doi:10.25502/fecc-w220/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "none",
		response_vars = "vine_length;plant_vigor;node_per_plant", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-08-21",
		carob_completion = 90,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "aerogrowthdataset-b.csv"]
	#f2 <- ff[basename(ff) == "data_dictionary_aerogrowth_dataset_b.csv"]

	r1 <- read.csv(f1)
	#r2 <- read.csv(f2)


	d <- data.frame(
	  country = "Nigeria",
		year = r1$Year,
		trial_id = as.character(r1$SourceID),
		plot_id = as.character(r1$Hole_No),
		plant_vigor = as.numeric(r1$PL_Vigor),
		vine_length = as.numeric(r1$VineLength),
		node_per_plant = as.numeric(r1$nnodes),
		crop_age = as.numeric(r1$Age),   #the plant age is in weeks
		crop = "yam"
	)

	
	d$plant_id <- as.character(r1$ID)
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	

	d$longitude <- NA
	d$latitude <-  NA
	d$geo_from_source <- FALSE


	d$planting_date <- as.character(as.Date(NA))
	d$harvest_date  <- as.character(as.Date(NA))


   d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
   d$fertilizer_type <- NA


	d$yield <- NA
	d$yield_part <- NA
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	d$yield_part <- "tubers"
	
	
	carobiner::write_files(path, meta, d)
}
