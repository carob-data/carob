# R script for "carob"

carob_script <- function(path) {

"Stress Tolerant Maize for Africa for Improved Livelihoods"

	uri <- "doi:10.25502/y1kz-yf32/d"
	group <- "varieties_maize"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication= "doi:10.1371/journal.pone.0252506",
		project = NA,
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		carob_contributor= "Eduardo Garcia Bendito",
		carob_date="2024-05-07"
	)
	
	f <- ff[grep("phenotypic.data.of.inbred.lines.evaluated.under.low.n.conditions.in.2017", basename(ff), ignore.case=TRUE)]
	r <- read.csv(f)

	d <- data.frame(
		crop = "maize",
		trial_id = r$LOC,
		rep = r$REP,
		plot_length = 4, 
		season = "minor", # from pub
		planting_date = "2017-09-01", # from pub
		variety = r$ENTRY,
		yield = r$YIELD,
		plant_spacing = 75, # from pub
		row_spacing = 40, # from pub
		plant_density = 66666,
		anthesis_days = r$POLLEN,
		silking_days = r$DYSK,
		soil_N = ifelse(r$LOC == "FUMESUA", 0.11, 0.06)*10000 #from pub 
	)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$striga_trial <- FALSE
	d$borer_trial <- FALSE
	d$striga_infected <- FALSE

##### Location
	d$country <- "Ghana"
	d$location <- ifelse(r$LOC == "FUMESUA", "Kumasi", "Accra")
	d$site <- ifelse(r$LOC == "FUMESUA", "Crop Research Institute, Fumesua", "University of Ghana, WACCI Farm")
	d$elevation <- ifelse(r$LOC == "FUMESUA", 286, 97) # from pub
	d$latitude <- ifelse(r$LOC == "FUMESUA", 6.7175, 5.6601) # from pub
	d$longitude <- ifelse(r$LOC == "FUMESUA", -1.5326, -0.1919) # from pub
	d$geo_from_source <- FALSE

##### Fertilizers
	d$fertilizer_type <- "urea;KCl;TSP" # from pub
	d$N_fertilizer <- 30 # from pub
	d$N_splits <- as.integer(2)
	d$P_fertilizer <- 27.6 # Phosphorus (triple super phosphate, 46% P) applied at 60 kg ha-1
	d$K_fertilizer <- 36 # Potassium (muriate of potash, 60% K) applied at 60 kg ha-1 

	d$yield_part <- "grain"
	d$irrigated <- FALSE
	
	carobiner::write_files(path, meta, d)
}
