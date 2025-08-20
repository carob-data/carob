# R script for "carob"

carob_script <- function(path) {

"
HOPE Project -  Farmer Field Trials at Ahmednagar for Sorghum for the season 2010-11

Farmer field level Trail data has been collected under HOPE Project -  Farmer Field Trials at Ahmednagar- Sorghum for the season 2010-11

Grain Yield and Fodder Yield have been recorded in different villages
"

	uri <- "doi:10.21421/D2/2XFDMI"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "ICRISAT",
		publication = NA,
		project = "HOPE",
		carob_date = "2025-08-20",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Gacheri Nturibi",
		completion = 100,	
		notes = NA
	)
	
	f <- ff[basename(ff) == "Ahmednagar FLD data 2010-11.xlsx"]
	
	r <- carobiner::read.excel(f, sheet="Observation")

	d <- data.frame(
	  trial_id = r$Farmer_name,
	  variety = r$Variety,
	  yield = as.numeric(r$Grain_Yield) * 2.47105,  # kg/acre to kg/ha
	  country = "India",
	  crop = "sorghum",
	  yield_part = "grain",
	  planting_date = "2010"
	)
	
	d$yield_moisture <- as.numeric(NA)

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$longitude <- 74.858  # coordinates from carobiner::geocode function
	d$latitude <- 19.1628 
	d$geo_from_source <- FALSE
	d$adm1 <- "Maharashtra"
	d$location <- "Ahmednagar"
	
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}
