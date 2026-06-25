# R script for "carob"


carob_script <- function(path) {
  
"Summary results and individual trial results from the International Late Yellow Variety - ILYV, (Tropical Late Yellow Normal and QPM Synthetic Variety Trial - EVT13S) conducted in 2006"

	uri <- "hdl:11529/10551"
	group <- "varieties_maize"

	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication= NA,
		project=NA,
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety;longitude;latitude",
		carob_contributor= "Mitchelle Njukuya",
		carob_effort = NA,
		carob_date="2024-03-28"
	)

	get_data <- function(fname, id, country, longitude, latitude, elevation, location, adm1, planting, harvest) {

		f <- ff[basename(ff) == fname]
		r <- carobiner::read.excel(f) 
		r <- r[22:42, 2:42]
		r[r == "."] <- NA
		
		d <- data.frame( 
			trial_id = as.character(id),
			yield_part = "grain",
			variety = r$BreedersPedigree1,
			yield = as.numeric(r$GrainYieldTons_FieldWt)*1000,
			asi = as.numeric(r$ASI),
			plant_height=as.numeric(r$PlantHeightCm),
			ear_height = as.numeric(r$EarHeightCm),
			rlper = as.numeric(r$RootLodgingPer),
			slper = as.numeric(r$StemLodgingPer),
			husk = as.numeric(r$BadHuskCoverPer),
			e_rot = as.numeric(r$EarRotTotalPer),
			moist = as.numeric (r$GrainMoisturePer),
#			plant_density = as.numeric(r$PlantStand_NumPerPlot),
			e_asp = as.numeric(r$EarAspect1_5),
			p_asp = as.numeric(r$PlantAspect1_5),
			country = country,
			adm1 = adm1,
			location = location,
			longitude = longitude,
			latitude = latitude,
			elevation = elevation,
			planting_date = planting,
			harvest_date = harvest
		)
	}
	
	d0 <- get_data("06EVT13S9-1.xls", 1, "Mexico", -96.6667, 19.3333, 15, "Cotaxtla", "Veracruz", "2006-07-11", "2006-11-06")
	d1 <- get_data("06EVT13S10-1.xls", 2, "Mexico", -90.6833, 19.5167, NA, "Champoton", "Campeche", "2006-05-01", "2006-11-01")
	d2 <- get_data("06EVT13S12-1.xls", 3, "India", 77.2667, 15.6333, 415, "Adoni Mandal", NA, "2006-07-11", "2006-11-10")
	d3 <- get_data("06EVT13S18-1.xls", 4, "Nigeria", 11.0667, 7.7, 648, "Zaria", NA, NA, NA)
	d4 <- get_data("06EVT13S26-1.xls", 5, "Mexico", -96.6667, 19.3333, 15, "Cotaxtla", "Veracruz", NA, NA)
	d5 <- get_data("06EVT13S30-1.xls", 6, "Ghana", -1.5833, 6.75, 720, "Fumesua", NA, "2007-04-19", "2007-08-09")
	d6 <- get_data("06EVT13S31-1.xls", 7, "Ghana", -1.3667, 7.3833, 232, "Ejura", NA, "2007-05-08", "2007-12-09")
	
	d <- carobiner::bindr(d0, d1, d2, d3, d4, d5, d6)

	d$crop <- "maize"
	d$on_farm <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	d$borer_trial <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(meta, d, path=path)
}


