# R script for "carob"

carob_script <- function(path) {

"The data were collected for the publication:Does leaving crop residues in the field lead to greater frost damage in wheat and barley under conservation agriculture? The dataset contains data from three experiments conducted to evaluate if leaving more residue in the field is associated with higher levels of frost damage. Farmers in the Bajio had indicated that leaving more crop residue in the field when sowing barley in permanent raised beds is associated with higher frost damage, which is an occasional problem in the region. To evaluate whether this is really the case frost damage was evaluated in a 1) a 2 year on farm experiment in San Juan del Rio, Queretaro, Mexico where barley was sown with different levels of residue retention, 2) An experiment with different types of tillage and levels of residue on the Sanjaya Rajaram station in Metepec, Mexico state, Mexico, where due to the high elevation frost damage was guaranteed to occur and 3) An comparison of yield in side by side comparison of conservation agriculture and conventional agriculture grown during the winter season across Mexico to assess whether conservation agriculture is associated with lower yields which could be indicative of a real problem."


	uri <- "hdl:11529/10548972"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;SAQ;CAU",
		publication="hdl:10883/22808",
		project=NA,
		data_type= "experiment",
		treatment_vars="land_prep_method;variety" , 
		response_vars="yield",
		carob_contributor= "Blessing Dzuda",
		carob_date="2024-05-16"
	)

	f <- ff[basename(ff) == "DAT-FrostExperiments.xlsx"]
	r <- carobiner::read.excel(f, sheet = "San Juan del Rio III")
	r <- r[1:96,1:56]

	d <- data.frame(
		  adm1=r$Estado,
		  latitude=r$Latitud,
		  longitude= -r$Longitud,
		  elevation=r$Altitud,
		  soil_texture=r$Soil,
		  rep=r$Num_Rep,
		  treatment=r$Name_tr,
		  crop_rotation=r$Crop_rotacion,
		  crop=r$Crop,
		  land_prep_method=r$Till,
		  previous_crop_residue_perc=r$Res_perc...28,
		  irrigation_number=r$Irrig,
		  variety=r$Variety,
		  N_fertilizer=r$Fert_N,
		  P_fertilizer=r$Fert_P,
		  K_fertilizer=r$Fert_K,
		  row_spacing=r$Row_dist*100,
		  plant_spacing=r$Intrarow*100,
		  plant_height=r$Height,
		  planting_date=r$Sowing_date,
		  emergence_date=r$Emergence_date,
		  flowering_date=r$Flower_days,
		  maturity_date=r$Mat_days,
		  harvest_date=r$Harvest_date,
		  plant_density=r$Plants_m2*10000,
		  dmy_storage=r$Yield_dry,
		  yield=r$Yield_moist,
		  seed_weight=r$Thousand,
		  crop_price=r$Price / 1000 # assuming it was per ton		  
	)

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$trial_id <- paste0(d$adm1,"_", d$rep)
	d$country <- "Mexico"
   d$geo_from_source <- TRUE
   
##### Time #####
	d$planting_date <- as.character(as.Date(d$planting_date))
	d$harvest_date  <- as.character(as.Date(d$harvest_date))
	d$emergence_date  <- as.character(as.Date(d$emergence_date))
	d$flowering_date  <- as.character(as.Date(d$flowering_date))
	d$maturity_date  <- as.character(as.Date(d$maturity_date))
	d$rep <- as.integer(d$rep)
	d$irrigation_number <- as.integer(d$irrigation_number)
	d$seed_weight <- as.numeric(d$seed_weight)
	
	d$yield_part <- "grain"
	d$crop <- gsub("Cebada", "barley", d$crop)
	d$crop <- gsub("Maíz", "maize", d$crop)
	d$crop_rotation <- gsub("M", "maize", d$crop_rotation)
	d$crop_rotation <- gsub("C", "barley", d$crop_rotation)
	d$crop_rotation <- gsub("-", ";", d$crop_rotation)
	d$soil_texture <- gsub("Vertisol", "clay", d$soil_texture)
	d$land_prep_method <- gsub("LC|LR", "conventional tilled beds", d$land_prep_method)
	d$land_prep_method <- gsub("CPA", "wide permanent beds", d$land_prep_method)
	d$land_prep_method <- gsub("CP", "conventional", d$land_prep_method)
	d$currency <- "MXN"
	i <- which(d$plant_height < 1)
	d$plant_height[i] <- d$plant_height[i] * 100

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}

