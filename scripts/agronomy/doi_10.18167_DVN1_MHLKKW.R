# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Crop yield, soil carbon stocks and fractions in a 20-year experiment with fertilizer and compost addition in Ivory Coast

These are the raw data of the paper 'Sustaining maize yields and soil carbon following land clearing in the forest–savannah transition zone of West Africa: Results from a 20-year experiment' authored by Rémi Cardinael, Hervé Guibert, Soumaïla T. Kouassi Brédoumy, Jacques Gigou, Kouadio Emmanuel N'Goran, Marc Corbeels. 

More specificially, the dataset contains weather data, crop yield, soil properties including soil organic carbon stocks and fractions from a long-term experiment (1971-1992) in Ivory Coast combining 12 treatments, replicated 8 times, of different combinations of mineral nitrogen fertilizer and compost additions.
"

	uri <- "doi:10.18167/DVN1/MHLKKW"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.fcr.2021.108335",
		project = NA,
		design = "randomized block design" ,
		data_type = "on-farm experiment",
		treatment_vars = "OM_used;fertilizer_used",
		response_vars = "yield;soil_SOC", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-24",
		carob_completion = 80,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "Dataset_Gagnoa.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Legend")
	r1b <- suppressWarnings(carobiner::read.excel(f1, sheet="Yield"))
	r1c <- carobiner::read.excel(f1, sheet="Soil", na="NA")
	r1d <- carobiner::read.excel(f1, sheet="Fractionation", na="NA")
	r1e <- carobiner::read.excel(f1, sheet="Rainfall")
	r1f <- carobiner::read.excel(f1, sheet="Max_air_temperature", na="NA")
	r1g <- carobiner::read.excel(f1, sheet="Min_air_temperature", na="NA")
	
	d1 <- data.frame(
	  country = "Côte d'Ivoire",
	  location = "Institut des Savanes (IDESSA), Gagnoa research station", 
	  plot_id = as.character(r1b$ID),
	  date = as.character(r1b$Date),
	  treatment = as.character(r1b$Treatment),
	  variety = r1b$Variety,
	  rep = as.integer(r1b$Replicate),
	  OM_amount = r1b$Treat_Compost_tDM,
	  N_fertilizer = r1b$Treat_Fertilizer_kgN,
	  yield = r1b$Grain_yield * 1000,
	  #crop_cycle = r1b$Cycle, ### number of cycles
	  soil_C_litter = r1b$ABG_tC * 1000,
	  dmy_roots = r1b$Root_DM,
	  dmy_residue = r1b$Straw_DM,
	  residue_C = r1b$Straw_tC*1000,#r1b$Compost_tC
	  crop =  tolower(r1b$Crop) 
	)

	d1$crop[d1$crop == "fallow"] <- "none"
	d1$OM_used <- r1b$Treat_Compost_tDM > 0
	d1$OM_type <- ifelse(d1$OM_used, "compost", "none")
	d1$fertilizer_used <- r1b$Treat_Fertilizer_kgN > 0
#	d1$fertilizer_type <- NA
  d1$residue_C[d1$residue_C == 0] <- NA

	d2 <- data.frame(
	  plot_id = as.character(r1c$ID),
	  date = as.character(r1c$Date),
	  treatment = as.character(r1c$Traitement),
	  rep = as.integer(ifelse(r1c$Replicate == "Composite", NA, r1c$Replicate)),
	  soil_pH = as.numeric(r1c$pH_water),
	  soil_pH_KCl = as.numeric(r1c$pH_KCl),
	  soil_K_exch = as.numeric(r1c$K_ech_cmole_kg),
	  soil_Ca_exch = as.numeric(r1c$Ca_ech_cmole_kg),
	  soil_Na_exch = as.numeric(r1c$Na_ech_cmole_kg),
	  soil_Mg_exch = as.numeric(r1c$Mg_ech_cmole_kg),
	  soil_P = as.numeric(r1c$P_ass_mg_kg),
	  soil_P_total = as.numeric(r1c$P_tot_mg_kg),
	  soil_N_total = as.numeric(r1c$Ntot_mg_g),
	  soil_CEC = as.numeric(r1c$CEC_cmole_kg)
	)
	d2$soil_pH_KCl[d2$soil_pH_KCl == 1.09] <- NA

	
	d3 <- data.frame(
	  treatment    = gsub("temoin", "control", tolower(r1d$Treatment)),
	  rep          = as.integer(r1d$Field_replicate),
	  method       = r1d$Method,
	  soil_SOC = as.numeric(r1d$Contenu_C_g_kg_sol) * 100,
	  soil_N   = as.numeric(r1d$Contenu_N_g_kg_sol) 
	)	
	
	d3$treatment <- as.character(d3$treatment)
	d3$treatment[d3$treatment == "control"] <- "1" 
	d3$treatment[d3$treatment == "compost"] <- "7"
	
	d3$Teneur_C <- r1d$Teneur_C_g_kg_fraction #concentration of carbon within that specific soil fraction
	d3$Teneur_N <- r1d$Teneur_N_g_kg_fraction #concentration of N within that specific soil fraction
	#d3$Proportion_C <-rd1$Proportion_C_sol_g_kg #proportional contributions of each fraction to the total soil carbon or nitorgen pool per given soil mass
	#d3$Proportion_C <- rd1$Proportion_N_sol_g_kg
	#d3$C_N_ratio <- rd1$C_N
	# Excluded  in  Age  in d3 since it is not clear if its the age of the sample or what.
	
	wth <- data.frame(
		date = as.Date(r1e$Date),
		prec = r1e$Rain
	)
	
	d5 <- data.frame(
	  date = r1f$...1,
	  tmax   = rowMeans(sapply(r1f[,-1], function(x) as.numeric(as.character(x))), na.rm = TRUE)
	)
		
	d6 <- data.frame(
	  date = r1g$...1,
	  tmin   = rowMeans(sapply(r1g[,-1], function(x) as.numeric(as.character(x))), na.rm = TRUE)
	)
	
	d7 <- merge(d5, d6, by = "date")
	d7 <- data.frame(
	  date = d7$date,
	  temp   = (d7$tmax + d7$tmin) / 2
	)
	
	d <- merge(d1, d2, by = c("plot_id", "date", "treatment", "rep"), all = TRUE)

	d <- merge(d, d3, by = c("treatment", "rep"), all = TRUE)
	d <- merge(d, d7, by = "date", all = TRUE)
	
	d$trial_id <- "1"
	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- NA

	# article has 06°08' N, 5°56' W (6.133, -5.9333, Hotel Le Flamboyan)
	# but the (now CNRA) research station is at 6.1329, -5.9017
	# geo_uncertainty set to 500 m
	d$longitude <- -5.9017
	d$latitude <- 6.1329
	d$geo_uncertainty = 500 
	d$geo_source = "Google Maps"
  
	d$geo_from_source <- FALSE ## within a few km though
	d$planting_date <- d$harvest_date <- NA
	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
	
	d$yield_part <- "grain"
	d$yield_isfresh <- FALSE 
	d$yield_moisture <- 0
	
	carobiner::write_files(path, meta, d)
}

