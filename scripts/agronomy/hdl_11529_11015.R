# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
TAMASA Ethiopia. Nutrient omission trial (NOT) datasets  for 2015 and 2016 seasons

Nutrient Ommission Trials (NOT's) conducted  in two zones (West Showa and Jimma) in Ethiopia in 2015 and 2016. Trials comprise six  nutrient management treatements, namely Control (zero fertilzer), PK, NK, PK, NPK, NPK+Ca+Mg+Zn+B. Trials were conducted on-farm with six  plots per farm. Observations include soil analysis (0-20cm),   biomass and grain yields
"

	uri <- "hdl:11529/11015"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT;EIAR",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2025-07-31",
		notes = NA,
		design = NA
	)
	
	f <- ff[basename(ff) == "TAMASA_ET_NOT_2015_&_2016F.xlsx"]
	r1 <- carobiner::read.excel(f,sheet = "Raw_data Harvest parameters")
	r2 <- carobiner::read.excel(f,sheet = "Pre_soil sample analysis")

	r1 <- r1[!is.na(r1$Location), ]
	r1$Code[is.na(r1$Code) & r1$Treatment == "Control"] <- 47
	r1$Code[r1$Year == 2016] <- r1$Code[r1$Year == 2016] + 1
		
	cols_to_fill <- c("Code", "District", "Peasant Association")
	for (col in cols_to_fill) {
	  for (i in 1:nrow(r1)) {
	    if (is.na(r1[i, col]) || r1[i, col] == "") {
	      r1[i, col] <- r1[i - 1, col]
	    }
	  }
	}
	
	d1 <- data.frame(
		country = "Ethiopia",
		crop= "maize",
		code = r1$Code,
		location = r1$Location,
		site = r1$District,
		elevation = r1$`Altitude (m.a.s.l)`,
		treatment = r1$Treatment,
		fwy_total = r1$`Biomass Weight (kg/18m2)`*556,
		yield_moisture = r1$`Grain moisture content(%)`,
		dm_yield = r1$`Grain Yield (kg/ha)`,
		plant_density = r1$`Crop Stand (Number/18m2)`*556,
		cob_density = r1$`Crop Stand (Number/18m2)`*556,
		yield_part = "grain",
		yield = r1$`Grain Yield (kg/ha)`
	)

	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$irrigated <- FALSE
	d1$borer_trial <- FALSE
	d1$striga_trial <- FALSE
	d1$striga_infected <- FALSE
	
	d1$planting_date <- as.character(as.Date(r1$Year))
	d1$harvest_date <- NA
	d1$harvest_date[d1$planting_date=="2015"]  <- as.character("2016")
	d1$harvest_date[d1$planting_date=="2016"]  <- as.character("2017")

	d1$fertilizer_used <- d1$treatment != "Control"
	d1$fertilizer_type <- ""
	d1$N_fertilizer <- d1$P_fertilizer <- d1$K_fertilizer <- d1$S_fertilizer <- d1$Ca_fertilizer <- d1$Mg_fertilizer <- d1$Zn_fertilizer <- d1$B_fertilizer <- 0
	i <- grep("^N", d1$treatment)
	d1$N_fertilizer[i] <- 120
	d1$fertilizer_type[i] <- paste0(d1$fertilizer_type[i], "urea")
	i <- grep("^NP|PK", d1$treatment)
	d1$P_fertilizer[i] <- 40
	d1$fertilizer_type[i] <- paste0(d1$fertilizer_type[i], ";TSP")
	i <- grep("^NK|PK", d1$treatment)
	d1$K_fertilizer[i] <- 40
	d1$fertilizer_type[i] <- paste0(d1$fertilizer_type[i], ";KCl")
	i <- grep("Mg", d1$treatment)	
	d1$S_fertilizer[i] <- 20
	d1$Ca_fertilizer[i] <- 10
	d1$Mg_fertilizer[i] <- 10
	d1$Zn_fertilizer[i] <- 5
	d1$B_fertilizer[i] <- 5
	d1$fertilizer_type[i] <- paste0(d1$fertilizer_type[i], ";gypsum;MgSO4;ZnSO4;borax")
	d1$fertilizer_type <- gsub("^;",  "", d1$fertilizer_type)
	d1$fertilizer_type[d1$fertilizer_type == ""] <- "none"

	d2 <- data.frame(
		code = r2$Code,
		depth_top = 0,
		depth_bottom = 20,
		soil_pH = r2$pH,
		soil_N = r2$`TN (%)`*10000,
		soil_P_total = r2$`P (ppm) or  mg/kg soil)`,
		soil_SOC = r2$`OC (%)`,
		soil_K = r2$`K (mg/kg soil)`
	) 
	d2$soil_N[d2$soil_N > 10000] <- NA
	  
	d <- merge(d1, d2, by = "code", all.x=TRUE)

	geo <- data.frame(
		code = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67), 
		longitude = c(37.326, 37.3047, 37.2869, 37.2649, 37.2735, 37.2506, 37.2505, 37.2427, 37.2477, 37.2448, 37.2488, 37.2224, 37.2236, 37.2263, 37.2388, 37.044, 37.0261, 37.0164, 37.0199, 36.947, 36.9548, 36.9487, 36.9227, 36.9099, 37.206, 37.1937, 37.1758, 37.1617, 37.1617, 37.1507, 37.1584, 37.1713, 37.144, 37.1344, 37.089, 37.0876, 37.0978, 37.044, 37.0478, 37.0306, 37.0265, 37.0262, 37.0075, 37.004, 37.0096, 36.9382, 36.9382, 37.2869, 37.3579, 37.3422, 37.3047, 37.2657, 37.2253, 37.2258, 37.2236, 37.0199, 37.2168, 37.2307, 37.2337, 37.2353, 37.2253, 37.0164, 37.0182, 36.9739, 36.9487, 36.979, 37.0295), 
		latitude = c(7.7164, 7.7316, 7.7304, 7.7303, 7.7328, 7.6964, 7.6964, 7.6723, 7.6551, 7.6606, 7.6449, 7.7162, 7.7385, 7.7302, 7.7658, 7.7119, 7.7067, 7.7179, 7.7167, 7.6872, 7.7232, 7.7294, 7.6997, 7.689, 9.0393, 9.043, 9.0715, 9.0772, 9.0772, 9.0821, 9.0912, 9.1096, 9.0888, 9.0877, 9.0797, 9.1144, 9.1191, 9.1491, 9.1519, 9.1559, 9.1811, 9.1227, 9.1111, 9.0465, 9.033, 9.1069, 9.1069, 7.7304, 7.7459, 7.7554, 7.7316, 7.7296, 7.8529, 7.719, 7.7385, 7.7167, 7.7178, 7.8311, 7.8299, 7.8328, 7.8529, 7.7179, 7.7207, 7.6835, 7.7294, 7.6848, 7.7067)
	) 

	d <- merge(d, geo, by="code")
	d$trial_id <- as.integer(d$code)
	d$code <- NULL
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}


