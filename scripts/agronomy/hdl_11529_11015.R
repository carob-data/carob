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

	d1$trial_id <- as.character(as.integer(as.factor(1)))

	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$irrigated <- FALSE
	d1$borer_trial <- FALSE
	d1$striga_trial <- FALSE
	d1$striga_infected <- FALSE
	
	#d1$longitude <- 37.135 
	#d1$latitude <- 8.41
	#d1$geo_from_source <- FALSE

	d1$planting_date <- as.character(as.Date(r1$Year))
	d1$harvest_date <- NA
	d1$harvest_date[d1$planting_date=="2015"]  <- as.character("2016")
	d1$harvest_date[d1$planting_date=="2016"]  <- as.character("2017")

	d1$fertilizer_used <- d1$treatment != "Control"
	d1$fertilizer_type <- NA
	d1$fertilizer_type[d1$treatment=="NP (-K)"] <- "NP"
	d1$fertilizer_type[d1$treatment=="NPK"] <- "NPK"
	d1$P_fertilizer <- ifelse(d1$treatment=="Control", 0, NA)
	d1$K_fertilizer <- ifelse(d1$treatment=="Control", 0, NA)
	d1$N_fertilizer <- ifelse(d1$treatment=="Control", 0, NA)
	d1$S_fertilizer <- ifelse(d1$treatment=="Control", 0, NA)

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
	d$Code <- NULL

	carobiner::write_files(path, meta, d)
}


