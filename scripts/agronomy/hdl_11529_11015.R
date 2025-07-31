# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"Nutrient Ommission Trials (NOT's) conducted in two zones (West Showa and Jimma) in Ethiopia in 2015 and 2016. Trials comprise six nutrient management treatements, namely Control (zero fertilzer), PK, NK, PK, NPK, NPK+Ca+Mg+Zn+B. Trials were conducted on-farm with six plots per farm. Observations include soil analysis (0-20cm), biomass and grain yields"

	uri <- "hdl:11529/11015"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "fertilizer_used;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2025-07-31",
		notes = NA,
		design = NA
	)
	
	f <- ff[basename(ff) == "TAMASA_ET_NOT_2015_&_2016F.xlsx"]
	r <- carobiner::read.excel(f,sheet = "Raw_data Harvest parameters")
	r1 <- carobiner::read.excel(f,sheet = "Pre_soil sample analysis")
	
	cols_to_fill <- c("Code", "District", "Peasant Association")
	
	for (col in cols_to_fill) {
	  for (i in 1:nrow(r)) {
	    if (is.na(r[i, col]) || r[i, col] == "") {
	      r[i, col] <- r[i - 1, col]
	    }
	  }
	}
	
	d <- data.frame(
		country = "Ethiopia",
		crop= "maize", 
		code = r$Code,
		location = r$Location,
		site = r$District,
		elevation = r$`Altitude (m.a.s.l)`,
		treatment = r$Treatment,
		fwy_total = r$`Biomass Weight (kg/18m2)`*556,
		yield_moisture = r$`Grain moisture content(%)`,
		dm_yield = r$`Grain Yield (kg/ha)`,
		plant_density = r$`Crop Stand (Number/18m2)`*556,
		cob_density = r$`Crop Stand (Number/18m2)`*556,
		yield_part = "grain",
		yield = r$`Grain Yield (kg/ha)`
	)

	d$trial_id <- as.character(as.integer(as.factor(1)))
	

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$borer_trial <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	
	d$longitude <- 37.135 
	d$latitude <- 8.41
	d$geo_from_source <- TRUE

	d$planting_date <- as.character(as.Date(r$Year))
	d$harvest_date <- NA
	d$harvest_date[d$planting_date=="2015"]  <- as.character("2016")
	d$harvest_date[d$planting_date=="2016"]  <- as.character("2017")

	d$fertilizer_used <- ifelse(d$treatment=="Control",FALSE,TRUE)
	d$fertilizer_type <- NA
	d$fertilizer_type[d$treatment=="NP (-K)"] <- "NP"
	d$fertilizer_type[d$treatment=="NPK"] <- "NPK"
  d$P_fertilizer <- ifelse(d$treatment=="Control",0,NA)
  d$K_fertilizer <- ifelse(d$treatment=="Control",0,NA)
  d$N_fertilizer <- ifelse(d$treatment=="Control",0,NA)
  d$S_fertilizer <- ifelse(d$treatment=="Control",0,NA)

  d1 <- data.frame(
    code = r1$Code,
    depth_top = 0,
    depth_bottom = 20,
    soil_pH = r1$pH,
    soil_N = r1$`TN (%)`*10000,
    soil_P_total = r1$`P (ppm) or  mg/kg soil)`,
    soil_SOC = r1$`OC (%)`,
    soil_K = r1$`K (mg/kg soil)`) 
  
  d <- merge(d,d1,by = "code")
  d$soil_N[d$soil_N > 10000] <- NA
  d$Code <- NULL

	carobiner::write_files(path, meta, d)
}


