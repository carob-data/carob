# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
Farmer field survey data on rice sowing dates, yield, and nutrient use in the Senegal River Valley

This dataset contains plot-level survey data collected from rice farmers’ fields to evaluate the influence of sowing date on rice yield and nutrient use efficiency under real smallholder production conditions. Data were collected through structured farmer surveys. The dataset captures variability in sowing dates, crop management practices, fertilizer use, and yield across contrasting seasons and production environments. The data reflect farmer decision making rather than controlled experimental treatments, making it particularly suitable for assessing agronomic performance under realistic management conditions. Linking sowing date information with yield and nutrient input data, the dataset enables analysis of optimal planting windows, yield penalties associated with delayed sowing, and implications for nutrient use efficiency in rice-based systems. The dataset supports agronomic research, digital advisory tool development, and evidence-based extension recommendations. (2025-12-15)
"
	
	
	uri <- "doi:10.7910/DVN/QVFRKM"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = "Sustainable Farming Program",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_completion = 100,
		carob_contributor = "Blessing Dzuda; Kora Simperegui",
		carob_date = "2025-06-15",
		notes = NA, 
		design = NA
	)
	

	f <- ff[basename(ff) == "data.xls"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		country = "Senegal",
		date=as.character(r$year),
		location=r$village,
		sex=r$farmer_gender,
		age=as.numeric(r$farmer_age),
		occupation=r$farmer_occupation,
		field_size=r$field_size_ha,
		crop="rice",
		variety=r$main_variety_use,
		planting_method=r$sowing_mode,
		yield=as.numeric(r$yield_t_ha)*1000,
		N_fertilizer=r$n_applied_kg_ha,
		P_fertilizer=r$p_applied_kg_ha,
		season=r$season,
		planting_date=as.character(r$sowing_date),
		maturity_date=as.character(r$date_to_maturity),
		transplanting_days=r$time_btw_sowing_transplanting
		## this seems to be a computed variable
		##optimum_planting=r$sowing_window
	)
	
	#convertng season constraints from wide to long format
	rr <- r[, grep("factor_affecting_sowing_", names(r))]
	nms <- gsub("factor_affecting_sowing_|unavailabi.ity_|unavaibility_|delay_acquiring_", "", names(rr))
	nms <- gsub("draught", "drought", nms)
	nms <- gsub("flooding", "flood", nms)
	nms <- gsub("weed", "weeds", nms)
	nms <- gsub("pest", "pests", nms)
	nms <- gsub("seeds", "seed", nms)
	nms <- gsub("labor", "labour", nms)
	nms <- gsub("fertiliser", "fertilizer", nms)
	nms <- gsub("membership", "other", nms)
	
	out <- matrix(rep(nms, each=nrow(rr)), nrow=nrow(rr))
	out[rr != "yes"] <- NA
	d$season_constraint <- apply(out, 1, \(x) paste(na.omit(x), collapse=";"))
  
	d$trial_id <- paste(d$location, d$planting_date, sep = "_")
	d$K_fertilizer<- as.numeric(NA)
	d$fertilizer_used <- !(d$K_fertilizer == 0 & d$N_fertilizer == 0 & d$P_fertilizer == 0) #No application of fertilizer for experiments with K_fertilizer = N_fertilizer = P_fertilizer == 0
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
	d$yield_isfresh <- TRUE
	# N (or P) efficienty was computed from the data as d$yield / d$N_fertilizer
	# no need to include it (and this is not a great measure for NUE anyway).
	#d$N_efficiency <- r$nue_kg_kg
	#d$P_efficiency <- r$pue_kg_kg
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)

  d$season <- ifelse(d$season=="DS", "dry", "wet")
  d$sex <- ifelse(d$sex=="Masculin", "male", "female")
  
  d$location <- trimws(d$location)  
  
  d$location[d$location == "Débi"]         <- "Debi"
  d$location[d$location == "Kassack nord"] <- "Kassack Nord"
  d$location[d$location == "Keur mbaye"]   <- "Keur Mbaye"
  d$location[d$location == "Ross-Bèthio"]  <- "Ross-Bethio"
  d$location[d$location == "Thiago"] <- "Thiagar"
  
  #The initial data comprises observations from Senegal and some in Mauritania, taken along the Senegal River Valley. However, observations from Mauritania were removed from the updated version of the data because the focus of the study was Senegal.
  #Therefore, there is no need to replace the coordinates of points in Mauritania with those in Senegal, as they were correct. 
  d$longitude <- as.numeric(gsub(",", ".", r$gps_long))
  d$latitude <- as.numeric(gsub(",", ".", r$gps_lat))
  
	carobiner::write_files(path, meta, d)
}

