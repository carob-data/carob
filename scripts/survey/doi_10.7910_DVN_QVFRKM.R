# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. added new variables

carob_script <- function(path) {

  
" Farmer field survey data on rice sowing dates, yield, and nutrient use in the Senegal River Valley

 This dataset contains plot-level survey data collected from rice farmers’ fields to evaluate the influence of sowing date on rice yield and nutrient use efficiency under real smallholder production conditions. Data were collected through structured farmer surveys. The dataset captures variability in sowing dates, crop management practices, fertilizer use, and yield across contrasting seasons and production environments. The data reflect farmer decision making rather than controlled experimental treatments, making it particularly suitable for assessing agronomic performance under realistic management conditions. Linking sowing date information with yield and nutrient input data, the dataset enables analysis of optimal planting windows, yield penalties associated with delayed sowing, and implications for nutrient use efficiency in rice-based systems. The dataset supports agronomic research, digital advisory tool development, and evidence-based extension recommendations. (2025-12-15)"


	uri <- "doi:10.7910/DVN/QVFRKM"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

		meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-06-15",
		notes = NA, 
		design = NA
	)
	

	f <- ff[basename(ff) == "Farmer field survey data on rice sowing dates yield and nutrient use in the Senegal River Valley.xls"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		country = "Senegal",
		date=as.character(r$year),
		adm3=r$village,
		sex=r$farmer_gender,
		age=as.numeric(r$farmer_age),
		farmer_occupation=r$farmer_occupation,
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
		transplanting_days=r$time_btw_sowing_transplanting,
		optimum_planting=r$sowing_window
		)
	
	#convertng season constraints from wide to long format
	d$season_constraint <- apply(r[, c("factor_affecting_sowing_draught",
"factor_affecting_sowing_flooding","factor_affecting_sowing_cold",
"factor_affecting_sowing_heat","factor_affecting_sowing_salinity",
"factor_affecting_sowing_weed","factor_affecting_sowing_pest",                     
"factor_affecting_sowing_diseases","factor_affecting_sowing_membership",               
"factor_affecting_sowing_land_ownership","factor_affecting_sowing_unavailability_seeds",     
"factor_affecting_sowing_unavailabitity_fertiliser","factor_affecting_sowing_unavaibility_labor",       
"factor_affecting_sowing_irrigation","factor_affecting_sowing_unavailability_credit",    
"factor_affecting_sowing_delay_acquiring_financing")], 1, function(x) {
  
	 conditions = c("factor_affecting_sowing_draught"= "drought",
	                "factor_affecting_sowing_flooding"="flood",
	               "factor_affecting_sowing_cold"="cold",
	               "factor_affecting_sowing_heat"="heat",
	               "factor_affecting_sowing_salinity"="soil_salinity",
	               "factor_affecting_sowing_weed"="weeds",                     
	               "factor_affecting_sowing_pest"="pests",                     
	               "factor_affecting_sowing_diseases"="disease",
	               "factor_affecting_sowing_membership"="other",               
	               "factor_affecting_sowing_land_ownership"="land_ownership",           
	               "factor_affecting_sowing_unavailability_seeds"="seed",     
	               "factor_affecting_sowing_unavailabitity_fertiliser"="fertilizer",
	               "factor_affecting_sowing_unavaibility_labor"="labour",       
	               "factor_affecting_sowing_irrigation"="irrigation",               
	               "factor_affecting_sowing_unavailability_credit"="other",    
	               "factor_affecting_sowing_delay_acquiring_financing"="capital")
	                                    
	     flagged <- conditions[x == "yes"]
	     if (length(flagged) == 0) return(NA)
	     paste(flagged, collapse = "; ")})
	
  
	d$trial_id <- paste(d$adm3,d$planting_date,sep = "_")
	d$K_fertilizer<- as.numeric(NA)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
  d$yield_isfresh <- TRUE
	d$N_efficiency <- r$nue_kg_kg
	d$P_efficiency <- r$pue_kg_kg
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)

  d$season <- ifelse(d$season=="DS","dry","wet")
  d$sex <- ifelse(d$sex=="Masculin","male","female")
  
  #manually adding coordinates because some of datasets' coordinates are in Mauritania
  d$adm3 <- trimws(d$adm3)  
  
  d$adm3[d$adm3 == "Débi"]         <- "Debi"
  d$adm3[d$adm3 == "Kassack nord"] <- "Kassack Nord"
  d$adm3[d$adm3 == "Keur mbaye"]   <- "Keur Mbaye"
  d$adm3[d$adm3 == "Ross-Bèthio"]  <- "Ross-Bethio"
  d$adm3[d$adm3 == "Thiago"] <- "Thiagar"
  
  loc <- data.frame(
    adm3      = c("Mbagame", "Debi", "Mbilor", "Mboltogne", "Kassack Nord",
                  "Ndiareme", "Ndieurba", "Diawar", "Ronkh", "Maka Diama",
                  "Ndiatene", "Fanaye", "Rosso", "Kheune", "Ndelle",
                  "Savoigne", "Keur Mbaye", "Lougue Demisse", "Ross-Bethio",
                  "Bokhol", "Gaé", "Thiagar", "Thillene", "Wassoul",
                  "Lampsar", "Ndombo", "Dagana", "Ndiol", "Mboundoum"),
    latitude  = c(16.483,  16.467,  16.508,  16.160,  16.400,
                  14.779,  16.500,  15.023,  16.475,  16.200,
                  16.500,  16.500,  16.420,  16.500,  16.267,
                  16.200,  14.742,  16.211,  16.267,  16.500,
                  16.578,  16.500,  16.171,  16.478,  16.400,
                  16.433,  16.483,  15.900,  16.400),
    longitude = c(-15.783, -16.283, -15.584, -16.292, -16.000,
                  -17.384, -15.300, -12.544, -15.988, -16.400,
                  -15.900, -15.200, -15.799, -16.100, -15.883,
                  -16.300, -17.319, -16.015, -16.133, -15.400,
                  -15.450, -15.900, -15.697, -16.025, -16.100,
                  -15.700, -15.600, -16.500, -16.100))
  
  d <- merge(d,loc,by="adm3", all.x=T)
  
	carobiner::write_files(path, meta, d)
}

