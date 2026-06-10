# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
On-farm rice yield responses to improved variety and nutrient management across salt-affected fields in the Senegal River Valley, Senegal

This dataset contains 86 field-level observations from on-farm rice trials conducted in 2024 in the Senegal River Valley (10 sites; 42 farmers), georeferenced with latitude and longitude (WGS84). Trials were implemented in both dry (DS, n = 56) and wet (WS, n = 30) seasons on moderately to strongly saline soils (soil electrical conductivity 3.1–7.63 dS m⁻¹). Each farmer hosts paired treatments comparing 'farmer practice' (Sahel 108 with NP fertilization: 133 kg N ha⁻¹ and 21 kg P ha⁻¹) against 'improved practices' that combine salinity-tolerant or improved varieties (ISRIZ10, Saltol, or Sahel 108) with balanced NPK (120 kg N, 26 kg P, 60 kg K ha⁻¹) and, in some cases, zinc (10 kg Zn ha⁻¹) and/or gypsum (100 kg ha⁻¹). For every plot, the dataset reports site, farmer, year, season, soil EC, treatment type, variety, fertilizer management, nutrient and gypsum application rates, and harvested paddy yield (2.3–7.3 t ha⁻¹). The data are suitable for analyzing yield response to salinity, variety and nutrient management, treatment comparison (farmer vs improved practices), and for spatial modelling of salt-affected rice systems under real farmer conditions.
"

	uri <- "doi:10.7910/DVN/SRANPI"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		carob_date = "2026-06-09",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		completion = 100,	
		notes = NA)
	

	f <- ff[basename(ff) == "On-farm_Salinity data.xls"]

	r <- carobiner::read.excel(f)

	d <- data.frame(
	  country="Senegal",
	  planting_date=as.character(r$year),
	  location=r$site,
	  latitude=r$lat,
	  longitude=r$long,
	  season=r$season,
	  crop="rice",
	  soil_EC=r$soil_EC_dS_m,
	  treatment=r$treatment,
	  variety=r$variety,
	  N_fertilizer=r$nitrogen_applied_kg_ha,
	  P_fertilizer=r$phosphorus_applied_kg_ha,
	  K_fertilizer=r$potassium_applied_kg_ha,
	  Zn_fertilizer=r$zinc_applied_kg_ha,
	  yield=r$paddy_yield_t_ha*1000
	)
	
	fert_type <- c(
	  "Sahel 108 + NP"="NP",
	  "Sahel 108 + NPK"="NPK",
	  "Saltol + Zinc"="ZnCl2", #in dataset, only zinc is mentioned, so had to look in carob to see which fert type adds zinc only, which is ZnCl2
	  "Saltol + NPK + Gyps"="NPK;gypsum",
	  "Salto + NPK + Gyps"="NPK;gypsum",
	  "ISRIZ10 + NPK +Gyps"="NPK;gypsum",
	  "ISRIZ10 +NPK + Gyps"="NPK;gypsum",
	  "ISRIZ10 +NPK + Zinc"="NPK;ZnCl2",
	  "ISRIZ10 + NPK"="NPK",
	  "Saltol + NPK"="NPK",
	  "ISRIZ10 + NPK + Zinc + Gyps"="NPK;ZnCl2;gypsum"
	  )
	
	d$fertilizer_type <- fert_type[d$treatment]
	d$season <- ifelse(d$season=="DS","dry","wet")
	d$latitude[is.na(d$latitude) & d$name == "Thilène"] <- 16.2712
	d$longitude[is.na(d$longitude) & d$name == "Thilène"] <- -16.1629
	d$trial_id <- paste(d$planting_date,d$location,sep = "-") 
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	carobiner::write_files(path, meta, d)
}

