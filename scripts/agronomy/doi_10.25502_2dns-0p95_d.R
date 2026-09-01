# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
EiA, Sasakawa Africa Association (SAA) Nigeria Use Case, Nutrient Omission Trials (NOTs) for Maize in Benue State, Nigeria

The objective of the EiA SAA Nigeria Use Case is to combine 3 fertilizer recommendation tools (AKILIMO for cassava, Nutrient Expert (NE) for maize and Rice Advice for rice) in one interface. Following the request of the “demand partner” Sasakwa Africa Association (SAA), Nigeria, the decision support tools (DSTs) for fertilizer will be combined with advice on the planting or sowing windows.
"

	uri <- "doi:10.25502/2dns-0p95/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; BUK",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
	    response_vars = "yield", 
		notes = NA,
		carob_contributor = "Mitchelle",
		carob_date = "2026-08-30",
	    carob_completion = 100,	
		carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "rawdata_soil.csv"]
	f2 <- ff[basename(ff) == "rawdata_yield.csv"]
	f3 <- ff[basename(ff) == "rawadata_field_mgt.csv"]
	f4 <- ff[basename(ff) == "rawdata_previous_field_mgt_hist.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	
	d1 <- data.frame(
		country = r1$country,
		field_id = r1$Field_ID,
		plot_id = r1$Plot_ID,
		hhid = r1$HHID,
		latitude = r1$latitude_North,
		longitude = r1$longitude_East,
		elevation = r1$altitude_m,
		geo_uncertainty = r1$precision_m,
		treatment = r1$Trt,
		variety_type = r1$Variety,
		sample_id = r1$Sample_ID,
		soil_pH = r1$pH_H20_.1.2.5.,
		soil_EC = r1$EC_Us_cm/1000,
		soil_SOC = r1$OC,
		soil_N = r1$N * 10000,
		soil_sand = r1$Sand,
		soil_clay = r1$Clay,
		soil_silt = r1$Silt,
		soil_Ca = r1$Ca_cmol_kg * 200.4,
		soil_Mg = r1$Mg_cmol_kg * 121.53,
		soil_K = r1$K_cmol_kg * 391.02,
		soil_Na = r1$Na_cmol_kg * 229.90,
		soil_ex_acidity = r1$Exch_Acidity_cmol_kg,
		soil_ECEC = r1$ECEC_cmol_kg,
		soil_Zn = r1$Zn_ppm,
		soil_Cu = r1$Cu_ppm,
		soil_Mn = r1$Mn_ppm,
		soil_Fe = r1$Fe_ppm
	)
	
	d2 <- data.frame(
	  country = r2$country,
		field_id = r2$Field_ID,
		plot_id = r2$Plot_ID,
		hhid = r2$HHID,
		treatment = r2$Trt,
		variety_type = r2$Variety,
		harvest_date = as.character(as.Date(r2$HarvestDate_E6, format = "%m/%d/%Y")),
		plot_area = r2$plot_size,
		moist = r2$GrainMoisture_perc_E7,
		yield = r2$GrainYield_kg_per_ha_C,
		yield_moisture = 14,                          #Grain yield (kg per ha) @ 14% moisture content 
		fwy_residue = r2$StalkYield_kg_per_ha_C,     #Stalk yield (kg per ha) @ 14% moisture content 
		dmy_residue = r2$DryWightkg_per_plot_of_maize_stalk_kg_per_netplot_C * 333.33,
		harvest_index = r2$Harvest_Index_C,
		plant_density = r2$plant_population_per_ha_C
	)

	d3 <- data.frame(
	  country = r3$country,
	  field_id = r3$Field_ID,
	  plot_id = r3$Plot_ID,
	  hhid = r3$HHID,
	  treatment = r3$Trt,
	  variety_type = r3$Variety,
	  planting_date = as.character(as.Date(r3$BasalFertApplica_plantingDate_E2, format = "%m/%d/%Y")),
	  drought_stress = as.character(r3$Monitoring_flowering_stage_rateDrought_E5),
	  flood_stress = as.character(r3$Monitoring_flowering_stage_rateWaterLogging_E5),
	  borer_dam_rat = r3$Monitoring_flowering_stage_rateStemborer_E5,
	  pest_severity = as.character(r3$Monitoring_flowering_stage_rateOtherPests_E5),
	  weed_severity = as.character(r3$Monitoring_flowering_stage_rateWeeds_E5),
	  disease_severity = as.character(r3$Monitoring_flowering_stage_rateOtherDisease_E5)
	)

	comb <- function(v) {
		v <- apply(v, 1, \(x) paste(as.Date(x[x != ""], format = "%m/%d/%Y"), collapse=";"))
		v[v == ""] <- NA
		v
	}
	
	d3$fertilizer_date <- comb(r3[, grep("Date_of_.*fertilizer_appliction", names(r3))])
	d3$fertilizer_dap <- comb(r3[, grep("Days_to_.*_fert_app", names(r3))]) 
	d3$weeding_dates <- comb(r3[, grep("weedingDetails_dateWeeding", names(r3))])
	d3$weeding_method <- comb(r3[, grep("weedingDetails_dateMethod", names(r3))])	
	
	d4 <- data.frame(
		country = r4$country,
		field_id = r4$Field_ID,
		plot_id = r4$Plot_ID,
		hhid = r4$HHID,
		treatment = r4$Trt,
		variety_type = r4$Variety,
		farmland = r4$Farm_Size_ha_E1,
		OM_type = "farmyard manure; poultry manure; cattle dung; sewage sludge; compost",
		fertilizer_type = "NPK; DAP; KCl; SSP; urea",
		N_fertilizer = r4$N_kg_per_ha_E1,
		P_fertilizer = r4$P2O5_kg_per_ha_E1/2.29,
		K_fertilizer = r4$K2O_kg_per_ha_E1/1.2051,
		previous_crop_residue_management = r4$Crop_Residue_MGT_E1
	)
	
	crop_cols <- c("pastcrop_Sorghum_E1", "pascrop_Maize_E1", "pascrop_Soybean_E1", "pascrop_Cowpea_E1",
	  "pascrop_Groundnut_E1", "Event_1_Pepper_E1", "pascrop_Onion_E1", "pascrop_Okro_E1", "pascrop_Cassava_E1", "pascrop_Yam_E1")

    crops = sapply(r4[, grep("crop_|Event_1_Pepper", names(r4))], tolower)
	crops <- apply(crops, 1, function(x) {
		x <- x[!is.na(x) & trimws(x) != ""]
		paste(x, collapse = "; ")
	})
	
	d4$crop_rotation <- ifelse(r4$Crop_system_E1 == "Rotation (cereal-legume)", crops, NA)
	d4$intercrops    <- ifelse(r4$Crop_system_E1 == "Mixed or Inter Crop", gsub("; ", "_", crops), NA)
	
	d5 <- merge(d1, d2, by = c("country","field_id","plot_id","hhid","treatment","variety_type"), all.x = TRUE)
	d6 <- merge(d5, d3, by = c("country","field_id","plot_id","hhid","treatment","variety_type"), all.x = TRUE)
	d <-  merge(d6, d4, by = c("country","field_id","plot_id","hhid","treatment","variety_type"), all.x = TRUE)
	
	d$trial_id <- as.character(as.integer(as.factor(1)))
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$crop <- "maize"
	d$geo_from_source <- TRUE
	d$yield_part <- "grain"
	d$yield_isfresh <- TRUE
	d$country[d$country=="NG"] <- "Nigeria"
	d$crop_rotation <- tolower(d$crop_rotation)
	d$intercrops <- tolower(d$intercrops)
	
	carobiner::write_files(path, meta, d)
}
