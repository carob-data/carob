# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Variations in rice grain zinc, iron and protein concentrations with genotypic, agronomic, soil and climatic variables: A meta-analysis

This database compiles a globally representative, meta-analysis–ready collection of rice biofortification experiments, capturing comprehensive plot-level information spanning environmental, genetic, agronomic, nutritional, and economic dimensions. It integrates detailed geographic and biophysical metadata (country, GPS coordinates, climate classification, aridity index, elevation, seasonal rainfall, long-term temperature and precipitation profiles), as well as full soil physicochemical characterization including pH, texture, organic carbon, macro- and micronutrient availability (N, P, K, Zn, Fe, etc.), cation exchange capacity, bulk density, and soil fertility status. For each trial entry, the dataset documents rice genetic background (species, subspecies, ecotype, cultivar name, variety type, origin, release year), crop establishment method, management practices (irrigation regime, tillage, residue recycling, farmyard manure use, inoculation), and full experimental treatment metadata — including nutrient application rates and forms (soil or foliar), growth-stage–specific timing, input economics, and treatment replication structures. The dataset further provides standardized agronomic outcomes such as total aboveground biomass, grain yield at 14% moisture, harvest index, nutrient-use efficiency (AEN, AEZn), and economic indicators (benefit–cost ratio). Critically, it includes high-resolution grain nutritional quality data, covering zinc, iron, protein, nitrogen, phytate, starch, oil, and a wide suite of mineral micronutrients (Ca, Mg, Se, B, Mn, Cu, Mo, Cd, As, Pb, Ni), along with associated uptake values and bioavailability-relevant molar ratios (e.g., phytate:Zn, phytate:Fe).
"

	uri <- "doi:10.7910/DVN/O71ROU"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "variety;N_fertilizer;P_fertilizer;K_fertilizer;Zn_fertilizer;land_prep_method;irrigated",
		response_vars = "yield;grain_Zn;grain_protein;grain_Fe", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-06-23",
		carob_completion = 100,	
		carob_effort = 6
	)
	

	f1 <- ff[basename(ff) == "data_Rice Biofortification data_meta-analysis.xls"]
	#f2 <- ff[basename(ff) == "dictionary_Rice Biofortification data_meta-analysis.xls"]
	#f3 <- ff[basename(ff) == "Publication Verdict List_Rice Biofortification data_meta-analysis.xls"]

	r1 <- suppressWarnings(carobiner::read.excel(f1, na= c("NA", "?", "N/A", "na")))
	#r2 <- carobiner::read.excel(f2)
	#r3 <- carobiner::read.excel(f3)


	d1 <- data.frame(
		reference = r1$study,
		trial_id = paste(r1$code, r1$climate_zone, r1$study_type, sep = "-"),
		#publication = r1$doi_url,
		#title = r1$study_title,
		#journal = r1$`Journal name`,
		country = r1$country,
		location = r1$trialsite,
		latitude = r1$latitude,
		longitude = r1$longitude,
		elevation = as.numeric(gsub("700-4000", (700+4000)/2, r1$elevation_masl)),
		planting_date = gsub("NA-Y|Year", NA,  substr(r1$yr_experiment, 1, 4)),
		season = tolower(r1$season),
		#r1$seasonal_rainfall_mm,
		rain = as.numeric(ifelse(grepl("1015-1115", r1$mean_annual_rainfall_mm), (1015+1115)/2, 
		                  ifelse(grepl("1300-1500", r1$mean_annual_rainfall_mm),  (1300+1500)/2, r1$mean_annual_rainfall_mm))),
		temp = r1$mean_annual_temp_c,
		tmin = r1$mean_annual_min_temp_c,
		tmax = r1$mean_annual_max_temp_c,
		soil_type = r1$soiltype_reported,
		soil_depth = trimws(gsub("0-|cm$|0–", "", r1$soil_depth_cm_range)),
		soil_pH =  ifelse(grepl("H2O|water|Buckman pH meter", r1$ph_method), r1$soil_ph, NA),
		soil_pH_KCl = ifelse(grepl("KCl", r1$ph_method), r1$soil_ph, NA),
		soil_pH_CaCl2 = ifelse(grepl("CaCl2", r1$ph_method), r1$soil_ph, NA),
		#soil_pH_method = r1$ph_method,
		soil_sand = r1$sand_pct,
		soil_clay = r1$clay_pct,
		soil_texture = gsub("clayey", "clay", tolower(r1$texture_recoded)),
		soil_SOC = as.numeric(gsub(" g/ha", NA, r1$soc_pct)),
		soil_SOM = as.numeric(r1$som_pct),
		soil_N_total = as.numeric(r1$total_n_pct)*10000,
		soil_P = as.numeric(gsub("Medium", NA, trimws(gsub("kg/ha|kg ha", NA, r1$soil_available_p_mg_kg)))),
		soil_P_method = r1$available_p_method,
		soil_K = as.numeric(gsub("Medium|kg/ha| kg/ha| kg/ha \\(K2O\\)|kg ha| kg ha| K2O| kg ha", NA, r1$available_k_mg_kg)),
		soil_K_exch= r1$exchangeable_k_mg_kg/390,# com/kg
		#r1$exchangeable_k_method,
		soil_CEC = r1$cec_cmolc_kg,
		soil_bd	 = as.numeric(gsub(" g/m3", "", r1$soil_bulk_density_g_cm3)),
		soil_Zn = as.numeric(gsub(" mg/ha", "", r1$soil_zn_mg_kg)),
		soil_Cu = r1$soil_cu_mg_kg,
		soil_S = as.numeric(gsub(" kg/ha", "", r1$soil_s_mg_kg)),
		soil_B = r1$soil_b_mg_kg,
		#soil_Se = r1$soil_se_mg_kg,
		soil_Ca_exch = r1$soil_exchangeable_ca_mg_kg/200,
		soil_Mg_exch = r1$soil_exchangeable_mg_mg_kg/120,
		soil_Mn_exch = r1$soil_mn_mg_kg/275,
		#soil_Mo = r1$soil_mo_mg_kg,
		soil_Na_exch = r1$soil_exchangeable_na_mg_kg/230,
		irrigated = grepl("Irrigated", r1$production_environment),
		#r1$rice_species,
		planting_method =  tolower(r1$crop_establishment),
		#hill_density = r1$hill_density_hills_m2,
		variety = r1$rice_cultivar_name,
		#r1$landrace_improved,
		variety_type = r1$variety_type,
		on_farm = as.logical(ifelse(!is.na(r1$experiment_type), grepl("On-farm", r1$experiment_type), r1$experiment_type)),
		treatment_trt = r1$treatment_description,
		treatment_crt = "control",
		#treatment_code = r1$old_trt_code,
		rep = as.integer(r1$replications_n),
		N_fertilizer_trt = r1$n_application_rate_kg_ha,
		N_fertilizer_crt = 0,
		P_fertilizer_trt = as.numeric(gsub("50\\?|All", NA, r1$p_application_rate_kg_ha)),
		P_fertilizer_crt = 0,
		K_fertilizer_trt = r1$k_application_rate_kg_ha,
		K_fertilizer_crt = 0,
		Zn_fertilizer_trt = r1$zn_application_rate_soil_kg_ha,
		Zn_fertilizer_crt = 0,
		Fe_fertilizer_trt = r1$fe_application_rate_soil_kg_ha,
		Fe_fertilizer_crt = 0,
		Mn_fertilizer_trt = r1$mn_application_rate_soil_kg_ha,
		Mn_fertilizer_crt = 0,
		residue_prevcrop_used = as.logical(ifelse(!is.na(r1$crop_residue_returned), grepl("yes", tolower(r1$crop_residue_returned)), r1$crop_residue_returned)) ,
		OM_used = as.logical(ifelse(!is.na(r1$farmyard_manure_applied), grepl("yes", tolower(r1$farmyard_manure_applied)), r1$farmyard_manure_applied)) ,
		OM_amount = as.numeric(r1$fym_rate_kg_ha),
		OM_type = ifelse(grepl("yes", tolower(r1$farmyard_manure_applied)),"farmyard manure", "none"),
		#r1$inoculation,
		land_prep_method = gsub("zero-tillage", "none", tolower(r1$tillage)),
		fwy_total_trt = r1$total_aboveground_biomass_t_ha,
		fwy_total_crt = NA,
		harvest_index_trt = r1$harvest_index,
		harvest_index_crt = r1$harvest_index_control,
		seed_weight_trt = r1$test_weight_1000_seed_g,
		seed_weight_crt = r1$test_weight_control_g,
		yield_trt = r1$grain_yield_t_ha*1000,
		yield_crt = r1$grain_yield_control_t_ha*1000,
		#r1$unit_grain_zn,
		grain_Zn_trt = r1$value_grain_zn/1000,
		grain_Zn_crt = r1$grain_zn_control/1000,
		#r1$unit_grain_fe,
		grain_fe_trt = r1$value_grain_fe/1000,
		grain_fe_crt = r1$grain_fe_control/1000,
		#r1$unit_grain_protein,
		grain_protein_trt = r1$value_protein/1000,
		grain_protein_crt = r1$protein_control/1000,
		#r1$unit_grain_p,
		grain_P_trt = r1$value_grain_p/1000,
		grain_P_crt = NA,
		#r1$unit_grain_calcium,
		grain_Cd_crt = r1$cadmium_control/1000,
		grain_Cd_trt = r1$value_grain_cadmium/1000,
		#r1$unit_grain_magnesium,
		grain_Mn_trt = r1$value_grain_manganese/1000,
		grain_Mn_crt = NA,
		#r1$unit_grain_selenium,
		grain_Se_trt = r1$value_grain_selenium/1000,
		grain_Se_crt = NA,
		grain_B_trt = as.numeric(gsub("mg/kg", NA, r1$value_grain_boron))/1000,
		grain_B_crt = NA,
		grain_Cu_trt = r1$value_grain_copper,
		grain_Cu_crt = NA, 
		grain_K_trt = r1$value_grain_k/1000,
		grain_K_crt = NA,
		grain_N_trt = r1$value_grain_n/1000,
		grain_N_crt = NA,
		grain_S_trt = r1$value_grain_sulphur/1000,
		grain_S_crt = NA,
		is_survey = FALSE, 
		crop = "rice", 
		yield_part = "grain", 
		yield_moisture = NA_real_, 
		geo_from_source = TRUE,
		yield_isfresh = NA
	)

	d <- reshape(d1, varying = list(c("fwy_total_trt", "fwy_total_crt"), c("harvest_index_trt", "harvest_index_crt"), c("seed_weight_trt", "seed_weight_crt"),
	                                c("yield_trt", "yield_crt"), c("grain_Zn_trt", "grain_Zn_crt"), c("grain_fe_trt", "grain_fe_crt"), c("grain_protein_trt", "grain_protein_crt"),
	                                c("grain_P_trt", "grain_P_crt"), c("grain_Cd_trt", "grain_Cd_crt"), c("grain_Mn_trt", "grain_Mn_crt"),c("grain_Se_trt", "grain_Se_crt"),
	                                c("grain_B_trt", "grain_B_crt"), c("grain_Cu_trt", "grain_Cu_crt"), c("grain_K_trt", "grain_K_crt"),
	                                c("grain_N_trt", "grain_N_crt"), c("grain_S_trt", "grain_S_crt"), c("treatment_trt", "treatment_crt"),
	                                c("N_fertilizer_trt", "N_fertilizer_crt"), c("P_fertilizer_trt", "P_fertilizer_crt"), c("K_fertilizer_trt", "K_fertilizer_crt"), 
	                                c("Zn_fertilizer_trt", "Zn_fertilizer_crt"), c("Fe_fertilizer_trt", "Fe_fertilizer_crt"), c("Mn_fertilizer_trt", "Mn_fertilizer_crt")),
	             v.names = c("fwy_total", "harvest_index", "seed_weight", "yield", "grain_Zn", "grain_Fe", "grain_protein", "grain_P", "grain_Cd", "grain_Mn", "grain_Se", "grain_B", "grain_Cu", "grain_K", "grain_N", "grain_S", "treatment", "N_fertilizer",
	                         "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "Fe_fertilizer", "Mn_fertilizer"),
	             direction = "long")
	
	d$time <- d$id <- NULL
	### Fixing country names
	P <- carobiner::fix_name(d$country)
	P <- gsub("Côte d’Ivoire", "Côte d'Ivoire", P)
	P <- gsub("Lao PDR", "Laos", P)
	P <- gsub("Phillipines", "Philippines", P)
	P <- gsub("The Philippines", "Philippines", P)
	P <- gsub("USA, Bangladesh, UK", NA, P)
	P <- gsub("USA", "United States", P)
	d$country <- P
	
	### 
	d$season <- ifelse(grepl("wet|cool|boro", d$season), "wet", 
	            ifelse(grepl("dry|hot|post-rainy", d$season), "dry", d$season))
	
	d$season <- gsub("winter \\(rabi\\)|rabi\\?", "rabi", d$season)
	
	d$soil_texture <- gsub("loamy", "loam", d$soil_texture)
	d$soil_texture <- gsub("sandy", "sand", d$soil_texture)
	d$soil_texture <- gsub("na", NA, d$soil_texture)
	
	### Fixing planting method 
	
	P <- carobiner::fix_name(d$planting_method)
	P <- gsub("^direct seeded$|direct-seed|direct seeded rice|direct sowing|^sowing$", "direct seeding", P)
	P <- gsub("aerobic unpuddled\\?", "unknown", P)
	P <- gsub("dibbled", "dibbling", P)
	P <- gsub("drill sowing", "drilling", P)
	P <- gsub("drilled sowing", "drilling", P)
	P <- gsub("puddled-transplanted", "transplanted", P)
	P <- gsub("seed drilling", "drilling", P)
	P <- gsub("tpr, dsr, ar", "transplanted", P)
	d$planting_method <- P
	d$soil_depth <- as.numeric(ifelse(grepl("30", d$soil_depth), 30, d$soil_depth))
	
	
	### Fixing lon and lat error 
	
	geo <- data.frame(
	  country= c("Bangladesh","Bangladesh", "Pakistan", "Pakistan", "Cameroon", "Cameroon", "Iran", rep("india",2), rep("Malawi", 10)),
	  location = c("Chandbill", "Hatikata", "Sheikhupura", "University of Agriculture, Faisalabad", "Wakwa","Yagoua", "Mazandaran province", "Eastern Himalaya","University of Calcutta", "Bandawe Tembo village", "Bereu", "Bridge to Kasuwa", "Chembelwa village", "Chizongwe", "Kasongo", "Mapelela", "Nkhapa village", "Puertete village", "Sani village"),
	  lon = c(88.6678, 88.831, 73.998, 73.075, 13.626, 15.236, 51.919, 91.732, 88.362, 34.149, 34.849, 33.985, 34.843, 34.324, 34.2636, 34.916, 34.294, NA, 34.302),
	  lat = c(23.748, 23.662, 31.7141, 31.429, 7.170, 10.347, 36.332,26.123, 22.575, -11.874, -16.148, -11.422, -14.023, -13.160, -12.872, -16.1015, -11.608, NA, -13.020),
	  geo_from = FALSE
	)
	
	
	d <- merge(d, geo, by= c("country", "location"), all.x = TRUE)
	d$longitude[!is.na(d$lon)] <- d$lon[!is.na(d$lon)]
	d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
	d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
	d$lon <- d$lat <- d$geo_from <-  NULL
	
	i <- grepl("Dakahlia Governorate", d$location) & grepl("Egypt", d$country)
	d$longitude[i] <- 31.573
	d$latitude[i] <- 31.179
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Puducherry Research farm", d$location) & grepl("India", d$country)
	d$longitude[i] <- 79.834
	d$latitude[i] <- 11.953
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Ankazomiriotra, ANK2", d$location) & grepl("Madagascar", d$country)
	d$longitude[i] <- 46.516
	d$latitude[i] <- -19.663
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Jerlun, Kangkong", d$location) & grepl("Malaysia", d$country)
	d$longitude[i] <- 100.264
	d$latitude[i] <- 6.2707
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Doho rice irrigation scheme", d$location) & grepl("Uganda", d$country)
	d$longitude[i] <- 30.122
	d$latitude[i] <- 0.264
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("King Faisal University", d$location) & grepl("Saudi Arabia", d$country)
	d$longitude[i] <- 49.599
	d$latitude[i] <- 25.3407
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("CMU", d$location) & grepl("Thailand", d$country)
	d$longitude[i] <- 98.954
	d$latitude[i] <- 18.808
	d$geo_from_source[i] <- FALSE
	
	### drop rows with unknown location
	d <- d[!is.na(d$latitude),]
	### missing country
	d <- d[!is.na(d$country),]
	d$planting_date[which(d$planting_date>2026)] <- NA
	
	d <- d[!is.na(d$yield),]
	
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}

