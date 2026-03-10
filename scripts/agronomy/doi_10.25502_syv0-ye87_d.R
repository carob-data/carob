# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Bean grain yield and quality as influenced by different agronomic and soil health practices

This dataset supports a global meta-analysis that investigates how agronomic practices and soil health characteristics influence bean grain yield and quality traits. Drawing on 13,684 data points compiled from 366 studies conducted across 43 countries, the analysis quantifies the probability of bean production meeting both agronomic performance and nutritional benchmarks. The dataset enables assessment of management interventions, including macro and micronutrient fertilizer applications, organic amendments, liming, and irrigation, as well as genetic and varietal differences under varying soil and climate conditions. Key biophysical drivers are captured alongside climatic variables such as rainfall, aridity, and temperature. Yield and quality responses are systematically documented, together with management practices like tillage, crop spacing, cropping system, and residue or manure incorporation. This rich dataset provides a foundation for evaluating the combined effects of soil fertility status, nutrient applications, and agronomic practices on bean productivity and nutritional quality. It also offers critical evidence for developing strategies in agronomic biofortification, sustainable intensification, and soil health management across diverse agroecological zones.
"

	uri <- "doi:10.25502/syv0-ye87/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "CIAT; IITA; DPBBM-AABU",# Department of Plant Biology and Biodiversity Management, Addis Ababa University
		publication = NA,
		project = NA,
		carob_date = "2026-03-05",
		design = NA,
		data_type = "compilation",
		treatment_vars = "variety;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "data_dictionary.csv"]
	f2 <- ff[basename(ff) == "bean-grain-yield-and-quality.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2, fileEncoding = "latin1", na= c(""))
	r2$Soil_Ca <- gsub(",", "", r2$Soil_Ca)

	d1 <- data.frame(
		trial_id = paste(r2$Trial_type, r2$Code, sep = "-"),
		reference = trimws(r2$Study),
		#Title = trimws(r2$Response_to_Soil_Acidity_of_Common_Bean_Genotypes),
		#Journal = r2$Journal_name,
		#r2$Volume,
		#publication = trimws(r2$DOI),
		country = r2$Country,
		#r2$Continent,
		location = trimws(r2$TrialSite),
		#trial_id = r2$Trial_type,
		planting_date = gsub(" \\(.*\\)", "", ifelse(nchar(carobiner::eng_months_to_nr(trimws(r2$Month_season_start)))==1, paste(substr(r2$Yr_experiment, 1, 4), carobiner::eng_months_to_nr(trimws(r2$Month_season_start)), sep = "-0"),  paste( substr(r2$Yr_experiment, 1, 4),  carobiner::eng_months_to_nr(trimws(r2$Month_season_start)), sep = "-"))),
		latitude = r2$Latitude,
		longitude = r2$Longitude,
		soil_type = ifelse(is.na(r2$WRG), r2$SoilType, r2$WRG) ,
		soil_P = r2$OlsenP,
		soil_P1 = r2$Avail_P,
		soil_P_method = trimws(r2$Avail_P_Method),
		soil_pH = ifelse(grepl("H2O", r2$pH_Method) & is.na(r2$pH_H2O),r2$Soil_pH , r2$pH_H2O) ,
		soil_pH_KCl = ifelse(grepl("KCl|KCL", r2$pH_Method),r2$Soil_pH, NA ),
		soil_pH_CaCl2 = ifelse(grepl("CaCl2", r2$pH_Method),r2$Soil_pH, NA ), 
		soil_sand = r2$sand,
		soil_clay = r2$clay,
		soil_N = r2$Tnitrogen,
		soil_SOC = r2$SOC,
		soil_SOM = r2$SOM,
		soil_CEC = r2$CEC,
		tmin = r2$TMin,
		tmax = r2$TMax,
		temp = r2$TMean,
		rain = r2$MAP,
		elevation = r2$Altitude,
		soil_texture = gsub("clayey", "clay", tolower(r2$Texture_recoded)),
		depth_bottom = suppressWarnings(as.numeric(substr(r2$Soil_depth, 3, 4))),
		depth_top = suppressWarnings(as.numeric(substr(r2$Soil_depth, 1, 1))),
		
		#soil_sand = r2$Sand,
		#soil_clay = r2$Clay,
		#soil_SOC = r2$SOC.1,
		#soil_N = r2$Tot_N,
		soil_K = r2$Soil_K,
		soil_Zn = r2$Soil_Zn,
		soil_Fe = r2$Soil_Fe,
		soil_B = r2$Soil_B,
		soil_Ca = as.numeric(r2$Soil_Ca),
		soil_Cu = as.numeric(r2$Soil_Cu),
		soil_Mg = r2$Soil_Mg,
		soil_Mn = r2$Soil_Mn,
		soil_Mo = r2$Soil_Mo,
		soil_Na= r2$Soil_Na,
		soil_S = r2$Soil_S,
		irrigated_trt = trimws(r2$Irrigation_Trt),
		irrigated_cnt = trimws(r2$Irrigation_Cnt),
		intercrops = tolower(r2$Crop_in_intercrop),
		#r2$Crop_stage_for_One.time,
		N_splits = as.integer(ifelse(is.na(r2$Firstsplit_N_Application), r2$X2ndsplit_N_Application, r2$Firstsplit_N_Application)),
		N_fertilizer_trt = r2$N_Applied_Trt,
		N_fertilizer_cnt = 0,
		P_fertilizer_trt = r2$Nominal_P_Applied_Trt,
		P_fertilizer_cnt = 0,
		K_fertilizer_trt = r2$K_Applied_Trt,
		K_fertilizer_cnt = 0,
		Zn_fertilizer_trt = r2$Zn_Applied_Trt,
		Zn_fertilizer_cnt = r2$Zn_Applied_Cnt,
		Fe_fertilizer_trt = r2$Fe_Applied_Trt,
		Fe_fertilizer_cnt = r2$Fe_Applied_Cnt,
		#r2$Other_micronutrient_applied_Trt,
		#r2$Other_micronutrient_applied_Cnt,
		Cu_fertilizer_trt = as.numeric(r2$Cu_Applied_Trt),
		Cu_fertilizer_cnt = as.numeric(r2$Cu_Applied_Cnt),
		Se_fertilizer_trt = r2$Se_Applied_Trt,
		Se_fertilizer_cnt = r2$Se_Applied_Cnt,
		S_fertilizer_trt = r2$S_Applied_Trt,
		S_fertilizer_cnt = r2$S_Applied_Cnt,
		B_fertilizer_trt = r2$B_Applied_Trt,
		B_fertilizer_cnt = r2$B_Applied_Cnt,
		Ca_fertilizer_trt = r2$Ca_Applied_Trt,
		Ca_fertilizer_cnt = r2$Ca_Applied_Cnt,
		Mg_fertilizer_trt = r2$Mg_Applied_Trt,
		Mg_fertilizer_cnt = r2$Mg_Applied_Cnt,
		Mn_fertilizer_trt = r2$Mn_Applied_Trt,
		Mn_fertilizer_cnt = r2$Mn_Applied_Cnt,
		Mo_fertilizer_trt = r2$Mo_Applied_Trt,
		Mo_fertilizer_cnt = r2$Mo_Applied_Cnt,
		fertilizer_type = r2$P_source,
		#r2$Residue_Trt,
		#r2$Residue_Cnt,
		#r2$Maturity_type,
		variety = trimws(r2$Cultivar_name),
		variety_traits = trimws(r2$Seed_colour),
		variety_type = r2$Variety_type,
		#variety_trait = r2$Cultivar,
		land_prep_method = r2$Tillage,
		#r2$Liming,
		weeding_method = trimws(r2$Weed_control),
		row_spacing = r2$Row_spacing,
		plant_spacing = r2$Plant_spacing,
		crop_system = r2$Cropping_system,
		#r2$Organic_matter_added,
		OM_type_cnt = tolower(ifelse(grepl("Yes", r2$FYM_Cnt),r2$Organic_matter_type, "none")),
		OM_type_trt = tolower(ifelse(grepl("Yes", r2$FYM_Trt),r2$Organic_matter_type, "none")),
		OM_used = !grepl("Without", r2$Manure),
		inoculated = grepl("Inoculated", r2$Inoculation) ,
		treatment_trt = trimws(r2$TRT_for_meta.analysis),
		treatment_cnt = "control",
		yield_trt = r2$GrainYld_Trt,
		yield_cnt = ifelse(is.na(r2$GrainYld_noinput_control), r2$GrainYld_Recommfert_control, r2$GrainYld_noinput_control),
		harvest_index = r2$Harvest_index,
		proteins_trt = r2$Proteins_Trt,
		proteins_cnt = r2$Proteins_Cnt,
		grain_N_trt = r2$GrainN_Trt*10,
		grain_N_cnt = r2$GrainN_Cnt*10,
		starch_trt = r2$Starch_Trt,
		starch_cnt = r2$Starch_Cnt,
		oil_trt = r2$Oil_Trt,
		oil_cnt = r2$Oil_Cnt,
		grain_P_trt = r2$Grain_P_Trt_ /1000,
		grain_P_cnt = r2$Grain_P_Cnt /1000,
		grain_Zn_trt = r2$GrainZn_Trt_ /1000,
		grain_Zn_cnt = r2$GrainZn_Cnt_ /1000,
		grain_Fe_trt = r2$Grain_Fe_Trt /1000,
		grain_Fe_cnt = r2$Grain_Fe_Cnt /1000,
		grain_B_trt = r2$Grain_B_Trt /1000,
		grain_B_cnt = r2$GrainB_Cnt /1000,
		grain_Mn_trt = r2$GrainMn_Trt /1000,
		grain_Mn_cnt = r2$GrainMn_Cnt /1000,
		grain_Cu_trt = r2$GrainCu_Trt /1000,
		grain_Cu_cnt = r2$GrainCu_Cnt /1000,
		grain_S_trt = r2$GrainS_Trt /1000,
		grain_S_cnt = r2$GrainS_Cnt /1000,
		grain_Mo_trt = r2$GrainMo_Trt /1000, ## from ppm to mg/g
		grain_Mo_cnt = r2$GrainMo_Cnt /1000,
		grain_se_trt = r2$GrainSe_Trt /1000,
		grain_se_cnt = r2$GrainSe_Cnt /1000,
		rep = r2$Replications.Observation,
		on_farm = grepl("On-farm", r2$Trial_type), 
		is_survey = FALSE, 
		crop = "common bean", 
		yield_part = "grain", 
		yield_moisture = as.numeric(NA), 
		geo_from_source = TRUE
	)
	
	### reshape the data 
	
	d <- reshape(d1, varying = list(c("N_fertilizer_trt", "N_fertilizer_cnt"), c("P_fertilizer_trt", "P_fertilizer_cnt"),
	                                c("K_fertilizer_trt", "K_fertilizer_cnt"), c("Zn_fertilizer_trt", "Zn_fertilizer_cnt"),
	                                c("Fe_fertilizer_trt", "Fe_fertilizer_cnt"), c("Cu_fertilizer_trt", "Cu_fertilizer_cnt"),
	                                c("Se_fertilizer_trt", "Se_fertilizer_cnt"), c("S_fertilizer_trt", "S_fertilizer_cnt"),
	                                c("B_fertilizer_trt", "B_fertilizer_cnt"), c("Mg_fertilizer_trt", "Mg_fertilizer_cnt"),
	                                c("Ca_fertilizer_trt", "Ca_fertilizer_cnt"), c("Mn_fertilizer_trt", "Mn_fertilizer_cnt"),
	                                c("Mo_fertilizer_trt", "Mo_fertilizer_cnt"), c("OM_type_trt", "OM_type_cnt"), c("irrigated_trt", "irrigated_cnt"),
	                                c("treatment_trt", "treatment_cnt"), c("yield_trt", "yield_cnt"), c("proteins_trt", "proteins_cnt"),
	                                c("grain_N_trt","grain_N_cnt"), c("starch_trt","starch_cnt"), c("oil_trt", "oil_cnt"),
	                                c("grain_P_trt", "grain_P_cnt"), c("grain_Zn_trt", "grain_Zn_cnt"), c("grain_Fe_trt","grain_Fe_cnt"),
	                                c("grain_B_trt", "grain_B_cnt"), c("grain_Mn_trt", "grain_Mn_cnt"), c("grain_Cu_trt", "grain_Cu_cnt"),
	                                c("grain_S_trt", "grain_S_cnt"), c("grain_Mo_trt","grain_Mo_cnt"), c("grain_se_trt", "grain_se_cnt")),
	             v.names = c("N_fertilizer","P_fertilizer", "K_fertilizer","Zn_fertilizer", "Fe_fertilizer", "Cu_fertilizer", "Se_fertilizer",
	                         "S_fertilizer", "B_fertilizer", "Mg_fertilizer", "Ca_fertilizer", "Mn_fertilizer", "Mo_fertilizer", "OM_type","irrigated", "treatment",
	                         "yield", "grain_protein", "grain_N", "dmy_residue", "oil_rate", "grain_P", "grain_Zn", "grain_Fe", "grain_B", "grain_Mn", "grain_Cu", "grain_S",
	                         "grain_Mo", "grain_Se"),
	             direction = "long",
	             times = c(1,2))
	
	d$irrigated <- ifelse(grepl("Yes", d$irrigated), TRUE, 
	               ifelse(grepl("No|NO", d$irrigated), FALSE, NA))
	d$time <- d$id <- NULL
	d$yield <- d$yield*1000 ## kg/ha
### Fixing intercrops names	

P <- carobiner::fix_name(d$intercrops)	
P <- gsub("beans|castor bean", "common bean", P)	
P <- gsub("fennel + dragonhead", "fennel", P)	
P <- gsub("potato \\(b53 variety\\)|potato \\(annette variety\\)", "potato", P)	
P <- gsub("maize and cassava", "maize;cassava", P)	
P <- gsub("monocrop|ntercrop", "none", P)	
P <- gsub("", NA, P)	
d$intercrops <- P 

### Fixing country names 
P <- carobiner::fix_name(d$country)
P <- gsub("Côte d\u0092Ivoire", "Côte d'Ivoire", P)
P <- gsub("DRC", "Democratic Republic of the Congo", P)
P <- gsub("México", "Mexico", P)
P <- gsub("USA", "United States", P)
d$country <- P

### Fixing fertilizer type 

P <- carobiner::fix_name(d$fertilizer_type)
P <- gsub("Triple superphosphate|Triple Superphosphate|Tripple superphosphate", "TSP", P)
P <- gsub("Diammonium phosphate|Di- ammonium phosphate\\)|Diammonium Phosphate|Diammonium phosphate |Diamonium phosphate", "DAP", P)
P <- gsub("Nitrogen Phosphate Fertilizer", "NPK", P)
P <- gsub("simple superphosphate|Simple superphosphate|Single superphosphate|Single super phosphate|Single Super phosphate|Sinle Super Phosphate|Single Super Phosphate|Super phosphate", "SSP", P)
P <- gsub("Nitrogen, Phosphorus and Potassium |Nitrogen,Phosphorous,Potassium|Nitrogen, Phosphorous and Potassium", "NPK", P)
P <- gsub("Ammonium Phosphate|Monoammonium phosphate", "MAP", P)
P <- gsub("P-K", "NPK", P)
P <- gsub("NPK \\(4:25:15\\)|NPK \\(0:30:10\\)|NPK \\(17-17-17\\)|7 N: 14 P: 7 K", "NPK", P)
P <- gsub("Diammonium phosphate +Nitrogen, Phosphorous and Potassium|Diammonium phosphate +Nitrogen", "DAP;NPK", P)
P <- gsub("Calcium Superphosphate", "SSP", P)
P <- gsub("Single Superphosphate|Superphosphate", "SSP", P)
P <- gsub("NPS blend \\(19%N, 38%P2O5\\)", "NPS", P)
P <- gsub("calcium dihydrogen phosphate", "unknown", P)
P <- gsub("Mavuno fertilizer|Fertilizer", "unknown", P)
P <- gsub("IAPAR \\(2003\\)", "IAPAR", P)
P <- gsub("121 NPS Kg ha|200 NPS|201 NPS|204 NPS|203 NPS|202 NPS", "NPS", P)
P <- gsub("NPSB blend|^NPSB$", "NPS", P)
P <- gsub("TSP \\(46% P2O5\\)", "TSP", P)
P <- gsub("Nitrogen, Phosphorus and Potassium", "NPK", P)
P <- gsub("DAP ", "DAP", P)
P <- gsub("NSPB", "NPS", P)
d$fertilizer_type <- P
d$fertilizer_type[grepl("+Nitrogen|+NPK", d$fertilizer_type)] <- "DAP;NPK" 
d$fertilizer_type[which(nchar(d$fertilizer_type)==4)] <- "DAP"
### Fixing OM type

P <- carobiner::fix_name(d$OM_type)
P <- gsub("chicken manure|chicken manure ", "animal dung", P)
P <- gsub("animal manure", "animal dung", P)
P <- gsub("cattle manure|cattle manure ", "cattle dung", P)
P <- gsub("farm yard manure", "farmyard manure", P)
P <- gsub("mushroom compost", "compost", P)
P <- gsub("^manure$", "farmyard manure", P)
P <- gsub("organic manure", "unknown", P)
P <- gsub("swine manure |swine manure", "animal dung", P)
P <- gsub("tithonia biomass", "unknown", P)
P <- gsub("^no$", "none", P)
d$OM_type <- P

### fixing soil texture 

P <- carobiner::fix_name(d$land_prep_method) 
P <- gsub("Tilled", "tillage", P)
P <- gsub("Tied-ridge", "ridge tillage;tied ridges", P)
P <- gsub("Unclear|Flat", "unknown", P)
P <- gsub("Notil", "none", P)
P <- gsub("Conventional", "conventional", P)
P <- gsub("Reduced", "reduced tillage", P)
d$land_prep_method <- P 

## Fixing date 
d$planting_date <- ifelse(grepl("^-", d$planting_date), NA, d$planting_date)
d$planting_date <- gsub("2017-11/3", "2017-11", d$planting_date)
d$planting_date <- gsub("2017-5-6", "2017-05", d$planting_date)
d$planting_date <- gsub("-$", "", d$planting_date)
d$planting_date <- gsub("2011-5", "2011-05", d$planting_date)
d$planting_date <- gsub("2011-9", "2011-09", d$planting_date)
d$planting_date <- gsub("NA-08|NA-05|NA-02|NA-03|NA-07", NA, d$planting_date)

#### Fixing error in lon and alt coordinate 
i <- grepl("Costa Rica", d$country)& grepl("San Juan Sur", d$location)
d$latitude[i] <- 10.09916
d$longitude[i] <- -84.2424
i <- grepl("Bushumba", d$location)
d$country[i] <- "Tanzania"
i <- grepl("Democratic Republic of the Congo", d$country) & grepl("Kamanyola", d$location)
d$latitude[i] <- -2.74026
d$longitude[i] <- 29.00477

i <- grepl("Iran", d$country) & grepl("Shahrekord", d$location)
d$latitude[i] <- 32.3292
d$longitude[i] <- 50.864

i <- grepl("Turkey", d$country) & grepl("Erzurum", d$location)
d$latitude[i] <- 39.9063
d$longitude[i] <- 41.2690
i <- grepl("Rwanda", d$country) & grepl("Nyagatare", d$location)
d$latitude[i] <- -abs(d$latitude[i])

i <- grepl("Afghanistan", d$country) & grepl("Kabul", d$location)
d$latitude[i] <- 34.571358
d$longitude[i] <- 69.20313

i <- grepl("Brazil", d$country) & grepl("Anapolis", d$location)
d$latitude[i] <- -16.329
d$longitude[i] <- -48.9566

i <- grepl("Brazil", d$country) & grepl("Dourados", d$location)
d$latitude[i] <- -22.22809
d$longitude[i] <- -54.8125

i <- grepl("Brazil", d$country) & grepl("Universidade Federal de Santa Maria", d$location)
d$latitude[i] <- -29.72069
d$longitude[i] <- -53.71474

i <- grepl("Ghana", d$country) & grepl("Fumesua", d$location)
d$longitude[i] <- - abs(d$longitude[i])

i <- grepl("Mexico", d$country) & grepl("Bravo experimental site in northern Tamaulipas", d$location)
d$longitude[i] <- - abs(d$longitude[i])

i <- grepl("Côte d'Ivoire", d$country) & grepl("Crop research Station in Bouaké", d$location)
d$longitude[i] <- - abs(d$longitude[i])

i <- grepl("South Africa", d$country) & grepl("North West province", d$location)
d$latitude[i] <- -27.32107
d$longitude[i] <- 24.6336

i <- grepl("Democratic Republic of the Congo", d$country) & grepl("Rutshuru", d$location)
d$latitude[i] <- -1.185837
d$longitude[i] <- 29.44730

i <- grepl("Tanzania", d$country) & grepl("Uyole", d$location)
d$latitude[i] <- - abs(d$latitude[i])

i <- grepl("Malawi", d$country) & grepl("Chitedze|Linthipe", d$location)
d$latitude[i] <- - abs(d$latitude[i])

i <- grepl("Malawi", d$country) & grepl("Lilongwe", d$location)
d$latitude[i] <- -13.98767
d$longitude[i] <- 33.7663

i <- grepl("Ethiopia", d$country) 
d$latitude[i] <- abs(d$latitude[i])
d$longitude[i] <- abs(d$longitude[i])

i <- grepl("Uganda", d$country) & grepl("Kawanda", d$location)
d$latitude[i] <- 0.4224
d$longitude[i] <- 32.5415

i <- grepl("Zimbabwe", d$country) & grepl("Harare", d$location)
d$latitude[i] <- -17.8257
d$longitude[i] <- 31.03898

i <- grepl("Ethiopia", d$country) & grepl("Hirna", d$location)
d$latitude[i] <- 9.21661
d$longitude[i] <- 41.10101

i <- grepl("Ethiopia", d$country) & grepl("Bako", d$location)
d$latitude[i] <- 9.26491
d$longitude[i] <- 37.04517

i <- grepl("Ethiopia", d$country) & grepl("Pawe", d$location)
d$latitude[i] <- 11.33775
d$longitude[i] <- 36.33257

i <- grepl("Ethiopia", d$country) & grepl("Aris Negele", d$location)
d$latitude[i] <- 7.35705
d$longitude[i] <- 38.6696

i <- grepl("Ethiopia", d$country) & grepl("Boshe", d$location)
d$latitude[i] <- NA ### unknown location 
d$longitude[i] <- NA

i <- grepl("Malawi", d$country) & grepl("Chitedze", d$location)
d$latitude[i] <- -13.9815 
d$longitude[i] <- 33.637

i <- grepl("Tanzania", d$country) & grepl("Uyole", d$location)
d$latitude[i] <- -8.896
d$longitude[i] <- 33.544


#### record soil P in long format

d$record_id <- as.integer(1:nrow(d)) 
soil_P_var <- d[, grepl("soil_P|record_id", names(d))] 
soil_P_var$soil_P1_method <- "Olsen"
soilP <- reshape(soil_P_var, varying = list(c("soil_P","soil_P1"), c("soil_P1_method", "soil_P_method")), v.names = c("soil_P", "soil_P_method"),
                 direction = "long") 
soilP <- soilP[!is.na(soilP$soil_P),]
soilP$id <- soilP$time <- NULL

d <- d[, !grepl("soil_P", names(d))]

### remove duplicate rows

d <- unique(d)

carobiner::write_files(path, meta, d, long = soilP)

}


