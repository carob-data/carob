# R script for "carob"


carob_script <- function(path) {
  
"Information is scarce for maize (Zea mays L.) response to nutrient application for many production areas in tropical Africa. Research was conducted to determine macronutrient response functions and to diagnose Mg–S–Zn–B deficiencies. Site–year × N-rate interactions within countries often accounted for little variation in yield relative to the N-rate effect. Country mean grain yield responses to N-rate were curvilinear to plateau, but linear in Malawi. Although mean yields differed, the response to N was similar for Kenya, Tanzania, and Zambia with a mean yield increase of 0.94 Mg ha–1 due to 50 kg ha–1 N compared with 1.59 Mg ha–1 for Malawi and Rwanda. Response to N was related to yield with no fertilizer applied (r = 0.40). Only Rwanda had mean responses to P and K with respective yield increases of 0.99 and 0.22 Mg ha–1 due to 15 kg ha–1. Application of Mg–S–Zn–B caused a mean yield increase of 0.73 Mg ha–1 in Rwanda but had no effect in other countries. Application of affordable fertilizer to twice as much land at 50% compared with 100% of the economically optimum rate results in mean gains of 50% for production and agronomic efficiency and 72% for profit/cost ratio. Soil test results were not related to response to applied nutrients but historical yield appears to be weakly predictive of N response. The determined country-level P and K response functions can be widely applied, except for Kenya, in consideration of other available information. The response to Mg–S–Zn–B in Rwanda needs further investigation.
	
Wortmann, C., C. Senkoro, A.R. Cyamweshi, C. Kibunja, D. Nkonde, M. Munthali, P. Nalivata, L.N. Nabahungu, K. Kaizzi. 2018. Maize-nutrient response functions for Eastern and Southern Africa. Agron. J. 110:2070-2079. doi:10.2134/agronj2018.04.0268

Also see: doi:10.21955/gatesopenres.1115299.1"
  

	uri <- "doi:10.5061/dryad.fg15tg2"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA, 
		project = "Optimization of Fertilizer Recommendations in Africa", 
		publication= "doi:10.2134/agronj2018.04.0268", 
		data_organization = "UNL;TARI;RAB;ZARI;CARS;LUANAR;IITA;NARL", 
		carob_contributor="Effie Ochieng';Rachel Mukami", 
		carob_date="2023-03-20", 
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;variety"
	)
	

	f <- ff[basename(ff) == "ESA Maize Fertilizer Response Data.xlsx"][1]
# should use sheet names, not numbers
	r0 <- carobiner::read.excel(f, sheet = "Kenya")
	r1 <- carobiner::read.excel(f, sheet = "Malawi")
	r2 <- carobiner::read.excel(f, sheet = "Rwanda")
	r3 <- carobiner::read.excel(f, sheet = "Tanzania")
	r4 <- carobiner::read.excel(f, sheet = "Zambia")

## Kenya	
	d0 <- data.frame(	
		country = "Kenya",
		adm3 = as.character(strsplit(r0$SY1, split = "[0-9]+")),
		rep = r0$R,
		treatment = r0$Trt,
		N_fertilizer = r0$N,
		P_fertilizer = r0$P,
		K_fertilizer = r0$K,
		variety = r0$Variety,
		yield = r0$GrainYld*1000, #converting to kg/ha
		trial_id = r0$SY1
	)
	
	d0$adm3 <- carobiner::replace_values(carobiner::fix_name(d0$adm3),
			c("Embu ATC", "EmbuKPS", "c(\"Kandara\", \"SR\")"), 
			c("Embu", "Embu", "Kandara"))
	
	#lat and lon as found in the reference
	geo <- data.frame( 
		adm3 = c("Eldoret", "Embu", "Kandara", "Kisii", "Kitale", "Machakos", "Njoro"), 
		latitude = c(0.569, -0.494, -0.465, -0.682, 1.000, -1.539, -0.344), 
		longitude = c(35.308, 37.452, 37.719, 34.766, 34.992, 37.252, 35.946), 
		geo_from_source = FALSE,
		elevation = c(2150, 1500, 1125, 1450, 1863, 1585, 2170), 
		soil_pH = c(5.0, 6.2, 5.6, 6.0, 5.4, 6.2, 5.7), 
		soil_P_total = c(22.9, 15.6, 64.9, 144.1, 15.2, 14.6, 7.6), 
		soil_K = c(569, 261, 281, 421, 239, 314, 768)
	)
		
	d0 <- merge(d0, geo, by = "adm3", all.x = TRUE)

	# dates extracted from the reference
	dd0 <- data.frame(
		trial_id = c("Eldoret14", "Embu ATC15", "EmbuKPS15", "Kandara14", "Kandara15SR", "Kisii14", "Kitale15", "Machakos", "Njoro14"), 
		planting_date = c("2014-06-19", "2015-03-31", "2015-03-31", "2014-03-27", "2015-10-02", "2014-04-03", "2015-04-28", "2015-04-08", "2014-04-23"), 
		harvest_date = c("2014", "2015-07-10", "2015-07-10", "2014-07-15", "2016-01-27", "2014", "2015", "2015-07-12", "2014-11-14"))
	
	d0 <- merge(d0, dd0, by = "trial_id", all.x = TRUE)
	
## Malawi
	d1 <- data.frame(	
		country = "Malawi",
		adm1 = r1$L,
		rep = r1$r,
		N_fertilizer = r1$N,
		P_fertilizer = r1$P,
		K_fertilizer = r1$K,
		seed_weight = (r1$`100-kernel wt`)*10, # to get 1000 grain weight
		yield = r1$GrainYld*1000, # to kg/ha
		trial_id = r1$L
	)
	#Dates extracted from reference
	dd1 <- data.frame(
		trial_id = c("BDF15", "BDFS15", "SLS15"), 
		planting_date = c("2015-01-05", "2015-01-05", "2015-01-25"), 
		harvest_date = c("2015-04-25", "2015-04-25", "2015-05-12")
	)
	
	d1 <- merge(d1, dd1, by = "trial_id", all.x = TRUE)

	d1$adm1 <- gsub("BDF", "Bunda", d1$adm1)
	d1$adm1 <- gsub("SL", "Salima", d1$adm1)
	dr <- data.frame(
		adm1 = c("Bunda", "Salima"), 
		soil_pH = c(5.6, 6.4), 
		soil_P_total = c(13.3, 10.7), 
		soil_K = c(113, 156),
		latitude = c(-14.170, -14.054),
		longitude = c(33.740, 33.740),
		elevation = c(1150, 1240)
	)
		
	d1 <- merge(d1, dr, by = "adm1", all.x = TRUE)


## Rwanda
	
	d2 <- data.frame(
		country = "Rwanda",
		adm2 = carobiner::fix_name(r2$District, "title"),
		variety = r2$Variety, 
		rep = r2$Rep,
		treatment = r2$Treatment,
		N_fertilizer = r2$N,
		P_fertilizer = r2$P,
		K_fertilizer = r2$K,
		yield = r2$GrainYld*1000
	)	
	d2$trial_id = paste("RW", d2$adm2, d2$Season, sep = "_")
	d2$K_fertilizer[d2$K_fertilizer == "D"] <- 0
	d2$K_fertilizer <- as.numeric(d2$K_fertilizer)

	# the dates are extracted from the reference, with a and b differentiating seasons within years.
	d2$trial_id <- paste(d2$Season, d2$adm2)
	dd2 <- data.frame(
		trial_id = c("2014B Ngoma", "2014B Nyagatare", "2015 B Ngoma", "2015 B Nyagatare", "2015 B Musanze", "2015A Bugesera", "2015A Ngoma", "2015A Nyagatare", "2015A Burera", "2015A Musanze", "2015A Huye"), 
		planting_date = c("2014-03-09", "2013-11-03", "2015-02-27", "2015-02-24", "2015-02-10", "2014-03-10", "2014-09-26", "2014-09-22", "2014-09-18", "2014-10-09", "2015-03-04"), 
		harvest_date = c("2014-07-29", "2014-07-24", "2015-07-16", "2015-07-21", "2015-08-11", "2015-02-06", "2015-02-10", "2015-02-03", "2015-02-26", "2015-03-02", "2015-07-23")
	)
	
	d2 <- merge(d2, dd2, by = "trial_id", all.x = TRUE)
	
	#lat and lon found in the reference	
	geo <- data.frame(adm2 = c("Ngoma", "Nyagatare", "Musanze", "Bugesera", "Burera", "Huye"), 
		latitude = c(-2.227, -1.516, -1.513, -2.239, -1.491, -2.539), 
		longitude = c(30.417, 30.284, 29.597, 30.067, 29.877, 29.741), 
		elevation = c(1458, 1500, 1930, 1369, 2096, 1630), 
		soil_P_total = c(6.4, 9.5, 28.6, 11.1, NA, 6.4), 
		soil_K = c(114, 114, 225, 119, NA, 114)
	)
	
	d2 <- merge(d2, geo, by = "adm2", all.x = TRUE)
	d2 <- unique(d2)
	
# Tanzania	

	d3 <- data.frame(
		country = "Tanzania",
		adm3 = carobiner::replace_values(r3$S, c("Mav", "Daredo"), c("Mavamizi", "Dareda")),
		rep = r3$R,
		variety = r3$Variety,
		N_fertilizer = r3$N,
		P_fertilizer = r3$P,
		K_fertilizer = r3$K,
		yield = r3$GrainYld*1000
	)

	d3$N_fertilizer[d3$N_fertilizer %in% c("Diagnostic package", "Diagnostic", NA)] <- 0 
	d3$N_fertilizer <- as.numeric(d3$N_fertilizer)
	d3$P_fertilizer[d3$P_fertilizer %in% c("Diagnostic", NA)] <- 0 
	d3$P_fertilizer[d3$P_fertilizer == "22.5     0"] <- "22.5"
	d3$P_fertilizer = as.numeric(d3$P_fertilizer)
		
	d3 <- d3[d3$K_fertilizer %in% c(0, 10, 20, 30), ] 
	
	#extracted lat and long info from the reference
	geo <- data.frame(
		adm3 = c("Ilonga", "Mavamizi", "Mlingano", "Selian", "Muheza", "Dareda", "Kwedizinga", "Uyole"), 
		latitude = c(-6.783, -5.138, -5.138, -3.366, -5.166, -5.418, -5.418, -8.918), 
		longitude	= c( 37.037, 38.852, 38.852, 36.632, 38.783, 35.524, 38.516, 33.517), 
		elevation = c(490, 190, 80, 1410, NA, 1635, 350, 1783), 
		soil_pH = c(6.7, 6.6, 5.9, 7.4, NA, 7.3, 6.9, 7.7), 
		soil_P_total = c(14.0, 7.8, 7.4, 37.9, NA, 7.3, 12.7, 11), 
		soil_K = c(316, 402, 339, 2133, NA, 343, 277, 1108))
	
	d3 <- merge(d3, geo, by = "adm3", all.x = TRUE) 
	
	#dates from the reference
	d3$trial_id <- paste(d3$adm3, d3$Y)
	
	dd <- data.frame(trial_id = c("Dareda 15", "Dareda 16", "Ilonga 14", "Ilonga 15", "Kwedizinga 16", "Kwedizinga 15", "Mavamizi 14", "Mlingano 14", "Mlingano 15", "Muheza 15", "Selian 14", "Selian 15", "Uyole 15", "Uyole 16"), 
		planting_date = c("2015-05-01", "2016-01-01", "2014-02-21", "2015-03-04", "2016-04-04", "2015-03-21", "2014-03-17", "2014-03-10", "2015-03-24", "2015-03-19", "2014-03-20", "2015-04-03", "2014-12-12", "2015-12-24"), 
		harvest_date = c("2015-07-15", "2016-07-28", "2014-07-10", "2015-07-14", "2016-07-26", "2015-07-20", "2014-07-18", "2014-07-18", "2015-08-13", "2015-09-14", "2014-09-14", "2015-09-11", "2015-08-11", "2016-07-05"))
	
	d3 <- merge(d3, dd, by = "trial_id", all.x = TRUE)


# Zambia
	
	d4 <- data.frame(
		country = "Zambia",
		location = carobiner::replace_values(r4$S, 
			c("Masekera", "MtMukulu", "Chomo", "Choma Mochipapa", "Chitapa" ), 
			c("Msekera", "Mt. Makulu", "Choma", "Choma", "Msekera")),
		variety = r4$Variety, 
		rep = r4$R,
		N_fertilizer = r4$N,
		P_fertilizer = r4$P,
		K_fertilizer = r4$K,
		yield = r4$GrainYld*1000 # converting to kg/ha
	)
	d4$trial_id = paste("ZM", d4$location, r4$Y, sep = "_")
	
	# lat and lon from the reference 
	geo <- data.frame(
		location = c("Mt. Makulu", "Msekera", "Choma", "Kasama"), 
		latitude = c(-15.548, -13.640, -16.829, -10.171), 
		longitude = c(28.251, 32.555, 27.063, 31.226), 
		elevation = c(1220, 1224, 1290, 1230), 
		soil_pH = c(7.3, 6.2, 5.7, 5.1), 
		soil_P_total = c(28.1, 11.9, 26.2, 37.7), 
		soil_K = c(295, 204, 72, 79)
	)
	
	d4 <- merge(d4, geo, by = "location", all.x = TRUE)
	# dates from the reference
	dd <- data.frame(
		trial_id = c("ZM_Choma_2014", "ZM_Choma_2015", "ZM_Kasama_2014", "ZM_Msekera_2014", "ZM_Msekera_2015", "ZM_Mt. Makulu_2013", "ZM_Mt. Makulu_2014", "ZM_Mt. Makulu_2015"), 
		planting_date = c("2014-12-08", "2015-12-13", "2014-12-06", "2014-12-13", "2015-12-18", "2013-12-12", "2014-12-12", "2015-12-01"), 
		harvest_date = c("2015-05-18", "2016-05-17", "2015-05-14", "2015-05-22", "2016-05-22", "2014-05-05", "2015-05-05", "2016-05-06"))
	
	d4 <- merge(d4, dd, by = "trial_id", all.x = TRUE)
	d4 <- unique(d4)
	
	
	d <- carobiner::bindr(d0, d1, d2, d3, d4)

	d$crop <- "maize"
	d$yield_part <- "grain"

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$row_spacing <- 75
	d$plant_spacing <- 25
	d$rep <- as.integer(d$rep)
	d$fertilizer_type <- "unknown" # Not specified

	# one spurious outlier
	d$seed_weight[d$seed_weight == 34210] <- 342.1

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}

