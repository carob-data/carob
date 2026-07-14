# R script for "carob"
# license: GPL (>=3)

## ISSUES
"suggested/provisional fields:
       pest_disease (combined pest+disease occurrence flag from score.csv - source

data does not distinguish which); 
       waterlogged and deficiency_P (waterlogging and phosphorus deficiency occurrence flags from score.csv). 
       N_fixation exceeds terminag's upper bound (150) for one trial (173 kg/ha)
       A few trials have NA yield despite having other real measurements (biomass, LAI, soil water).
       longitude/latitude are NA for 17 trials (the 15 farmer-named groundnut trials,

plus test and the MKSH2 trial) that have no match in usms
       N_fertilizer (the declared treatment variable) is NA for the same 15 farmer-named trials lacking usms.csv 
       linkage - their fertilization treatment was never coded in tec.csv either."

##  NOTES
# -  In score.csv
#         weed infestation score (0=low,1=medium,2=high) recoded to text to match
#         Suggested names: deficiency_P (p_def_f); pest_disease (pest_disease_f); waterlogged(waterlog_f)

# - In soil.csv
#         HCCF/HMINF/DAF are confirmed uniform across all 5 STICS layers

# - In sta.csv
#          longitude not provided anywhere in the source data - estimated via carobiner::geocode(country="Zimbabwe", location="Murehwa")


carob_script <- function(path) {

"
Data for the calibration of STICS and yield gap analysis for maize, groundnut cowpea and pigeon pea in Murehwa district, sub-humid Zimbabwe published in 'Identifying the drivers of yield gaps for maize and legumes in Zimbabwean smallholder farms using locally calibrated crop modelling'

The dataset contains data on the growth of maize, groundnut, cowpea and pigeon pea collected during two growing seasons in on-farm trials in the Murehwa district, sub-humid Zimbabwe. The observed climate data for the experimental seasons is provided, along with basic soil characteristics. Crop management (planting date, date of field operations) is also described, as well as initial conditions prior to planting. Eventually, observations of in-season leaf area index (when available), soil water (when available), biological nitrogen fixation (when available), crop nitrogen content (when available), aboveground biomass and grain yield at harvest (and biomass at flowering when available) are provided. Scores of weed infestation, pest and disease occurrence, waterlogging occurrence and phosphorus deficiency occurrence are included. 

 There is one simulation unit for each combination of crop, soil and climate, identified by an unique key (see usms file)
"

	uri <- "doi:10.18167/DVN1/BDKC6D"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield;dmy_total;LAI;harvest_index;N_fixation",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_date = "2026-07-13",
		carob_completion = 70,	
		carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "obs.csv"]    # Observations. Actual measured outcomes
	f2 <- ff[basename(ff) == "score.csv"]  #Categorical scores per simulation
	f3 <- ff[basename(ff) == "tec.csv"]    #Management operations per simulation
	f4 <- ff[basename(ff) == "soil.csv"]   #Soil characteristics
	f5 <- ff[basename(ff) == "sta.csv"]    # Weather station metadata
	f6 <- ff[basename(ff) == "usms.csv"]   # master linking table
	f7 <- ff[basename(ff) == "climate_for_stics.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)
	r6 <- read.csv(f6)
	r7 <- read.csv(f7)

### yield/actual observation data
	r1[r1 == -999] <- NA
	
	d1 <- data.frame(
	  usm_name = r1$usm_name,
	  yield = r1$mafruit * 1000,
	  yield_part = "grain",
	  yield_moisture = 0,
	  yield_isfresh = FALSE,
	  dmy_total = r1$masec_n * 1000,
	  seed_weight = r1$p1000grain,
	  harvest_index = r1$ircarb.n. * 100,
	  grain_N = r1$CNgrain * 10,
	  LAI = r1$lai_n,
	  flowering_days = r1$iflos - r1$iplt0,
	  maturity_days  = r1$imats - r1$iplt0,
	  N_fixation = r1$Qfix #amount of N fixed symbiotically (BNF), kg/ha
	  #redundant, can be computed from N content and yield
	  #grain_N_total = r1$QNgrain #suggested term for total amount of N in harvested grain, kg/ha
	)
	d1$date = apply(r1[, c("ian", "mo", "jo")], 1, \(x) paste0(x, collapse="-")) |> as.Date() |> as.character()
	d1 <- d1[d1$usm_name != "test", ]

	#  Select relevant columns
	d2 <- data.frame(
	  usm_name = r2$usm_name,
	  weed_severity = c("low","medium","high")[r2$weed_f + 1],
	  pest_disease = r2$pest_disease_f,
	  waterlogged = r2$waterlog_f,
	  deficiency_P = r2$p_def_f
	)
	
# farm management data
	r3$irec[r3$irec == 999] <- NA   # harvest date genuinely not recorded

	d3 <- data.frame(
	  usm_name = r3$usm_name,
	  seed_density = r3$densitesem * 10000,
	  N_fertilizer = ifelse(r3$Qtot_N == 999, NA, r3$Qtot_N),
	  emergence_days = r3$ilev - r3$iplt0,
	  iplt0 = r3$iplt0
	)

	d4 <- data.frame(
	  nomsol = r4$nomsol,
	  soil_clay = r4$argi,
	  soil_pH = r4$pH,
	  soil_N = r4$norg * 10000,
	  soil_CaCO3 = r4$calc * 10,
	  soil_bd = r4$DAF_1,
	  soil_FC = r4$HCCF_1,   
	  soil_PWP = r4$HMINF_1  
	)
	
	d5 <- data.frame(
	  station_id = gsub("_sta.xml", "", r5$id_ws),
	  latitude = r5$latitude,
	  elevation = r5$altisimul,
	  longitude = 31.8388,
	  geo_from_source = FALSE
	)
	
	d6 <- data.frame(
	  usm_name = r6$usm_name,
	  nomsol = r6$nomsol,
	  station_id = gsub("_sta.xml", "", r6$id_ws),
	  crop = tolower(r6$crop),
	  treatment = r6$ferti,
	  year_harvest = r6$year_harvest
	)

	
# Final merge of all d tables
## note 
# d2$usm_name[!(d2$usm_name %in% d1$usm_name)]
# [1] "Makombe_Makombe_Chipo_GN_2"
# d1$usm_name[!(d1$usm_name %in% d2$usm_name)]
# [1] "2022_MKSH2_MZ_1_MZ"  "Dadha_Babra_Mafuta_GN_2_MTK" "Dadha_Christine_Karuru_GN_2_MTK",  and 13 more

	d <- merge(d1, d2, by = "usm_name", all.x = TRUE)
	d <- merge(d, d3, by = "usm_name", all.x = TRUE)

	dd <- merge(d6, d4, by = "nomsol", all.x = TRUE)
	dd <- merge(dd, d5, by = "station_id", all.x = TRUE)

	d <- merge(d, dd, by = "usm_name", all.x = TRUE)

	d$latitude[is.na(d$latitude)] <- mean(d$latitude, na.rm=TRUE)
	d$longitude[is.na(d$longitude)] <- mean(d$longitude, na.rm=TRUE)

	d$planting_date = as.character(as.Date(paste0(d$year_harvest-2, "-12-31")) + d$iplt0)
	d$iplt0 <- NULL
	d$year_harvest <- NULL
	
	d$trial_id <- d$usm_name
	d$usm_name <- NULL
	d$country <- "Zimbabwe"
	d$adm1 <- "Mashonaland East"
	d$adm2 <- "Murehwa"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$nomsol <- NULL
	
	d$K_fertilizer <- NA
	d$P_fertilizer <- NA

	w <- data.frame(
		station_id = r7$id_ws,
		date = as.character(as.Date(paste0(r7$YEAR-1, "-12-31")) + r7$DOY),
		tmin = r7$T2M_MIN,
		tmax = r7$T2M_MAX,
		srad = r7$RADIATION,
		prec = r7$rainfall,
		wspd = r7$WIND,
		rhum = r7$HUMIDITY,
		ETP = r7$ETP # presumably actual evapotranspiratoin
	)
	
	w <- merge(w, d5[, c("station_id", "longitude", "latitude", "elevation")], all.x=TRUE)
	d <- unique(d)
	
	carobiner::write_files(path, meta, d, wth=w)
}


