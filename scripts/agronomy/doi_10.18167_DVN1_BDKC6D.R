# R script for "carob"
# license: GPL (>=3)

## ISSUES
"Several suggested/provisional fields:
       grain_N_total (total N in harvested grain, kg/ha - distinct from
grain_N, which is a concentration in mg/g); 
       soil_FC and soil_WP (field capacity and wilting point, % w - no existing terminag soil-water field);
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
#          Soil_FC and soil_WP (field capacity / wilting point) are suggested new terminag terms
#          HCCF/HMINF/DAF are confirmed uniform across all 5 STICS layers

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
	                                design = "on-farm trials, one simulation unit per crop x soil x climate x fertilizer treatment combination",
	                                data_type = "on-farm experiment",
	                                treatment_vars = "N_fertilizer",
	                                response_vars = "yield;dmy_total;LAI;harvest_index;N_fixation",
	                                notes = NA,
	                                carob_contributor = "Stella Muthoni",
	                                carob_date = "2026-07-13",
	                                carob_completion = 70,	
	                                carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "obs.csv"] # Observations. Actual measured outcomes
	f2 <- ff[basename(ff) == "score.csv"]  #Categorical scores per simulation
	f3 <- ff[basename(ff) == "tec.csv"]         #Management operations per simulation
	f4 <- ff[basename(ff) == "soil.csv"] #Soil characteristics
	f5 <- ff[basename(ff) == "sta.csv"]    # Weather station metadata
	f6 <- ff[basename(ff) == "usms.csv"]        # master linking table

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)
	r6 <- read.csv(f6)

### Clean the yield/actual observation data
	r1_clean <- r1
	r1_clean[r1_clean == -999] <- NA
	
### Aggregate to one row per usm_name, pulling together the single real value
	## for each variable regardless of which row it originally sat on.
	r1_agg <- aggregate(
	  cbind(mafruit, masec_n, p1000grain, ircarb.n., CNgrain, QNgrain,
	        iplt0, ilevs, iflos, imats, lai_n, Qfix) ~ usm_name,
	  data = r1_clean,
	  FUN = function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE),
	  na.action = na.pass)
	
	#  Select relevant columns
	d1 <- data.frame(
	  usm_name = r1_agg$usm_name,
	  yield = r1_agg$mafruit * 1000,
	  yield_part = "grain",
	  yield_moisture = 0,
	  yield_isfresh = FALSE,
	  dmy_total = r1_agg$masec_n * 1000,
	  seed_weight = r1_agg$p1000grain,
	  harvest_index = r1_agg$ircarb.n. * 100,
	  grain_N = r1_agg$CNgrain * 10,
	  LAI = r1_agg$lai_n,
	  flowering_days = r1_agg$iflos - r1_agg$iplt0,
	  maturity_days  = r1_agg$imats - r1_agg$iplt0,
	  N_fixation = r1_agg$Qfix, #amount of N fixed symbiotically (BNF), kg/ha
	  grain_N_total = r1_agg$QNgrain #suggested term for total amount of N in harvested grain, kg/ha
	)
	
	#  Select relevant columns
	d2 <- data.frame(
	  usm_name = r2$usm_name,
	  weed_severity = c("low","medium","high")[r2$weed_f + 1],
	  pest_disease = r2$pest_disease_f,
	  waterlogged = r2$waterlog_f,
	  deficiency_P = r2$p_def_f
	)
	
# Clean the farm management database
	# Clean the farm management database
	r3_clean <- r3
	r3_clean$Qtot_N[r3_clean$Qtot_N == 999] <- NA
	r3_clean$irec[r3_clean$irec == 999] <- NA   # harvest date genuinely not recorded
	
	## bring in fclim1's year to convert iplt0 (day-of-year count) into a real date
	r3_clean <- merge(r3_clean, data.frame(usm_name = r6$usm_name, fclim1 = r6$fclim1), by = "usm_name", all.x = TRUE)
	r3_clean$fclim1_year <- as.numeric(sub(".*\\.", "", r3_clean$fclim1))
	r3_clean$planting_date <- as.character(as.Date(paste0(r3_clean$fclim1_year, "-01-01")) + (r3_clean$iplt0 - 1))
	
	d3 <- data.frame(
	  usm_name = r3_clean$usm_name,
	  seed_density = r3_clean$densitesem * 10000,
	  N_fertilizer = r3_clean$Qtot_N,
	  irrigated = FALSE,
	  emergence_days = r3_clean$ilev - r3_clean$iplt0,
	  planting_date = r3_clean$planting_date
	)
	
	d4 <- data.frame(
	  nomsol = r4$nomsol,
	  soil_clay = r4$argi,
	  soil_pH = r4$pH,
	  soil_N = r4$norg * 10000,
	  soil_CaCO3 = r4$calc * 10,
	  soil_bd = r4$DAF_1,
	  soil_FC = r4$HCCF_1,   # suggested term
	  soil_WP = r4$HMINF_1   # suggested term
	)
	
	d5 <- data.frame(
	  id_ws = r5$id_ws,
	  latitude = r5$latitude,
	  elevation = r5$altisimul,
	  longitude = 31.8388,
	  geo_from_source = FALSE)
	
	d6 <- data.frame(
	  usm_name = r6$usm_name,
	  nomsol = r6$nomsol,
	  id_ws = r6$id_ws,
	  crop = tolower(r6$crop),
	  treatment = r6$ferti,
	  year_harvest = r6$year_harvest,
	  on_farm = TRUE,
	  is_survey = FALSE)

	# add the linking usm_name for d4 and d5
	d4 <- merge(d6[, c("usm_name","nomsol")], d4, by = "nomsol", all.x = TRUE)
	d4$nomsol <- NULL
	d5 <- merge(d6[, c("usm_name","id_ws")], d5, by = "id_ws", all.x = TRUE)
	d5$id_ws <- NULL
	
# Final merge of all d tables
	d <- d1
	d <- merge(d, d2, by = "usm_name", all.x = TRUE)
	d <- merge(d, d3, by = "usm_name", all.x = TRUE)
	d <- merge(d, d4, by = "usm_name", all.x = TRUE)
	d <- merge(d, d5, by = "usm_name", all.x = TRUE)
	d <- merge(d, d6, by = "usm_name", all.x = TRUE)
	
	d$trial_id <- as.character(as.integer(as.factor(d$usm_name)))
	d$date <- as.character(d$year_harvest)
	d$year_harvest <- NULL
	d$usm_name <- NULL
	d$country <- "Zimbabwe"
	d$adm1 <- "Mashonaland East"
	d$adm2 <- "Murehwa"
	d$nomsol <- NULL
	d$id_ws <- NULL
	
	d$K_fertilizer <- NA
	d$P_fertilizer <- NA
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}


