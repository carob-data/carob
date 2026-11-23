# R script for "carob"
# license: GPL (>=3)

## NOTES
# Paper: Shehu et al. 2018 (doi:10.3390/agronomy8020018); rates, plot size, spacing, soil depth and 15% moisture are from its Methods.
# Each field had an OPV and a hybrid set of 6 nutrient omission treatments (one plot each); fields are the replicates.
# 2016 soil, site history, N dates and fall armyworm scores come from the separate 2016 files.
# Coordinates were removed from the public files; 4 of 113 village names were found in OpenStreetMap, the rest are at LGA centroids.

## ISSUES
# S_fertilizer includes the S in SSP (12%) for all P treatments, plus 24 kg S/ha in +SMM; the paper reports only the 24 kg. Keep?
# 99 plots without yield (88 in 2015, mostly not recorded; 3 with 0 plants; 11 in 2016 not harvested). Kept as NA.
# Out of bounds: terminag minima look too strict for maize grain Mg/Ca and micronutrients (mg/kg-like); low grain N and leaf S fit the omission treatments.
# Likely data errors, left as reported: grain K up to 97 mg/g, negative residue P/Mg, grain moisture of 0.3%.
# variety_type taken from the variety name; NG_NOT_Yield_2016 agrees except one OBA SUP-1 plot labelled OPV.
# pest_species "fall armyworm" is not in terminag (values_pest); please add.

## NEW VARIABLES
# agroecological_zone: Northern Guinea or Sudan Savanna; sets the fertilizer rates
# grain_moisture: grain moisture when weighed (%); yields are at 15% (yield_moisture)
# site_status: 2016 field "New" or "Repeated"
# previous_crops_2yr: crops grown in the past two years (2015)
# farmer_yield: maize yield estimated by the farmer (kg/ha)
# soil_sample_date: date of soil sampling (2015)

## NOT PROCESSED
# 2015 stover and biomass dry weights: fresh weight x a fixed 0.48, not measured
# 2016 above-ground biomass: sum of grain and stover
# 2015 farmer practice (manure, fertilizer, other crop): unknown units
# ear-leaf sampling dates: unreliable (some on or after harvest)
# 2016 site history recall data: mixed formats
# 15 fields in 2016 with soil data but no plot data
# NG_NOT_Yield_2016 "Yield Data": same 1039 plots and values as Data_2016 (only the armyworm score is used)
# 2015 NG_NOT_* files (Grain, Stover, Yield) and the second TAMASA file: older copies of Data_2015

carob_script <- function(path) {

"
TAMASA Nutrient Omission Trials (NOTs) 2015–2016: Soil, plant nutrient and maize yield data from Nigeria

This dataset contains data from nutrient omission trials (NOTs) conducted in farmers' fields in Kaduna, Kano, and Katsina States, Nigeria, during the 2015 and 2016 growing seasons, as part of the Taking Maize Agronomy to Scale in Africa (TAMASA) project.

The trials evaluated maize response to different fertilizer treatments using hybrid and open-pollinated maize varieties. The dataset includes information on experimental sites, soil characteristics, crop management practices, fertilizer applications, grain and stover yields, and nutrient concentrations in soil and plant samples.

The data can be used to study nutrient limitations, fertilizer responses, and maize productivity in northern Nigeria.
"

	uri <- "hdl:11529/10548310"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "KUL;CIMMYT;BUK",
		publication = "doi:10.3390/agronomy8020018",
		project = "TAMASA",
		design = "nutrient omission trial; fields as replicates",   # paper: modified nutrient omission trial design
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;Ca_fertilizer;Mg_fertilizer;Zn_fertilizer;B_fertilizer;variety",
		response_vars = "yield;fwy_residue;dmy_residue;leaf_N;leaf_P;leaf_K;residue_N;residue_P;residue_K;grain_N;grain_P;grain_K",
		carob_contributor = "Stella Muthoni",
		carob_LLM = "Claude Opus 5.5 (merged from 3 drafts)",
		carob_date = "2026-11-18",
		carob_completion = 85,
		carob_effort = 1
	)

	f1 <- ff[basename(ff) == "TAMASA_NOTs_Database_Nigeria_2015_2016-1 (blurred).xlsx"]
	f2 <- ff[basename(ff) == "NG_NOT_Soil_2016 (blurred).xlsx"]
	f3 <- ff[basename(ff) == "NG_NOT_Yield_2016 (blurred).xlsx"]
	
	r1a <- carobiner::read.excel(f1, sheet="Meta-data")                    # dataset description
	r1b <- carobiner::read.excel(f1, sheet="Variables")                    # codebook: names and units
	r1c <- carobiner::read.excel(f1, sheet="Treatment_Description")        # the 6 treatment codes
	r1d <- carobiner::read.excel(f1, sheet="Soil_analysis_methods")        # soil lab methods (Mehlich-3)
	r1e <- carobiner::read.excel(f1, sheet="Fertilizer rates & variety")   # N-P2O5-K2O rates per zone, varieties
	r1f <- carobiner::read.excel(f1, sheet="Data_2015", skip=1)            # 2015 plot data
	r1g <- carobiner::read.excel(f1, sheet="Data_2016", skip=1)            # 2016 plot data
	
	r2a <- carobiner::read.excel(f2, sheet="Meta Data")                    # description of the 2016 soil file
	r2b <- carobiner::read.excel(f2, sheet="Variable")                     # codebook for soil data
	r2c <- carobiner::read.excel(f2, sheet="Soil Analysis Data")           # 2016 soil per site
	r2d <- carobiner::read.excel(f2, sheet="Data History")                 # file version notes
	
	r3a <- carobiner::read.excel(f3, sheet="Meta Data")                    # description of the 2016 yield file
	r3b <- carobiner::read.excel(f3, sheet="Plot Layout")                  # plots and treatments (shows Mg in treatment 6)
	r3c <- carobiner::read.excel(f3, sheet="Variables")                    # codebook for yield data
	r3d <- carobiner::read.excel(f3, sheet="Site mgt history")             # 2016 site, State/LGA, farm history
	r3e <- carobiner::read.excel(f3, sheet="Timing Data")                  # 2016 N split dates
	r3f <- carobiner::read.excel(f3, sheet="Yield Data")                   # 2016 fall armyworm score
	r3g <- carobiner::read.excel(f3, sheet="Data history")                 # file version notes
	
	## 2015 plots (r1f)
	
	# text dates are dd/mm/yyyy; numbers are Excel dates with day and month swapped
	fix_date <- function(x) {
	  out <- as.Date(x, format="%d/%m/%Y")
	  i <- is.na(out) & grepl("^[0-9]+$", x)
	  n <- as.Date(as.numeric(x[i]), origin="1899-12-30")
	  out[i] <- as.Date(format(n, "%Y-%d-%m"))
	  as.character(out)
	}
	
	# site-level values in Data_2015 are only given for some plots of a site
	fill_site <- function(x, site) {
	  v <- tapply(x, site, function(i) i[!is.na(i)][1])
	  as.vector(v[site])
	}
	
	# farmers' crop names to terminag names
	crop_names <- function(x) {
	  x <- tolower(trimws(x))
	  x <- gsub("g/nut|gnut", "groundnut", x)
	  x <- gsub(", and |, | and |/| & ", ";", x)
	  x <- gsub("mazie", "maize", x)
	  x <- gsub("sorgum", "sorghum", x)
	  x <- gsub("\\bsoybea\\b", "soybean", x)
	  x <- gsub("sweet potato", "sweetpotato", x)
	  x <- gsub("late millet", "millet", x)
	  x <- gsub("\\bpepper\\b", "chili pepper", x)   # assumed Capsicum
	  x <- gsub("fallow", "none", x)
	  x
	}
	
	trt <- c("Control", "PK", "NK", "NP", "NPK", "NPK+S+Ca+Mg+Zn+B")   # plot numbers 1-6
	site1 <- r1f$`Experiment site code = EXPsit`
	maturity1 <- data.frame(do.call(rbind, strsplit(as.character(r1f$`Crop maturation`), "-")))   # e.g. "105-110" days

	d1a <- data.frame(
	  trial_id = site1,
	  adm1 = r1f$State,
	  adm2 = r1f$LGA,
	  location = r1f$`Community/village`,
	  agroecological_zone = r1f$`Agro-ecological zone`,
	  plot = r1f$`Plot number = PLOTnumb`,
	  treatment = trt[r1f$`Plot number = PLOTnumb`],   # 2015 label lacks Mg
	  variety = r1f$`Maize variety name = MVnam`,
	  maturity_days = rowMeans(sapply(maturity1[, c("X1", "X2")], as.numeric)),   # mean of the variety's range
	  planting_date = fix_date(r1f$`Planting date = PLNdat`),   # equals "1st basal application"
	  N1_date = fix_date(r1f$`1st basal application`),
	  N2_date = fix_date(r1f$`2nd N application`),
	  N3_date = fix_date(r1f$`3rd N application`),
	  harvest_date = fix_date(r1f$`Harvest date = HDATE`),
	  plant_density = r1f$`No. of plants in the net plot` * 10000 / 9,   # 9 m2 net plot
	  cob_density = r1f$`No. of cobs in the net plot` * 10000 / 9,
	  fwy_residue = r1f$`Wt (kg) of the total maize stalks in the net plot` * 10000 / 9,
	  grain_moisture = r1f$`Grain moisture content (%)`,
	  shelling_percentage = r1f$`Shelling Percentage`,
	  seed_weight = r1f$`100 seed weight (g)` * 10,   # 100 -> 1000 seeds
	  yield = r1f$`Grain Yield (kg/ha)`,
	  leaf_N = r1f$`% N...69` * 10,   # ear leaf; % -> mg/g
	  leaf_P = r1f$`% P...70` * 10,
	  leaf_K = r1f$`% K...71` * 10,
	  leaf_Ca = r1f$`% Ca...72` * 10,
	  leaf_Mg = r1f$`% Mg...73` * 10,
	  leaf_S = r1f$S * 10,   # unit not given; values fit %
	  leaf_Cu = r1f$`ppm Cu...75` / 1000,   # ppm -> mg/g
	  leaf_Fe = r1f$`ppm Fe...76` / 1000,
	  leaf_Mn = r1f$`ppm Mn...77` / 1000,
	  leaf_Zn = r1f$`ppm Zn...78` / 1000,
	  leaf_B = r1f$`ppm B` / 1000,
	  residue_N = r1f$`% N...80` * 10,   # stover
	  residue_P = r1f$`% P...81` * 10,
	  residue_K = r1f$`% K...82` * 10,
	  residue_Ca = r1f$`% Ca...83` * 10,
	  residue_Mg = r1f$`% Mg...84` * 10,
	  residue_Na = r1f$`ppm Na...85` / 1000,
	  residue_Mn = r1f$`ppm Mn...86` / 1000,
	  residue_Cu = r1f$`ppm Cu...87` / 1000,
	  residue_Fe = r1f$`ppm Fe...88` / 1000,
	  residue_Zn = r1f$`ppm Zn...89` / 1000,
	  grain_N = r1f$`% N...90` * 10,
	  grain_P = r1f$`% P...91` * 10,
	  grain_K = r1f$`% K...92` * 10,
	  grain_Ca = r1f$`% Ca...93` * 10,
	  grain_Mg = r1f$`% Mg...94` * 10,
	  grain_Na = r1f$`ppm Na...95` / 1000,
	  grain_Mn = r1f$`ppm Mn...96` / 1000,
	  grain_Cu = r1f$`ppm Cu...97` / 1000,
	  grain_Fe = r1f$`ppm Fe...98` / 1000,
	  grain_Zn = r1f$`ppm Zn...99` / 1000,
	  farmland = fill_site(r1f$`Farm Size (Ha)`, site1),
	  previous_crops_2yr = crop_names(fill_site(r1f$`Crops grown in the past two years`, site1)),
	  farmer_yield = fill_site(r1f$`Estimated yield of Maize (kg/ha)`, site1),
	  soil_sample_date = fix_date(fill_site(r1f$`Soil sampling date zero`, site1)),
	  soil_pH = fill_site(r1f$`pH (Water)  1:1`, site1),
	  soil_SOC = fill_site(r1f$`OC (%)`, site1),
	  soil_N_total = fill_site(r1f$`N (%)`, site1) * 10000,   # % -> mg/kg
	  soil_P = fill_site(r1f$`MehP (ppm)`, site1),
	  soil_sand = fill_site(r1f$`Sand (%)`, site1),
	  soil_silt = fill_site(r1f$`Silt (%)`, site1),
	  soil_clay = fill_site(r1f$`Clay (%)`, site1),
	  soil_Ca_exch = fill_site(r1f$`Ca (cmol/kg)`, site1),
	  soil_Mg_exch = fill_site(r1f$`Mg (cmol/kg)`, site1),
	  soil_K_exch = fill_site(r1f$`K (cmol/kg)`, site1),
	  soil_Na_exch = fill_site(r1f$`Na (cmol/kg)`, site1),
	  soil_ex_acidity = fill_site(r1f$`Exch. Acidity (cmol/kg)`, site1),
	  soil_ECEC = fill_site(r1f$`ECEC (cmol/kg)`, site1),
	  soil_Zn = fill_site(r1f$`Zn (ppm)`, site1),
	  soil_Cu = fill_site(r1f$`Cu (ppm)`, site1),
	  soil_Mn = fill_site(r1f$`Mn (ppm)`, site1),
	  soil_Fe = fill_site(r1f$`Fe (ppm)`, site1),
	  soil_bd = fill_site(r1f$`BD (g/cm3)`, site1),
	  soil_B = fill_site(r1f$`B (ppm)`, site1),
	  soil_S = fill_site(r1f$`S (pmm)`, site1)   # header typo; ppm
	)
	d1a <- d1a[which(r1f$Country == "Nigeria"), ]   # drop empty and summary rows
	d1a$previous_crops_2yr[d1a$previous_crops_2yr == "53"] <- NA   # not a crop
	
	## 2016 plots (r1g)
	site2 <- r1g$`Experimental site code = EXPsit`
	maturity2 <- data.frame(do.call(rbind, strsplit(as.character(r1g$`Crop maturation`), "-")))

	d1b <- data.frame(
	  trial_id = site2,
	  sample_code = r1g$sample_code,   # key to d3f; removed after merging
	  agroecological_zone = r1g$`Agro-ecological zone`,
	  site_status = r1g$Status,
	  plot = match(r1g$Treatment, trt),
	  treatment = r1g$Treatment,
	  variety = r1g$`Maize variety name = MVnam`,
	  maturity_days = rowMeans(sapply(maturity2[, c("X1", "X2")], as.numeric)),   # mean of the variety's range
	  planting_date = fix_date(r1g$`Planting date`),
	  N1_date = fix_date(r1g$`Planting date`),
	  harvest_date = as.character(as.Date(r1g$`Harvest Date`)),
	  plant_density = r1g$`Number plants per netplot` * 10000 / 9,   # 9 m2 net plot
	  cob_density = r1g$`Number Ears per net plot` * 10000 / 9,
	  fwy_residue = r1g$`Total stalks_weight per netplot  (Kg)` * 10000 / 9,
	  dmy_residue = r1g$`Stalk Yield (kg/ha)`,   # dry/fresh ratio of a dried subsample
	  grain_moisture = r1g$`Grain moisture content (%)`,
	  shelling_percentage = r1g$`Shelling Percentage`,
	  yield = r1g$`Grain Yield (kg/ha)`,
	  leaf_N = r1g$`% N...42` * 10,   # ear leaf; % -> mg/g
	  leaf_P = r1g$`% P...43` * 10,
	  leaf_K = r1g$`% K...44` * 10,
	  leaf_Ca = r1g$`% Ca` * 10,
	  leaf_Mg = r1g$`% Mg` * 10,
	  leaf_S = r1g$S * 10,   # unit not given; values fit %
	  leaf_Cu = r1g$`ppm Cu` / 1000,   # ppm -> mg/g
	  leaf_Fe = r1g$`ppm Fe` / 1000,
	  leaf_Mn = r1g$`ppm Mn` / 1000,
	  leaf_Zn = r1g$`ppm Zn` / 1000,
	  leaf_B = r1g$`ppm B` / 1000,
	  residue_N = r1g$`% N...53` * 10,   # stover; same column order as 2015
	  residue_P = r1g$`% P...54` * 10,
	  residue_K = r1g$`% K...55` * 10,
	  grain_N = r1g$`% N...56` * 10,
	  grain_P = r1g$`% P...57` * 10,
	  grain_K = r1g$`% K...58` * 10
	)
	d1b <- d1b[!is.na(d1b$trial_id), ]   # drop empty rows; rows without Country are unharvested plots
	d1b$harvest_date[d1b$harvest_date == "2017-09-30"] <- "2016-09-30"   # N-NOT6-4 OPV control; other plots of the site 2016-09-30

	## 2016 soil per site (r2c)
	d2c <- data.frame(
	  trial_id = r2c$`SAMPLE CODE`,
	  soil_pH = r2c$`pH(H2O)`,
	  soil_SOC = r2c$OC,
	  soil_N_total = r2c$N * 10000,   # % -> mg/kg
	  soil_P = r2c$`Meh P`,
	  soil_sand = r2c$`Particle size`,   # sand, silt, clay
	  soil_silt = r2c$...12,
	  soil_clay = r2c$...13,
	  soil_Ca_exch = r2c$Ca,
	  soil_Mg_exch = r2c$Mg,
	  soil_K_exch = r2c$K,
	  soil_Na_exch = r2c$Na,
	  soil_ex_acidity = r2c$`Exch. Acidity`,
	  soil_ECEC = r2c$ECEC,
	  soil_Zn = r2c$Zn,
	  soil_Cu = r2c$Cu,
	  soil_Mn = r2c$Mn,
	  soil_Fe = r2c$Fe
	)
	d2c <- d2c[!is.na(d2c$trial_id) & !duplicated(d2c$trial_id), ]   # one record per site

	## 2016 site history (r3d)
	d3d <- data.frame(
	  trial_id = r3d$`Site Code`,
	  adm1 = r3d$State,   # main sheet has wrong State/LGA for N-NOT1-4 and N-NOT4-7
	  adm2 = r3d$LGA,
	  location = r3d$Community,
	  farmland = r3d$`Farm Size (ha)`,
	  farmer_yield = r3d$`Maize Yield (t/ha)` * 1000,
	  previous_crop = crop_names(r3d$`Previous crop1/crops 2`),
	  previous_crop_residue_management = trimws(r3d$`Previous crop Residue Management`)   # includes unexplained numbers
	)
	d3d <- d3d[!is.na(d3d$trial_id) & !duplicated(d3d$trial_id), ]   # one record per site
	pcrm <- d3d$previous_crop_residue_management
	d3d$previous_crop_burnt <- ifelse(grepl("Burnt", pcrm), TRUE, ifelse(grepl("Remove|Retained", pcrm), FALSE, NA))

	## 2016 N split dates (r3e); Data_2016 has these for one plot per site only
	d3e <- data.frame(
	  trial_id = r3e$`Site Code`,
	  N2_date = fix_date(r3e$`2nd N Split Application Date`),
	  N3_date = fix_date(r3e$`3rd N Split Application Date`)
	)
	d3e <- d3e[!is.na(d3e$trial_id) & !duplicated(d3e$trial_id), ]   # one record per site

	## 2016 fall armyworm per plot (r3f); its other columns repeat Data_2016
	d3f <- data.frame(
	  sample_code = r3f$sample_code,
	  pest_species = "fall armyworm",
	  pest_severity = as.character(r3f$`Fall army warm infestation ranking`),
	  severity_scale = "1-5"   # % plants affected: 1 = 0-10, 2 = 11-25, 3 = 26-50, 4 = 51-75, 5 = 76-100
	)
	d3f <- d3f[!is.na(d3f$sample_code), ]

	## 2016: plots with soil, site history, N dates and armyworm
	d1b <- merge(d1b, d2c, by="trial_id", all.x=TRUE)
	d1b <- merge(d1b, d3d, by="trial_id", all.x=TRUE)
	d1b <- merge(d1b, d3e, by="trial_id", all.x=TRUE)
	d1b <- merge(d1b, d3f, by="sample_code", all.x=TRUE)
	d1b$sample_code <- NULL

	## both years
	d <- carobiner::bindr(d1a, d1b)
	
	d$adm2[d$adm2 == "T/Wada"] <- "Tudun Wada"
	d$location <- carobiner::fix_name(d$location, "title")
	d$agroecological_zone[d$agroecological_zone == "Sudan savanna"] <- "Sudan Savanna"
	d$variety_type <- ifelse(grepl("OBA", d$variety), "hybrid", "OPV")   # from the variety name; f3 variety_type agrees except one plot
	d$variety <- gsub("OBA SUP-", "Oba Super ", d$variety)
	d$plot_id <- paste(substr(d$planting_date, 1, 4), d$trial_id, d$variety_type, d$plot, sep="_")
	d$plot <- NULL
	
	d$country <- "Nigeria"
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$yield_moisture <- 15   # paper; also fits all records
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE   # rainy season; 2016 site history: rainfed
	d$plot_area <- 30   # 5 m x 6 m
	d$sampled_plot_area <- 9
	d$row_spacing <- 75
	d$plant_spacing <- 25
	d$depth_top <- 0
	d$depth_bottom <- 20   # paper: 0-20 cm
	d$soil_P_method <- "Mehlich3"   # sheet "Soil_analysis_methods"
	
	## fertilizer (paper and rate sheet): N-P2O5-K2O 140-50-50 (Northern Guinea Savanna), 120-40-40 (Sudan Savanna)
	ss <- d$agroecological_zone == "Sudan Savanna"
	hasN <- d$treatment %in% c("NK", "NP", "NPK", "NPK+S+Ca+Mg+Zn+B")
	hasP <- d$treatment %in% c("PK", "NP", "NPK", "NPK+S+Ca+Mg+Zn+B")
	hasK <- d$treatment %in% c("PK", "NK", "NPK", "NPK+S+Ca+Mg+Zn+B")
	smm <- d$treatment == "NPK+S+Ca+Mg+Zn+B"
	d$N_fertilizer <- hasN * ifelse(ss, 120, 140)
	d$P_fertilizer <- hasP * ifelse(ss, 40, 50) / 2.29   # P2O5 -> P
	d$K_fertilizer <- hasK * ifelse(ss, 40, 50) / 1.2051   # K2O -> K
	d$S_fertilizer <- d$P_fertilizer / 0.0874 * 0.12 + smm * 24   # S in SSP (12%) plus 24 kg S in +SMM
	d$Ca_fertilizer <- smm * 10
	d$Mg_fertilizer <- smm * 10
	d$Zn_fertilizer <- smm * 5
	d$B_fertilizer <- smm * 5
	ftype <- c("none", "SSP;KCl", "urea;KCl", "urea;SSP", "urea;SSP;KCl", "urea;SSP;KCl;gypsum;MgSO4;ZnSO4;Borax")
	d$fertilizer_type <- ftype[match(d$treatment, trt)]   # CaSO4 as gypsum
	d$fertilizer_used <- d$treatment != "Control"
	d$N_splits <- as.integer(hasN * 3)
	
	# paper: N in 3 equal splits at planting, 21 and 42 days after emergence; other nutrients at planting
	d$fertilizer_date <- ifelse(hasN, paste(d$N1_date, d$N2_date, d$N3_date, sep=";"), d$N1_date)
	d$fertilizer_date[d$treatment == "Control"] <- NA
	d$N1_date <- d$N2_date <- d$N3_date <- NULL
	
	
	# villages found in OpenStreetMap (looked up offline) and inside their LGA; the others are not mapped
	vil <- data.frame(
	  adm2 = c("Doguwa", "Doguwa", "Doguwa", "Dandume"),
	  location = c("Doguwa", "Sogaga", "Tagwaye", "Kadawa"),
	  longitude = c(8.5922, 8.5777, 8.6148, 7.2167),
	  latitude = c(10.74, 10.7549, 10.8513, 11.4333),
	  geo_uncertainty = 2000,   # village extent
	  geo_source = "OpenStreetMap village"
	)
	
	# LGA centroids (GADM 4.1, computed offline) for villages not found; geo_uncertainty is the max distance to the LGA border (m)
	lga <- data.frame(
	  adm2 = c("Giwa", "Ikara", "Kauru", "Lere", "Makarfi", "Soba", "Bunkure", "Doguwa", "Tofa", "Tudun Wada", "Bakori", "Dandume", "Faskari", "Funtua"),
	  longitude = c(7.4226, 8.162, 8.3093, 8.5577, 7.9541, 8.0428, 8.5254, 8.6387, 8.3101, 8.478, 7.4381, 7.1985, 7.1081, 7.3256),
	  latitude = c(11.0907, 11.2909, 10.1913, 10.2996, 11.3245, 10.926, 11.6629, 10.8832, 12.0034, 11.209, 11.5998, 11.3833, 11.6424, 11.4148),
	  geo_uncertainty = c(43000, 42000, 64000, 43000, 21000, 36000, 23000, 39000, 14000, 39000, 26000, 20000, 38000, 24000),
	  geo_source = "GADM 4.1 adm2 centroid"
	)
	
	d <- merge(d, vil, by=c("adm2", "location"), all.x=TRUE)
	i <- is.na(d$longitude)
	j <- match(d$adm2[i], lga$adm2)
	d$longitude[i] <- lga$longitude[j]
	d$latitude[i] <- lga$latitude[j]
	d$geo_uncertainty[i] <- lga$geo_uncertainty[j]
	d$geo_source[i] <- lga$geo_source[j]
	d$geo_from_source <- FALSE
	
	carobiner::write_files(path, meta, d)
}

