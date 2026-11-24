# R script for "carob"
# license: GPL (>=3)

## NOTES
# Paper: Shehu et al. 2018 (doi:10.3390/agronomy8020018); rates, plot size, spacing, soil depth and 15% moisture are from its Methods.
# Each field had an OPV and a hybrid set of 6 nutrient omission treatments (one plot each); fields are the replicates.
# 2016 soil, site history, N dates and fall armyworm scores come from the separate 2016 files.

## ISSUES
# S_fertilizer includes the S in SSP (12%) for all P treatments, plus 24 kg S/ha in +SMM; the paper reports only the 24 kg. 
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
# 2015 farmer practice: manure rate has no unit; fertilizer rate is kg product/ha (NPK, or urea and DAP together)
# ear-leaf sampling dates: unreliable (some on or after harvest)
# 2016 farmer recall: mineral fertilizer is three unlabelled kg/ha columns; residue management mixes words and fractions; soil depth is Deep or Shallow. Organic fertilizer is previous practice, not a trial treatment
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
	
	
	# Site location and elevation data from the unblurred files
	geo <- data.frame(
		trial_id = c("N-1-1", "N-1-2", "N-1-3", "N-1-4", "N-1-5", "N-1-6", "N-1-7", "N-1-8", "N-1-9",
			"N-1-10", "N-2-1", "N-2-2", "N-2-3", "N-2-4", "N-2-5", "N-2-6", "N-2-7", "N-2-8", "N-2-9",
			"N-2-10", "N-3-1", "N-3-2", "N-3-3", "N-3-4", "N-3-5", "N-3-6", "N-3-7", "N-3-8", "N-3-9",
			"N-3-10", "N-4-1", "N-4-2", "N-4-3", "N-4-4", "N-4-5", "N-4-6", "N-4-7", "N-4-8", "N-4-9",
			"N-4-10", "N-5-1", "N-5-2", "N-5-3", "N-5-4", "N-5-5", "N-5-6", "N-5-7", "N-5-8", "N-5-9",
			"N-5-10", "N-6-1", "N-6-2", "N-6-3", "N-6-4", "N-6-5", "N-6-6", "N-6-7", "N-6-8", "N-6-9",
			"N-6-10", "N-7-1", "N-7-2", "N-7-3", "N-7-4", "N-7-5", "N-7-6", "N-7-7", "N-7-8", "N-7-9",
			"N-7-10", "N-8-1", "N-8-2", "N-8-3", "N-8-4", "N-8-5", "N-8-6", "N-8-7", "N-8-8", "N-8-9",
			"N-8-10", "N-9-1", "N-9-2", "N-9-3", "N-9-4", "N-9-5", "N-10-1", "N-10-2", "N-10-3", "N-10-4",
			"N-10-5", "N-10-6", "N-10-7", "N-10-8", "N-10-9", "N-10-10", "N-NOT1-1", "N-NOT1-2", "N-NOT1-3",
			"N-NOT1-4", "N-NOT1-5", "N-NOT1-6", "N-NOT1-7", "N-NOT1-8", "N-NOT1-9", "N-NOT1-10", "N-NOT2-1",
			"N-NOT2-2", "N-NOT2-3", "N-NOT2-4", "N-NOT2-5", "N-NOT2-6", "N-NOT2-7", "N-NOT2-8", "N-NOT2-9",
			"N-NOT3-1", "N-NOT3-2", "N-NOT3-3", "N-NOT3-4", "N-NOT3-5", "N-NOT4-1", "N-NOT4-2", "N-NOT4-3",
			"N-NOT4-4", "N-NOT4-5", "N-NOT4-6", "N-NOT4-7", "N-NOT4-8", "N-NOT4-9", "N-NOT4-10", "N-NOT5-1",
			"N-NOT5-2", "N-NOT5-3", "N-NOT5-4", "N-NOT5-5", "N-NOT5-6", "N-NOT5-7", "N-NOT5-8", "N-NOT5-9",
			"N-NOT5-10", "N-NOT6-1", "N-NOT6-2", "N-NOT6-3", "N-NOT6-4", "N-NOT7-1", "N-NOT7-2", "N-NOT7-3",
			"N-NOT7-4", "N-NOT7-5", "N-NOT7-6", "N-NOT7-7", "N-NOT7-8", "N-NOT8-1", "N-NOT8-2", "N-NOT8-3",
			"N-NOT8-4", "N-NOT8-5", "N-NOT9-1", "N-NOT9-2", "N-NOT9-3", "N-NOT9-4", "N-NOT9-5", "N-NOT9-6",
			"N-NOT9-7", "N-NOT9-8", "N-NOT9-9", "N-NOT9-10", "N-NOT10-1", "N-NOT10-2", "N-NOT10-3",
			"N-NOT10-4", "N-NOT10-5", "N-NOT11-1", "N-NOT11-2", "N-NOT11-3", "N-NOT11-4", "N-NOT11-5",
			"N-NOT11-6", "N-NOT11-7", "N-NOT11-8", "N-NOT12-1", "N-NOT12-2", "N-NOT12-3", "N-NOT12-4",
			"N-NOT12-5", "N-NOT12-6", "N-NOT12-7", "N-NOT12-8", "N-NOT12-9", "N-NOT13-1", "N-NOT13-2",
			"N-NOT13-3", "N-NOT13-4", "N-NOT13-5", "N-NOT14-1", "N-NOT14-2", "N-NOT14-3", "N-NOT14-4",
			"N-NOT14-5"),
		latitude = c(10.4875, 10.50395, 10.52715, 10.51662, 10.51345, 10.50512, 10.50272, 10.52515,
			10.54295, 10.51815, 10.73025, 10.75030, 10.71538, 10.68866, 10.77826, 10.79174, 10.77831,
			10.77896, 10.68781, 10.67590, 11.44327, 11.44262, 11.44295, 11.46079, 11.46064, 11.43369,
			11.43786, 11.43122, 11.43515, 11.44089, 11.57925, 11.55759, 11.62754, 11.62602, 11.52981,
			11.53308, 11.61091, 11.61581, 11.63083, 11.57696, 11.54052, 11.54201, 11.50024, 11.37662,
			11.38078, 11.38209, 11.38229, 11.47598, 11.57925, 11.55008, 11.19444, 11.14138, 11.14053,
			11.14067, 11.14018, 11.13414, 11.12717, 11.12285, 11.12777, 11.13942, 11.00855, 11.01100,
			11.01180, 11.01435, 11.01557, 11.015709, 11.01717, 11.04658, 11.05197, 11.05500, 11.31994,
			11.45278, 11.45514, 11.32161, 11.31616, 11.31644, 11.32039, 11.31147, 11.32000, 11.39340,
			11.24825, 11.24925, 11.28334, 11.29094, 11.27799, 11.68786, 11.68655, 11.68468, 11.68309,
			11.67712, 11.67278, 11.73519, 11.68081, 11.67099, 11.68229, 11.62195, 11.64533, 11.62875,
			11.57416, 11.55343, 11.663035, 11.65778, 11.6392, 11.63299, 11.63346, 11.53497, 11.47186, 11.4732,
			11.5002, 11.5079, 11.5069, 11.48173, 11.48156, 11.5794, 11.46078, 11.460457, 11.435628, 11.443078,
			11.43465, 11.73704, 11.73731, 11.75949, 11.7667, 11.75527, 11.57618, 11.54272, 11.62045, 11.6201,
			11.53085, 11.28899, 11.2897, 11.28962, 11.29636, 11.33448, 11.24194, 11.24248, 11.25114, 11.24581,
			11.25075, 11.45124, 11.4638, 11.3862, 11.39477, 11.01071, 11.0137, 11.0138, 11.01584, 11.0222,
			11.0158, 11.01627, 11.01925, 10.59355, 10.56343, 10.56473, 10.57152, 10.54874, 10.72944, 10.75062,
			10.73861, 10.77884, 10.77841, 10.77832, 10.79184, 10.68406, 10.67283, 10.74032, 10.52754,
			10.54297, 10.52542, 10.50803, 10.48773, 11.12786, 11.12727, 11.12484, 11.13327, 11.14099,
			11.14065, 11.19498, 11.13432, 11.2376, 11.23856, 11.23952, 11.24198, 11.25834, 11.24061, 11.22976,
			11.26851, 11.23345, 11.66945, 11.67769, 11.73501, 11.75608, 11.68038, 11.99441, 11.98762,
			12.01035, 12.01728, 11.99703),
		longitude = c(8.545867, 8.52375, 8.508783, 8.49195, 8.48811, 8.48386, 8.50559, 8.46690, 8.45099,
			8.46467, 8.57735, 8.58041, 8.60209, 8.62682, 8.58441, 8.58402, 8.60252, 8.60721, 8.62577, 8.63299,
			7.19931, 7.19999, 7.19659, 7.14772, 7.14874, 7.24725, 7.25665, 7.24968, 7.25239, 7.26125, 7.40556,
			7.41263, 7.46770, 7.46558, 7.36272, 7.36767, 7.45851, 7.46241, 7.46457, 7.45516, 7.28199, 7.27891,
			7.33610, 7.33746, 7.33749, 7.33895, 7.34009, 7.28875, 7.24797, 7.27764, 8.14108, 8.23223, 8.23195,
			8.23935, 8.23509, 8.23159, 8.23679, 8.23421, 8.23931, 8.23532, 7.91857, 7.90709, 7.89551, 7.89514,
			7.89849, 7.88112, 7.87684, 7.87486, 7.85280, 7.84558, 7.99636, 7.92364, 7.92681, 7.95611, 7.96033,
			7.96478, 7.98585, 7.98358, 7.99560, 7.90139, 8.37213, 8.37048, 8.36618, 8.37420, 8.37718, 8.53474,
			8.53336, 8.53477, 8.52455, 8.52781, 8.54260, 8.53797, 8.54914, 8.52986, 8.54580, 7.11551, 7.14161,
			7.13451, 7.09071, 7.08804, 7.162408, 7.15811, 7.15978, 7.17642, 7.18204, 7.28632, 7.32028,
			7.31351, 7.33571, 7.34541, 7.35738, 7.32072, 7.30753, 7.2478, 7.14795, 7.14887, 7.252622, 7.20054,
			7.25192, 7.35922, 7.341, 7.37933, 7.37876, 7.37697, 7.45716, 7.36464, 7.4504, 7.45143, 7.36067,
			7.41883, 7.421, 7.41594, 7.40918, 7.38633, 7.29623, 7.29168, 7.26777, 7.27914, 7.31966, 7.91658,
			7.92765, 7.9185, 7.92542, 7.90639, 7.90804, 7.89542, 7.89873, 7.90644, 7.87852, 7.88098, 7.87734,
			8.18953, 8.18741, 8.18058, 8.17729, 8.13038, 8.57747, 8.58097, 8.59801, 8.60706, 8.60288, 8.58362,
			8.58405, 8.62938, 8.62854, 8.58389, 8.50865, 8.45094, 8.46585, 8.52864, 8.54609, 8.23937, 8.23678,
			8.23495, 8.23771, 8.23207, 8.23433, 8.14196, 8.23279, 8.36558, 8.37008, 8.38178, 8.33391, 8.39448,
			8.42099, 8.48014, 8.37706, 8.40356, 8.52184, 8.52814, 8.53887, 8.5414, 8.53032, 8.30174, 8.30378,
			8.31265, 8.30675, 8.29185),
		elevation = c(795, 776, 798, 797, 796, 791, 776, 785, 788, 783, 748, 759, 757, 780, 743, 741, 754,
			753, 773, 752, 702, 710, 710, 704, 708, 705, 710, 702, 711, 713, 682, 665, 665, 663, 691, 696,
			664, 670, 658, 643, 681, 682, 720, 664, 665, 666, 665, 700, 651, 678, 699, 672, 718, 721, 722,
			721, 714, 708, 719, 718, 680, 683, 676, 670, 673, 669, 672, NA, NA, NA, NA, 641, 637, 696, 683,
			681, 679, 674, 672, 688, 612, 615, 580, 571, 584, NA, 490, 489, 491, 496, 493, 468, 479, 477, 495,
			603, 639, 629, 631, 649, 732, 650, 639, 639, 647, 689, 685, 696, 717, 707, 690, 697, 712, 649,
			704, 705, 700, 730, 711, 622, 608, 610, 614, 615, 638, 691, 655, 667, 694, 652, 655, 655, 644,
			638, 658, 660, 668, 671, 635, 636, 634, 673, 664, 642, 646, 629, 635, 641, 629, 631, 632, 649,
			648, 650, 657, 677, 755, 761, 753, 750, 751, 737, 748, 772, 741, 762, 795, 781, 781, 781, 789,
			690, 673, 682, 691, 691, 687, 665, 682, 616, 619, 607, 602, 579, 598, 629, 597, 589, 477, 490,
			459, 446, 489, 499, 497, 498, 500, 505)
	)
	d <- merge(d, geo, by="trial_id", all.x=TRUE)
	d$geo_from_source <- TRUE

	d$seed_weight[d$seed_weight == 0] <- NA
	d$seed_weight[d$seed_weight > 1000] <- NA
	
	carobiner::write_files(path, meta, d)
}

