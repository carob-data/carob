# R script for "carob"
# license: GPL (>=3)

## NOTES: 
# 5 sites: BEOU (Benin), ETBA (Ethiopia), GHKP (Ghana), MANT (Mali), RWBU (Rwanda), 2 experiments (seasons) each. 
# GHKP TRT_NAME encodes N-P rate and residue as free text, not parsed. 
# RWBU soil_id does not follow the SITEyyNNNN pattern used elsewhere. 
# RWBU initial-conditions layer block has a stray value outside the defined columns, left as NA rather than guessed. 
# BEOU soils sheet showed only a profile summary row, no deeper layer table - needs re-checking against the raw data

## SUGGESTED NEW TERMINAG TERMS
#        soil_sample_date (pre-planting soil sample); tillage_date; tillage_depth;
#        planting_depth; land_prep_traction (tillage power source, e.g. animal-drawn);
#        crop_N, crop_P (in-season crop nutrient content); soil_water_total,
#        soil_N_total (whole-profile totals, distinct from the depth-resolved
#        soil_water_layer/soil_N_layer); *_sd companion columns


carob_script <- function(path) {
  
"Calibration data for: Modelling climate change impacts on maize yields under
low nitrogen input conditions in sub-Saharan Africa
The dataset contains experimental data on maize growth for crop model
calibration, at five sites in sub-Saharan Africa (Benin, Ethiopia, Ghana,
Mali, Rwanda). Weather, soil, field management, initial conditions and
observed crop growth (daily, soil layers, and season-end summary, each
with average and standard deviation) are provided for 10 experiments
(2 seasons per site)."
  
  uri <- "doi:10.18167/DVN1/CIN8PB"
  group <- "agronomy"
  
  ff <- carobiner::get_data(uri, path, group, filter=F)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                  data_organization = "CIRAD; APNI; CIMMYT; ICRISAT",
                                  publication = NA,
                                  project = NA,
                                  carob_date = "2026-08-03",
                                  design = "on-farm calibration trials, 2 seasons per site, 5 sites",
                                  data_type = NA,
                                  treatment_vars = "N_fertilizer; P_fertilizer; K_fertilizer", 
                                  response_vars = "yield; dmy_total; LAI; crop_N; crop_P; soil_water_total; soil_N_total",
                                  carob_contributor = "Stella Muthoni",
                                  notes = NA,
                                  carob_completion = 70,
                                  carob_effort = 16
  )
  
  f1  <- ff[basename(ff) == "Agmip_LowInput_template_variable_definition0717-2.xlsx"]
  
  #Field management, soil and daily weather
  f2  <- ff[basename(ff) == "BEOU_field_mgmt_soils_weather070518LI-2.xlsx"] #Benin 
  f3  <- ff[basename(ff) == "ETBA_field_mgmt_soils_weather070518LI-2.xlsx"]
  f4  <- ff[basename(ff) == "GHKP_field_mgmt_soils_weather071718LI-2.xlsx"]
  f5  <- ff[basename(ff) == "MANT_field_mgmt_soils_weather062018LI-2.xlsx"]
  f6  <- ff[basename(ff) == "RWBU_field_mgmt_soils_weather060818LI-2.xlsx"]
  
  #
  f7  <- ff[basename(ff) == "AgMIP_Smallholder_Observation_daily_average.txt"]
  f8  <- ff[basename(ff) == "AgMIP_Smallholder_Observation_daily_sd.txt"]
  f9  <- ff[basename(ff) == "AgMIP_Smallholder_Observation_SoilWN_layers_average.txt"]
  f10 <- ff[basename(ff) == "AgMIP_Smallholder_Observation_SoilWN_layers_sd.txt"]
  f11 <- ff[basename(ff) == "AgMIP_Smallholder_Observation_summary_average.txt"]
  f12 <- ff[basename(ff) == "AgMIP_Smallholder_Observation_summary_sd.txt"]
  
  f13 <- ff[basename(ff) == "readme_dataverse.txt"]
  
  r1a <- carobiner::read.excel(f1, sheet="Summary")
  r1b <- carobiner::read.excel(f1, sheet="Daily")
  
  
  ############################################################
  # BEOU (Benin)
  ############################################################
  
  ### Reading small tables for Benin farm management, soils and weather
  r2a_exp  <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=19, n_max=2)
  
  r2a_prevcrop <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=27, n_max=2)
  r2a_prevcrop$exname <- ifelse(r2a_prevcrop$`!...1` == 1, "BEOU1501", "BEOU1401")
  r2a_prevcrop$icdat  <- as.Date(r2a_prevcrop$icdat)
  
  r2a_iclayers <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=33, n_max=6)
  r2a_iclayers$exname <- ifelse(r2a_iclayers$`!...1` == 1, "BEOU1501", "BEOU1401")
  
  r2a_tillage <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=44, n_max=2)
  r2a_tillage$exname <- ifelse(r2a_tillage$`! Definitions` == 1, "BEOU1501", "BEOU1401")
  r2a_tillage$`yyyy-mm-dd` <- as.Date(r2a_tillage$`yyyy-mm-dd`)
  
  r2a_planting <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=48, n_max=2)
  r2a_planting$exname <- ifelse(r2a_planting$`! Definitions` == 1, "BEOU1501", "BEOU1401")
  r2a_planting$`yyyy-mm-dd` <- as.Date(r2a_planting$`yyyy-mm-dd`)
  
  r2a_summary  <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=67, n_max=2)
  r2a_summary$exname <- ifelse(r2a_summary$`#` == 1, "BEOU1501", "BEOU1401")
  r2a_summary$pldae <- as.Date(r2a_summary$pldae)
  r2a_summary$adat  <- as.Date(r2a_summary$adat)
  r2a_summary$mdat  <- as.Date(r2a_summary$mdat)
  
  r2b_profile <- carobiner::read.excel(f2, sheet="Soils", skip=12, n_max=1)
  r2b_layers  <- carobiner::read.excel(f2, sheet="Soils", skip=17, n_max=3)
  
  r2c_station <- carobiner::read.excel(f2, sheet="Weather", skip=3, n_max=1)
  r2c_daily   <- carobiner::read.excel(f2, sheet="Weather", skip=9, n_max=730)
  r2c_daily$w_date <- as.Date(r2c_daily$w_date)
  r2c_daily$exname <- ifelse(r2c_daily$`!YEAR` == 2014, "BEOU1401", "BEOU1501")
  
  # small data.frames, one per source table
  d2_exp <- data.frame(
    trial_id  = r2a_exp$exname,
    location_id = "BEOU",
    country   = "Benin",
    location  = tools::toTitleCase(tolower(r2a_exp$site_name[1])),
    latitude  = r2a_exp$fl_lat,
    longitude = r2a_exp$fl_long,
    elevation = r2a_exp$flele,
    geo_from_source = TRUE,
    is_survey = FALSE,
    on_farm = TRUE,
    treatment = r2a_exp$TRT_NAME[1],
    station_id = r2a_exp$wst_id[1]
  )
  d2_prevcrop <- data.frame(
    trial_id = r2a_prevcrop$exname,
    soil_sample_date = r2a_prevcrop$icdat,
    previous_crop = tolower(r2a_prevcrop$`!...5`),
    residue_prevcrop = r2a_prevcrop$icrag + r2a_prevcrop$icrt,
    residue_prevcrop_used = (r2a_prevcrop$icrag + r2a_prevcrop$icrt) > 0
  )
  d2_tillage <- data.frame(
    trial_id = r2a_tillage$exname,
    land_prep_traction = r2a_tillage$text,
    tillage_date = r2a_tillage$`yyyy-mm-dd`,
    tillage_depth = r2a_tillage$cm
  )
  d2_planting <- data.frame(
    trial_id = r2a_planting$exname,
    planting_date = r2a_planting$`yyyy-mm-dd`,
    variety = r2a_planting$text...10,
    variety_type = r2a_planting$text...12,
    plant_density = r2a_planting$`#/m2` * 10000,
    row_spacing = r2a_planting$cm,
    planting_depth = r2a_planting$mm
  )
  d2_summary <- data.frame(
    trial_id = r2a_summary$exname,
    emergence_date = r2a_summary$pldae,
    flowering_date = r2a_summary$adat,
    maturity_date = r2a_summary$mdat
  )
  
  d2a <- Reduce(function(x, y) merge(x, y, by = "trial_id"),
                list(d2_exp, d2_prevcrop, d2_tillage, d2_planting, d2_summary))
  
  # single confirmed facts (explicit "!No fertiliser" / "!No organic amendment" in source)
  d2a$crop <- "maize"
  d2a$irrigated <- FALSE
  d2a$OM_used <- FALSE
  d2a$N_organic <- NA
  d2a$P_organic <- NA
  d2a$K_organic <- NA
  d2a$N_fertilizer <- NA
  d2a$P_fertilizer <- NA
  d2a$K_fertilizer <- NA
  d2a$N_splits <- NA
  d2a$fertilizer_amount <- NA
  d2a$fertilization_method <- NA
  
  # soil for BEOU - kept SEPARATE, combined across sites later
  r2b_beou <- data.frame(
    location_id = "BEOU",
    soil_id = "BEOU150001",
    soil_texture = r2b_profile$sltx,
    max_rooting_depth = 60,   # source profile row: "Maximum rooting depth"
    max_water_depth = 60,     # "Maximum water measurement depth"
    max_N_depth = 60,         # "Maximum Nitrogen measurement depth"
    depth = r2b_layers$sllb,  # each layer's own sample depth - distinct from the 3 max-depths above
    soil_bd     = r2b_layers$slbdm,
    soil_SOC    = r2b_layers$sloc,
    soil_clay   = r2b_layers$slcly,
    soil_silt   = r2b_layers$slsil,
    soil_gravel = r2b_layers$slcf,
    soil_CEC    = r2b_layers$slcec,
    soil_P      = r2b_layers$slpx,
    soil_K_exch = r2b_layers$slke,
    soil_pH     = r2b_layers$slhw,
    soil_N      = r2b_layers$slni * 10000,
    soil_FC     = r2b_layers$sldul,
    soil_PWP    = r2b_layers$slll,
    soil_saturation = r2b_layers$slsat
  )
  
  # weather for BEOU - kept SEPARATE, combined across sites later
  r2c_beou <- data.frame(
    location_id = "BEOU",
    date = r2c_daily$w_date,
    tmax = r2c_daily$tmax,
    tmin = r2c_daily$tmin,
    prec = r2c_daily$rain,
    srad = r2c_daily$srad,
    wspd = r2c_daily$wind /86.4,  # km/d -> m/s
    rhum = r2c_daily$rhum
  )
  
  # initial conditions (iclayers) - depth-resolved, one date per experiment
  r2_iclayers_long <- data.frame(
    trial_id = r2a_iclayers$exname,
    location_id = "BEOU",
    depth = r2a_iclayers$`icbl...2`,
    date = d2_prevcrop$soil_sample_date[match(r2a_iclayers$exname, d2_prevcrop$trial_id)],
    soil_NH4 = r2a_iclayers$icnh4,
    soil_NO3 = r2a_iclayers$icno3,
    soil_GWC = r2a_iclayers$ich2o / r2b_layers$slbdm[match(r2a_iclayers$`icbl...2`, r2b_layers$sllb)]
  )
  
  
  ############################################################
  # ETBA (Ethiopia)
  ############################################################
  #Reading small tables for Ethiopia
  r3a_exp      <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=19, n_max=2)
  
  r3a_prevcrop <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=27, n_max=2)
  r3a_prevcrop$exname <- ifelse(r3a_prevcrop$`!...1` == 1, "ETBA1401", "ETBA1301")
  r3a_prevcrop$icdat  <- as.Date(r3a_prevcrop$icdat)
  
  r3a_iclayers <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=33, n_max=14)
  r3a_iclayers$exname <- ifelse(r3a_iclayers$`!` == 1, "ETBA1401", "ETBA1301")
  
  r3a_tillage <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=51, n_max=2)
  r3a_tillage$exname <- c("ETBA1401", "ETBA1301")
  r3a_tillage$`yyyy-mm-dd` <- as.Date(r3a_tillage$`yyyy-mm-dd`)
  
  r3a_planting <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=55, n_max=2)
  r3a_planting$exname <- c("ETBA1401", "ETBA1301")
  r3a_planting$`yyyy-mm-dd` <- as.Date(r3a_planting$`yyyy-mm-dd`)
  
  r3a_fert <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=66, n_max=6)
  r3a_fert$exname <- ifelse(r3a_fert$`%` == 1, "ETBA1401", "ETBA1301")
  r3a_fert$`yyyy-mm-dd` <- as.Date(r3a_fert$`yyyy-mm-dd`)
  fert_totals_3 <- aggregate(cbind(`kg[N]/ha`, `kg[P]/ha`, `kg[K]/ha`) ~ exname, data = r3a_fert, sum)
  names(fert_totals_3) <- c("trial_id", "N_fertilizer", "P_fertilizer", "K_fertilizer")
  n_splits_3 <- aggregate(`kg[N]/ha` ~ exname, data = r3a_fert[r3a_fert$`kg[N]/ha` > 0, ], length)
  names(n_splits_3) <- c("trial_id", "N_splits")
  r3a_fert$product_kg <- ifelse(r3a_fert$`!Fertilizer type - code signification` == "Di-ammonium Phosphate", r3a_fert$`kg[N]/ha` / 0.18,
                                ifelse(r3a_fert$`!Fertilizer type - code signification` == "Urea", r3a_fert$`kg[N]/ha` / 0.46, NA))
  fert_amount_3 <- aggregate(product_kg ~ exname, data = r3a_fert, sum)
  names(fert_amount_3) <- c("trial_id", "fertilizer_amount")
  
  r3a_summary  <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=79, n_max=2)
  r3a_summary$exname <- ifelse(r3a_summary$`#` == 1, "ETBA1401", "ETBA1301")
  r3a_summary$pldae  <- as.Date(r3a_summary$pldae)
  r3a_summary$adat   <- as.Date(r3a_summary$adat)
  r3a_summary$mdat   <- as.Date(r3a_summary$mdat)
  
  r3b_profile <- carobiner::read.excel(f3, sheet="Soils", skip=12, n_max=1)
  r3b_layers  <- carobiner::read.excel(f3, sheet="Soils", skip=17, n_max=7)
  
  r3c_station <- carobiner::read.excel(f3, sheet="Weather", skip=3, n_max=1)
  r3c_daily   <- carobiner::read.excel(f3, sheet="Weather", skip=9, n_max=730)
  r3c_daily$w_date <- as.Date(r3c_daily$w_date)
  r3c_daily$exname <- ifelse(r3c_daily$`!YEAR` == 2013, "ETBA1301", "ETBA1401")
  
  d3_exp <- data.frame(
    trial_id  = r3a_exp$exname,
    location_id = "ETBA",
    country   = "Ethiopia",
    location  = tools::toTitleCase(tolower(r3a_exp$site_name[1])),
    latitude  = r3a_exp$fl_lat,
    longitude = r3a_exp$fl_long,
    elevation = r3a_exp$flele,
    geo_from_source = TRUE,
    is_survey = FALSE,
    on_farm = TRUE,
    treatment = r3a_exp$TRT_NAME[1],
    station_id = r3a_exp$wst_id[1]
  )
  d3_prevcrop <- data.frame(
    trial_id = r3a_prevcrop$exname,
    soil_sample_date = r3a_prevcrop$icdat,
    previous_crop = tolower(r3a_prevcrop$`!...5`),
    residue_prevcrop = r3a_prevcrop$icrag + r3a_prevcrop$icrt,
    residue_prevcrop_used = (r3a_prevcrop$icrag + r3a_prevcrop$icrt) > 0
  )
  d3_tillage <- data.frame(
    trial_id = r3a_tillage$exname,
    land_prep_traction = r3a_tillage$text,
    tillage_date = r3a_tillage$`yyyy-mm-dd`,
    tillage_depth = r3a_tillage$cm
  )
  d3_planting <- data.frame(
    trial_id = r3a_planting$exname,
    planting_date = r3a_planting$`yyyy-mm-dd`,
    variety = "ETBA_CUL",
    variety_type = "Hybrid",
    plant_density = r3a_planting$`#/m2` * 10000,
    row_spacing = r3a_planting$cm,
    planting_depth = r3a_planting$mm
  )
  d3_summary <- data.frame(
    trial_id = r3a_summary$exname,
    emergence_date = as.Date(NA),  # not recorded at ETBA
    flowering_date = r3a_summary$adat,
    maturity_date = r3a_summary$mdat
  )
  
  d3a <- Reduce(function(x, y) merge(x, y, by = "trial_id"),
                list(d3_exp, d3_prevcrop, d3_tillage, d3_planting, d3_summary,
                     fert_totals_3, n_splits_3, fert_amount_3))
  
  d3a$crop <- "maize"
  d3a$irrigated <- FALSE
  d3a$OM_used <- FALSE
  d3a$N_organic <- NA
  d3a$P_organic <- NA
  d3a$K_organic <- NA
  d3a$fertilization_method <- "Broadcast, incorporated"
  
  r3b_etba <- data.frame(
    location_id = "ETBA",
    soil_id = "ETBA140001",
    soil_texture = r3b_profile$sltx,
    max_rooting_depth = 120,
    max_water_depth = 100,
    max_N_depth = 0,
    depth = r3b_layers$sllb,
    soil_bd     = r3b_layers$slbdm,
    soil_SOC    = r3b_layers$sloc,
    soil_clay   = r3b_layers$slcly,
    soil_silt   = r3b_layers$slsil,
    soil_gravel = r3b_layers$slcf,
    soil_CEC    = r3b_layers$slcec,
    soil_P      = r3b_layers$slpx,
    soil_K_exch = r3b_layers$slke,
    soil_pH     = r3b_layers$slhw,
    soil_N      = r3b_layers$slni * 10000,
    soil_FC     = r3b_layers$sldul,
    soil_PWP    = r3b_layers$slll,
    soil_saturation = r3b_layers$slsat
  )
  
  r3c_etba <- data.frame(
    location_id = "ETBA",
    date = r3c_daily$w_date,
    tmax = r3c_daily$tmax,
    tmin = r3c_daily$tmin,
    prec = r3c_daily$rain,
    srad = r3c_daily$srad,
    wspd = r3c_daily$wind/86.4,  # km/d -> m/s,
    rhum = r3c_daily$rhum
  )
  
  r3_iclayers_long <- data.frame(
    trial_id = r3a_iclayers$exname,
    location_id = "ETBA",
    depth = r3a_iclayers$icbl,
    date = d3_prevcrop$soil_sample_date[match(r3a_iclayers$exname, d3_prevcrop$trial_id)],
    soil_NH4 = r3a_iclayers$icnh4,
    soil_NO3 = r3a_iclayers$icno3,
    soil_GWC = r3a_iclayers$ich2o / r3b_layers$slbdm[match(r3a_iclayers$icbl, r3b_layers$sllb)]
  )
  
  ############################################################
  # GHKP (Ghana)
  ############################################################
  # Reading small tables for Ghana
  r4a_exp      <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=19, n_max=2)
  r4a_prevcrop <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=27, n_max=2)
  r4a_prevcrop$exname <- ifelse(r4a_prevcrop$`!...1` == 1, "GHKP0801", "GHKP0901")
  r4a_prevcrop$icdat  <- as.Date(r4a_prevcrop$icdat)
  
  r4a_iclayers <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=33, n_max=12)
  r4a_iclayers$exname <- ifelse(r4a_iclayers$`!...1` == 1, "GHKP0801", "GHKP0901")
  
  # GHKP has no tillage - confirmed "!No Tillage" in source
  
  r4a_planting <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=53, n_max=2)
  r4a_planting$exname <- ifelse(r4a_planting$`! Definitions` == 1, "GHKP0801", "GHKP0901")
  r4a_planting$`yyyy-mm-dd` <- as.Date(r4a_planting$`yyyy-mm-dd`)
  
  r4a_fert <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=64, n_max=8)
  r4a_fert$exname <- ifelse(r4a_fert$`%` == 1, "GHKP0801", "GHKP0901")
  r4a_fert$product_kg <- ifelse(r4a_fert$`!Fertilizer type - code signification` == "Urea", r4a_fert$`kg[N]/ha` / 0.46,
                                ifelse(r4a_fert$`!Fertilizer type - code signification` == "Triple Super Phosphate", r4a_fert$`kg[P]/ha` / 0.1923,
                                       r4a_fert$`kg[K]/ha` / 0.498))
  fert_totals_4 <- aggregate(cbind(`kg[N]/ha`, `kg[P]/ha`, `kg[K]/ha`) ~ exname, data = r4a_fert, sum)
  names(fert_totals_4) <- c("trial_id", "N_fertilizer", "P_fertilizer", "K_fertilizer")
  n_splits_4 <- aggregate(`kg[N]/ha` ~ exname, data = r4a_fert[r4a_fert$`kg[N]/ha` > 0, ], length)
  names(n_splits_4) <- c("trial_id", "N_splits")
  fert_amount_4 <- aggregate(product_kg ~ exname, data = r4a_fert, sum)
  names(fert_amount_4) <- c("trial_id", "fertilizer_amount")
  
  r4a_summary <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=79, n_max=2)
  r4a_summary$exname <- ifelse(r4a_summary$`#` == 1, "GHKP0801", "GHKP0901")
  r4a_summary$pldae  <- as.Date(r4a_summary$pldae)
  r4a_summary$adat   <- as.Date(r4a_summary$adat)
  r4a_summary$mdat   <- as.Date(r4a_summary$mdat)
  
  r4b_profile <- carobiner::read.excel(f4, sheet="Soils", skip=12, n_max=2)   # two profiles
  r4b_layers  <- carobiner::read.excel(f4, sheet="Soils", skip=18, n_max=12)  # A's 6 depths then C's 6 depths
  r4b_layers$soil_id <- ifelse(r4b_layers$`%` == "A", "GHKP080001", "GHKP090001")
  
  r4c_station <- carobiner::read.excel(f4, sheet="Weather", skip=3, n_max=1)
  r4c_daily <- carobiner::read.excel(f4, sheet="Weather", skip=9, n_max=730)
  r4c_daily$w_date <- as.Date(r4c_daily$w_date)
  r4c_daily$exname <- ifelse(r4c_daily$`!YEAR` == 2008, "GHKP0801", "GHKP0901")
  
  d4_exp <- data.frame(
    trial_id  = r4a_exp$exname,
    location_id = "GHKP",
    country   = "Ghana",
    location  = tools::toTitleCase(tolower(r4a_exp$site_name[1])),
    latitude  = r4a_exp$fl_lat,
    longitude = r4a_exp$fl_long,
    elevation = r4a_exp$flele,
    geo_from_source = TRUE,
    is_survey = FALSE,
    on_farm = TRUE,
    treatment = r4a_exp$TRT_NAME,   # GHKP: TRT_NAME differs per experiment
    station_id = r4a_exp$wst_id[1]
  )
  d4_prevcrop <- data.frame(
    trial_id = r4a_prevcrop$exname,
    soil_sample_date = r4a_prevcrop$icdat,
    previous_crop = tolower(r4a_prevcrop$`!...5`),
    residue_prevcrop = r4a_prevcrop$icrag + r4a_prevcrop$icrt,
    residue_prevcrop_used = (r4a_prevcrop$icrag + r4a_prevcrop$icrt) > 0
  )
  d4_planting <- data.frame(
    trial_id = r4a_planting$exname,
    planting_date = r4a_planting$`yyyy-mm-dd`,
    variety = "GHKP_CUL",
    variety_type = "Open Pollinated Variety (OPV)",
    plant_density = r4a_planting$`#/m2` * 10000,
    row_spacing = r4a_planting$cm,
    planting_depth = r4a_planting$mm
  )
  d4_summary <- data.frame(
    trial_id = r4a_summary$exname,
    emergence_date = r4a_summary$pldae,
    flowering_date = r4a_summary$adat,
    maturity_date = r4a_summary$mdat
  )
  
  d4a <- Reduce(function(x, y) merge(x, y, by = "trial_id"),
                list(d4_exp, d4_prevcrop, d4_planting, d4_summary,
                     fert_totals_4, n_splits_4, fert_amount_4))
  
  # no tillage at GHKP - confirmed "!No Tillage" in source
  d4a$land_prep_traction <- NA
  d4a$tillage_date <- as.Date(NA)
  d4a$tillage_depth <- NA
  
  d4a$crop <- "maize"
  d4a$irrigated <- FALSE
  d4a$OM_used <- FALSE
  d4a$N_organic <- NA
  d4a$P_organic <- NA
  d4a$K_organic <- NA
  d4a$fertilization_method <- "Broadcast, incorporated"
  
  r4b_ghkp <- data.frame(
    location_id = "GHKP",
    soil_id = r4b_layers$soil_id,
    soil_texture = r4b_profile$sltx[match(r4b_layers$soil_id, ifelse(r4b_profile$`%`=="A","GHKP080001","GHKP090001"))],
    max_rooting_depth = 100,
    max_water_depth = 100,
    max_N_depth = 100,
    depth = r4b_layers$sllb,
    soil_bd     = r4b_layers$slbdm,
    soil_SOC    = r4b_layers$sloc,
    soil_clay   = r4b_layers$slcly,
    soil_silt   = r4b_layers$slsil,
    soil_gravel = r4b_layers$slcf,
    soil_CEC    = r4b_layers$slcec,
    soil_P      = r4b_layers$slpx,
    soil_K_exch = r4b_layers$slke,
    soil_pH     = r4b_layers$slhw,
    soil_N      = r4b_layers$slni * 10000,
    soil_FC     = r4b_layers$sldul,
    soil_PWP    = r4b_layers$slll,
    soil_saturation = r4b_layers$slsat
  )
  
  r4c_ghkp <- data.frame(
    location_id = "GHKP",
    date = r4c_daily$w_date,
    tmax = r4c_daily$tmax,
    tmin = r4c_daily$tmin,
    prec = r4c_daily$rain,
    srad = r4c_daily$srad,
    wspd = r4c_daily$wind/86.4,  # km/d -> m/s,
    rhum = r4c_daily$rhum
  )
  
  r4_iclayers_long <- data.frame(
    trial_id = r4a_iclayers$exname,
    location_id = "GHKP",
    depth = r4a_iclayers$`icbl...2`,
    date = d4_prevcrop$soil_sample_date[match(r4a_iclayers$exname, d4_prevcrop$trial_id)],
    soil_NH4 = r4a_iclayers$icnh4,
    soil_NO3 = r4a_iclayers$icno3,
    soil_GWC = r4a_iclayers$ich2o / r4b_layers$slbdm[match(
      paste(ifelse(r4a_iclayers$exname=="GHKP0801","GHKP080001","GHKP090001"), r4a_iclayers$`icbl...2`),
      paste(r4b_layers$soil_id, r4b_layers$sllb))]
  )
  
  ############################################################
  # MANT (Mali)
  ############################################################
  #Reading small tables for Mali
  r5a_exp      <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=19, n_max=2)
  r5a_prevcrop <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=27, n_max=2)
  r5a_prevcrop$exname <- ifelse(r5a_prevcrop$`!...1` == 1, "MANT1001", "MANT0901")
  r5a_prevcrop$icdat  <- as.Date(r5a_prevcrop$icdat)
  
  r5a_iclayers <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=33, n_max=14)
  r5a_iclayers$exname <- ifelse(r5a_iclayers$`!...1` == 1, "MANT1001", "MANT0901")
  
  r5a_tillage <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=51, n_max=3)
  r5a_tillage$exname <- ifelse(r5a_tillage$`! Definitions` == 1, "MANT1001", "MANT0901")
  r5a_tillage$`yyyy-mm-dd` <- as.Date(r5a_tillage$`yyyy-mm-dd`)
  
  r5a_planting <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=55, n_max=3)
  r5a_planting$exname <- ifelse(r5a_planting$`! Definitions` == 1, "MANT1001", "MANT0901")
  r5a_planting$`yyyy-mm-dd` <- as.Date(r5a_planting$`yyyy-mm-dd`)
  
  r5a_organic <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=64, n_max=3)
  r5a_organic$exname <- ifelse(r5a_organic$`%...1` == 1, "MANT1001", "MANT0901")
  r5a_organic$`yyyy-mm-dd` <- as.Date(r5a_organic$`yyyy-mm-dd`)
  r5a_organic$N_organic_kg <- r5a_organic$`kg/ha` * r5a_organic$`%...15` / 100
  r5a_organic$P_organic_kg <- r5a_organic$`kg/ha` * r5a_organic$`%...17` / 100
  r5a_organic$K_organic_kg <- r5a_organic$`kg/ha` * r5a_organic$`%...19` / 100
  
  r5a_fert <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=70, n_max=5)
  r5a_fert$exname <- ifelse(r5a_fert$`%` == 1, "MANT1001", "MANT0901")
  r5a_fert$`yyyy-mm-dd` <- as.Date(r5a_fert$`yyyy-mm-dd`)
  fert_totals_5 <- aggregate(cbind(`kg[N]/ha`, `kg[P]/ha`, `kg[K]/ha`) ~ exname, data = r5a_fert, sum)
  names(fert_totals_5) <- c("trial_id", "N_fertilizer", "P_fertilizer", "K_fertilizer")
  n_splits_5 <- aggregate(`kg[N]/ha` ~ exname, data = r5a_fert[r5a_fert$`kg[N]/ha` > 0, ], length)
  names(n_splits_5) <- c("trial_id", "N_splits")
  r5a_fert$product_kg <- ifelse(r5a_fert$`!Fertilizer type - code signification` == "Urea", r5a_fert$`kg[N]/ha` / 0.46, NA)
  fert_amount_5 <- aggregate(product_kg ~ exname, data = r5a_fert, sum, na.rm = TRUE)
  names(fert_amount_5) <- c("trial_id", "fertilizer_amount")
  
  r5a_summary <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=86, n_max=2)
  r5a_summary$exname <- ifelse(r5a_summary$`#` == 1, "MANT1001", "MANT0901")
  r5a_summary$pldae  <- as.Date(r5a_summary$pldae)
  r5a_summary$adat   <- as.Date(r5a_summary$adat)
  r5a_summary$mdat   <- as.Date(r5a_summary$mdat)
  
  r5b_profile <- carobiner::read.excel(f5, sheet="Soils", skip=12, n_max=1)
  r5b_layers  <- carobiner::read.excel(f5, sheet="Soils", skip=17, n_max=7)
  
  r5c_station <- carobiner::read.excel(f5, sheet="Weather", skip=3, n_max=1)
  r5c_daily   <- carobiner::read.excel(f5, sheet="Weather", skip=9, n_max=730)
  r5c_daily$w_date <- as.Date(r5c_daily$w_date)
  r5c_daily$exname <- ifelse(r5c_daily$`!YEAR` == 2009, "MANT0901", "MANT1001")
  
  d5_exp <- data.frame(
    trial_id  = r5a_exp$exname,
    location_id = "MANT",
    country   = "Mali",
    location  = tools::toTitleCase(tolower(r5a_exp$site_name[1])),
    latitude  = r5a_exp$fl_lat,
    longitude = r5a_exp$fl_long,
    elevation = r5a_exp$flele,
    geo_from_source = TRUE,
    is_survey = FALSE,
    on_farm = TRUE,
    treatment = r5a_exp$TRT_NAME[1],
    station_id = r5a_exp$wst_id[1]
  )
  d5_prevcrop <- data.frame(
    trial_id = r5a_prevcrop$exname,
    soil_sample_date = r5a_prevcrop$icdat,
    previous_crop = tolower(r5a_prevcrop$`!...5`),
    residue_prevcrop = r5a_prevcrop$icrag + r5a_prevcrop$icrt,
    residue_prevcrop_used = (r5a_prevcrop$icrag + r5a_prevcrop$icrt) > 0
  )
  d5_tillage <- data.frame(
    trial_id = r5a_tillage$exname,
    land_prep_traction = r5a_tillage$text,
    tillage_date = r5a_tillage$`yyyy-mm-dd`,
    tillage_depth = r5a_tillage$cm
  )
  d5_planting <- data.frame(
    trial_id = r5a_planting$exname,
    planting_date = r5a_planting$`yyyy-mm-dd`,
    variety = "MANT_CUL",
    variety_type = "Open Pollinated Variety (OPV)",
    plant_density = r5a_planting$`#/m2` * 10000,
    row_spacing = r5a_planting$cm,
    planting_depth = r5a_planting$mm
  )
  d5_organic <- data.frame(
    trial_id = r5a_organic$exname,
    OM_used = TRUE,
    N_organic = r5a_organic$N_organic_kg,
    P_organic = r5a_organic$P_organic_kg,
    K_organic = r5a_organic$K_organic_kg
  )
  d5_summary <- data.frame(
    trial_id = r5a_summary$exname,
    emergence_date = as.Date(NA),  # not recorded at MANT
    flowering_date = r5a_summary$adat,
    maturity_date = r5a_summary$mdat
  )
  
  d5a <- Reduce(function(x, y) merge(x, y, by = "trial_id"),
                list(d5_exp, d5_prevcrop, d5_tillage, d5_planting, d5_organic, d5_summary,
                     fert_totals_5, n_splits_5, fert_amount_5))
  
  d5a$crop <- "maize"
  d5a$irrigated <- FALSE
  d5a$fertilization_method <- "Broadcast, incorporated"
  
  r5b_mant <- data.frame(
    location_id = "MANT",
    soil_id = "MANT100001",
    soil_texture = r5b_profile$sltx,
    max_rooting_depth = 120,
    max_water_depth = 80,
    max_N_depth = 0,
    depth = r5b_layers$sllb,
    soil_bd     = r5b_layers$slbdm,
    soil_SOC    = r5b_layers$sloc,
    soil_clay   = r5b_layers$slcly,
    soil_silt   = r5b_layers$slsil,
    soil_gravel = r5b_layers$slcf,
    soil_CEC    = r5b_layers$slcec,
    soil_P      = r5b_layers$slpx,
    soil_K_exch = r5b_layers$slke,
    soil_pH     = r5b_layers$slhw,
    soil_N      = r5b_layers$slni * 10000,
    soil_FC     = r5b_layers$sldul,
    soil_PWP    = r5b_layers$slll,
    soil_saturation = r5b_layers$slsat
  )
  
  r5c_mant <- data.frame(
    location_id = "MANT",
    date = r5c_daily$w_date,
    tmax = r5c_daily$tmax,
    tmin = r5c_daily$tmin,
    prec = r5c_daily$rain,
    srad = r5c_daily$srad,
    wspd = r5c_daily$wind/86.4,  # km/d -> m/s,
    rhum = r5c_daily$rhum
  )
  
  r5_iclayers_long <- data.frame(
    trial_id = r5a_iclayers$exname,
    location_id = "MANT",
    depth = r5a_iclayers$icbl,
    date = d5_prevcrop$soil_sample_date[match(r5a_iclayers$exname, d5_prevcrop$trial_id)],
    soil_NH4 = r5a_iclayers$icnh4,
    soil_NO3 = r5a_iclayers$icno3,
    soil_GWC = r5a_iclayers$ich2o / r5b_layers$slbdm[match(r5a_iclayers$icbl, r5b_layers$sllb)]
  )
  
  ############################################################
  # RWBU (Rwanda)
  ############################################################
  # Reading small tables for Rwanda
  r6a_exp      <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=19, n_max=2)
  r6a_prevcrop <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=27, n_max=2)
  r6a_prevcrop$exname <- ifelse(r6a_prevcrop$`!...1` == 1, "RWBU1501", "RWBU1401")
  r6a_prevcrop$icdat  <- as.Date(r6a_prevcrop$icdat)
  
  r6a_iclayers <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=33, n_max=12)
  r6a_iclayers$exname <- ifelse(r6a_iclayers$`!` == 1, "RWBU1501", "RWBU1401")
  # icnh4/icno3 NA in source for RWBU
  
  r6a_tillage <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=50, n_max=2)
  r6a_tillage$exname <- c("RWBU1501", "RWBU1401")
  r6a_tillage$`yyyy-mm-dd` <- as.Date(r6a_tillage$`yyyy-mm-dd`)
  
  r6a_planting <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=54, n_max=2)
  r6a_planting$exname <- c("RWBU1501", "RWBU1401")
  r6a_planting$`yyyy-mm-dd` <- as.Date(r6a_planting$`yyyy-mm-dd`)
  
  r6a_fert <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=65, n_max=5)
  r6a_fert$exname <- ifelse(r6a_fert$`%` == 1, "RWBU1501", "RWBU1401")
  r6a_fert$`yyyy-mm-dd` <- as.Date(r6a_fert$`yyyy-mm-dd`)
  fert_totals_6 <- aggregate(cbind(`kg[N]/ha`, `kg[P]/ha`, `kg[K]/ha`) ~ exname, data = r6a_fert, sum)
  names(fert_totals_6) <- c("trial_id", "N_fertilizer", "P_fertilizer", "K_fertilizer")
  n_splits_6 <- aggregate(`kg[N]/ha` ~ exname, data = r6a_fert[r6a_fert$`kg[N]/ha` > 0, ], length)
  names(n_splits_6) <- c("trial_id", "N_splits")
  r6a_fert$product_kg <- ifelse(r6a_fert$`!Fertilizer type - code signification` == "Di-ammonium Phosphate", r6a_fert$`kg[N]/ha` / 0.18,
                                ifelse(r6a_fert$`!Fertilizer type - code signification` == "Urea", r6a_fert$`kg[N]/ha` / 0.46, NA))
  fert_amount_6 <- aggregate(product_kg ~ exname, data = r6a_fert, sum)
  names(fert_amount_6) <- c("trial_id", "fertilizer_amount")
  
  r6a_summary <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=76, n_max=3)
  r6a_summary$exname <- ifelse(r6a_summary$`#` == 1, "RWBU1501", "RWBU1401")
  r6a_summary$pldae  <- as.Date(r6a_summary$pldae)
  r6a_summary$adat   <- as.Date(r6a_summary$adat)
  r6a_summary$mdat   <- as.Date(r6a_summary$mdat)
  
  r6b_profile <- carobiner::read.excel(f6, sheet="Soils", skip=12, n_max=1)
  r6b_layers  <- carobiner::read.excel(f6, sheet="Soils", skip=17, n_max=6)
  
  r6c_station <- carobiner::read.excel(f6, sheet="Weather", skip=3, n_max=1)
  r6c_daily   <- carobiner::read.excel(f6, sheet="Weather", skip=9, n_max=867)
  r6c_daily$w_date <- as.Date(r6c_daily$w_date)
  # RWBU's two seasons cross a calendar-year boundary and the weather
  # record runs well beyond both (station kept recording after the
  # second harvest) - each day is assigned to a season only if it falls
  # within that season's real planting-to-maturity window; everything
  # else stays NA rather than being force-assigned to whichever season
  # happens to be "most recent"
  r6c_daily$exname <- NA
  r6c_daily$exname[r6c_daily$w_date >= as.Date("2013-09-24") & r6c_daily$w_date <= as.Date("2014-01-22")] <- "RWBU1401"
  r6c_daily$exname[r6c_daily$w_date >= as.Date("2014-10-09") & r6c_daily$w_date <= as.Date("2015-02-04")] <- "RWBU1501"
  
  d6_exp <- data.frame(
    trial_id  = r6a_exp$exname,
    location_id = "RWBU",
    country   = "Rwanda",
    location  = tools::toTitleCase(tolower(r6a_exp$site_name[1])),
    latitude  = r6a_exp$fl_lat,
    longitude = r6a_exp$fl_long,
    elevation = r6a_exp$flele,
    geo_from_source = TRUE,
    is_survey = FALSE,
    on_farm = TRUE,
    treatment = r6a_exp$TRT_NAME[1],
    station_id = r6a_exp$wst_id[1] 
  )
  d6_prevcrop <- data.frame(
    trial_id = r6a_prevcrop$exname,
    soil_sample_date = r6a_prevcrop$icdat,
    previous_crop = tolower(r6a_prevcrop$`!...5`),
    residue_prevcrop = r6a_prevcrop$icrag + r6a_prevcrop$icrt,
    residue_prevcrop_used = (r6a_prevcrop$icrag + r6a_prevcrop$icrt) > 0
  )
  d6_tillage <- data.frame(
    trial_id = r6a_tillage$exname,
    land_prep_traction = r6a_tillage$text,
    tillage_date = r6a_tillage$`yyyy-mm-dd`,
    tillage_depth = r6a_tillage$cm
  )
  d6_planting <- data.frame(
    trial_id = r6a_planting$exname,
    planting_date = r6a_planting$`yyyy-mm-dd`,
    variety = "RWBU_CUL",
    variety_type = "Open Pollinated Variety (OPV)",
    plant_density = r6a_planting$`#/m2` * 10000,
    row_spacing = r6a_planting$cm,
    planting_depth = r6a_planting$mm
  )
  d6_summary <- data.frame(
    trial_id = r6a_summary$exname,
    emergence_date = as.Date(NA),
    flowering_date = r6a_summary$adat,
    maturity_date = r6a_summary$mdat
  )
  
  d6a <- Reduce(function(x, y) merge(x, y, by = "trial_id"),
                list(d6_exp, d6_prevcrop, d6_tillage, d6_planting, d6_summary,
                     fert_totals_6, n_splits_6, fert_amount_6))
  
  d6a$crop <- "maize"
  d6a$irrigated <- FALSE
  d6a$OM_used <- FALSE
  d6a$N_organic <- NA
  d6a$P_organic <- NA
  d6a$K_organic <- NA
  d6a$fertilization_method <- "Broadcast, incorporated"
  
  r6b_rwbu <- data.frame(
    location_id = "RWBU",
    soil_id = "RWBU",
    soil_texture = r6b_profile$sltx,
    max_rooting_depth = 100,
    max_water_depth = 100,
    max_N_depth = 0,
    depth = r6b_layers$sllb,
    soil_bd     = r6b_layers$slbdm,
    soil_SOC    = r6b_layers$sloc,
    soil_clay   = r6b_layers$slcly,
    soil_silt   = r6b_layers$slsil,
    soil_gravel = r6b_layers$slcf,
    soil_CEC    = r6b_layers$slcec,
    soil_P      = r6b_layers$slpx,
    soil_K_exch = r6b_layers$slke,
    soil_pH     = r6b_layers$slhw,
    soil_N      = r6b_layers$slni * 10000,
    soil_FC     = r6b_layers$sldul,
    soil_PWP    = r6b_layers$slll,
    soil_saturation = r6b_layers$slsat
  )
  
  r6c_rwbu <- data.frame(
    location_id = "RWBU",
    date = r6c_daily$w_date,
    tmax = r6c_daily$tmax,
    tmin = r6c_daily$tmin,
    prec = r6c_daily$rain,
    srad = r6c_daily$srad,
    wspd = r6c_daily$wind/86.4,  # km/d -> m/s,
    rhum = r6c_daily$rhum
  )
  
  # RWBU icnh4/icno3 are NA throughout - genuine source gap, not a read error
  r6_iclayers_long <- data.frame(
    trial_id = r6a_iclayers$exname,
    location_id = "RWBU",
    depth = r6a_iclayers$icbl,
    date = d6_prevcrop$soil_sample_date[match(r6a_iclayers$exname, d6_prevcrop$trial_id)],
    soil_NH4 = NA,
    soil_NO3 = NA,
    soil_GWC = r6a_iclayers$ich2o / r6b_layers$slbdm[match(r6a_iclayers$icbl, r6b_layers$sllb)]
  )
  
  ############################################################
  # Combine field management, soil, and weather across all 5 sites
  ############################################################
  
  d_fieldmgmt <- rbind(d2a, d3a, d4a, d5a, d6a)
  d_soil      <- rbind(r2b_beou, r3b_etba, r4b_ghkp, r5b_mant, r6b_rwbu)
  d_weather   <- rbind(r2c_beou, r3c_etba, r4c_ghkp, r5c_mant, r6c_rwbu)
  
  ############################################################
  # Raw daily crop observations (r7/r8) - the important data.
  # Reshaped long: one row per real measurement event, using date to
  # distinguish records
  ############################################################
  r7  <- read.delim(f7,  header=TRUE) # daily observations, average: Yield, Biomass, LAI, CroN, CroP, SoilWTot, SoilNTot
  r8  <- read.delim(f8,  header=TRUE) # daily observations, sd: same variables except CroP (no sd reported for crop P in source)
  r9  <- read.delim(f9,  header=TRUE) # soil water/N by layer, average: depth-resolved SoilW/SoilN, multiple dates per experiment
  r10 <- read.delim(f10, header=TRUE) # soil water/N by layer, sd: same structure as r9, standard deviation
  r11 <- read.delim(f11, header=TRUE) # season summary, average: one row per experiment - Planting/Ant/Mat dates, final Yield, Biom.ma, MaxLAI, CroN.ma, CroP.ma, SoilN
  r12 <- read.delim(f12, header=TRUE) # season summary, sd: same variables as r11, standard deviation
  
  r13 <- readLines(f13) # reference only
  
  ### d: raw daily crop growth observations (r7/r8), reshaped WIDE
  # r7_agg/r8_agg: same date sometimes split across separate rows.
  r7_agg <- aggregate(cbind(Yield, Biom, LAI, CroN, CroP, SoilWTot.MaxMeasDepth, SoilNTot.MaxMeasDepth) ~ Experiment.name + Date,
                      data = r7, FUN = function(x) x[!is.na(x)][1], na.action = na.pass)
  r8_agg <- aggregate(cbind(Yield, Biom, LAI, CroN, SoilWTot.MaxMeasDepth, SoilNTot.MaxMeasDepth) ~ Experiment.name + Date,
                      data = r8, FUN = function(x) x[!is.na(x)][1], na.action = na.pass)
  
  r7a <- merge(r7_agg, r8_agg, by = c("Experiment.name", "Date"), suffixes = c("", "_sd"), all.x = TRUE)
  
  d <- data.frame(
    trial_id = r7a$Experiment.name,
    date = as.Date(r7a$Date),
    yield = as.numeric(r7a$Yield) * 1000,          # t -> kg
    yield_sd = as.numeric(r7a$Yield_sd) * 1000,
    dmy_total = as.numeric(r7a$Biom) * 1000,        # t -> kg
    dmy_total_sd = as.numeric(r7a$Biom_sd) * 1000,
    LAI = as.numeric(r7a$LAI),
    LAI_sd = as.numeric(r7a$LAI_sd),
    crop_N = as.numeric(r7a$CroN),          # not in terminag - kept as interpreted
    crop_N_sd = as.numeric(r7a$CroN_sd),
    crop_P = as.numeric(r7a$CroP),          # not in terminag - no sd in source
    soil_water_total = as.numeric(r7a$SoilWTot.MaxMeasDepth),      # not in terminag
    soil_water_total_sd = as.numeric(r7a$SoilWTot.MaxMeasDepth_sd),
    soil_N_total = as.numeric(r7a$SoilNTot.MaxMeasDepth),          # not in terminag
    soil_N_total_sd = as.numeric(r7a$SoilNTot.MaxMeasDepth_sd)
  )
  value_cols <- c("yield","dmy_total","LAI","crop_N","crop_P","soil_water_total","soil_N_total")
  d <- d[rowSums(!is.na(d[value_cols])) > 0, ]   # drops only rows where EVERY variable is NA - a single measured variable is enough to keep the row
  d$date <- as.character(d$date)
  
  
  ### SoilWN layers (r9/r10).
  r9$Date  <- as.Date(r9$Date,  format = "%d.%m.%Y")   # r9's own format
  r10$Date <- as.Date(r10$Date, format = "%Y-%m-%d")   # r10's own format
  r9_10 <- merge(r9, r10, by = c("Experiment.name", "Date", "Soil.layer.base.depth"),
                 suffixes = c("", "_sd"), all.x = TRUE)
  
  d_soilwn <- data.frame(
    trial_id = r9_10$Experiment.name,
    location_id = substr(r9_10$Experiment.name, 1, 4),
    depth = r9_10$Soil.layer.base.depth,
    date  = as.character(r9_10$Date),
    soil_water_layer = as.numeric(r9_10$SoilW.layer.mm),          # not in terminag
    soil_water_layer_sd = as.numeric(r9_10$SoilW.layer.mm_sd),
    soil_N_layer = as.numeric(r9_10$SoilN.layer.kg.ha),           # not in terminag
    soil_N_layer_sd = as.numeric(r9_10$SoilN.layer.kg.ha_sd)
  )
  d_soilwn <- d_soilwn[!is.na(d_soilwn$soil_water_layer) | !is.na(d_soilwn$soil_N_layer), ]
  
  
  ### initial conditions (iclayers, per site) - same treatment as d_soilwn
  iclayers_all <- rbind(r2_iclayers_long, r3_iclayers_long, r4_iclayers_long,
                        r5_iclayers_long, r6_iclayers_long)
  
  d_iclayers <- data.frame(
    trial_id = iclayers_all$trial_id,
    location_id = iclayers_all$location_id,
    depth = iclayers_all$depth,
    date  = as.character(iclayers_all$date),
    soil_NH4 = iclayers_all$soil_NH4,
    soil_NO3 = iclayers_all$soil_NO3,
    soil_GWC = iclayers_all$soil_GWC
  )
  d_iclayers <- d_iclayers[rowSums(!is.na(d_iclayers[c("soil_NH4","soil_NO3","soil_GWC")])) > 0, ]
  
  
  ### d_fieldmgmt date conversions
  d_fieldmgmt$soil_sample_date <- as.character(d_fieldmgmt$soil_sample_date)
  d_fieldmgmt$tillage_date <- as.character(d_fieldmgmt$tillage_date)
  d_fieldmgmt$planting_date <- as.character(d_fieldmgmt$planting_date)
  d_fieldmgmt$emergence_date <- as.character(d_fieldmgmt$emergence_date)
  d_fieldmgmt$flowering_date <- as.character(d_fieldmgmt$flowering_date)
  d_fieldmgmt$maturity_date <- as.character(d_fieldmgmt$maturity_date)
  prevcrop_term_lookup <- c("fallow" = "none","dry bean" = "common bean")
  d_fieldmgmt$previous_crop <- ifelse(
    d_fieldmgmt$previous_crop %in% names(prevcrop_term_lookup),
    prevcrop_term_lookup[d_fieldmgmt$previous_crop],d_fieldmgmt$previous_crop)

  d_weather$date <- as.character(d_weather$date)
  
  carobiner::write_files(path, meta, wide=d_fieldmgmt, long=d, wth=d_weather)
}
