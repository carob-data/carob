# R script for "carob"
# license: GPL (>=3)

## NOTES:
# Compilation of 5 published maize experiments used for AgMIP crop model calibration:
# BEOU (Benin; Amouzou et al. 2018), ETBA (Ethiopia; Sida et al. 2018), GHKP (Ghana;
# MacCarthy et al. 2015), MANT (Mali; Traore et al. 2014), RWBU (Rwanda; Ndoli et al. 2018);
# 2 experiments (seasons) each.
# The publication (Falconnier et al. 2020, doi:10.1111/gcb.15261) confirms: total N applied
# was 0 (Benin), 64 (Rwanda), 80 (Ghana), 85 (Mali), 87 (Ethiopia) kg/ha; no irrigation
# anywhere; open-pollinated varieties everywhere except Ethiopia (hybrid). The N totals
# computed here from the fertilizer tables match these values exactly.
# The original site experiments are described in (per Falconnier et al. 2020):
#   BEOU: Amouzou, Naab, Lamers & Becker (2018), Field Crops Research 217, 134-149, doi:10.1016/j.fcr.2017.12.017
#   ETBA: Sida, Baudron, Hadgu, Derero & Giller (2018), Agriculture, Ecosystems & Environment 260, 36-46, doi:10.1016/j.agee.2018.03.011
#   GHKP: MacCarthy, Akponikpe, Narh & Tegbe (2015), Nutrient Cycling in Agroecosystems 102, 45-64, doi:10.1007/s10705-015-9701-x
#   MANT: Traore, van Wijk, Descheemaeker, Corbeels, Rufino & Giller (2014), Field Crops Research 156, 63-75, doi:10.1016/j.fcr.2013.10.014
#   RWBU: Ndoli, Baudron, Sida, Schut, van Heerwaarden & Giller (2018), Field Crops Research 221, 238-244, doi:10.1016/j.fcr.2018.03.003
# GHKP TRT_NAME encodes N-P rate and residue as free text, kept verbatim in "treatment".
# GHKP had no tillage ("!No Tillage" in source).
# Maximum measurement depths for the profile totals in the long table
# (soil_SWC / soil_N_min_total): BEOU 60/60 cm (water/N), ETBA 100 cm (water; no
# in-season N measured), GHKP 100/100 cm, MANT 80 cm (water), RWBU 100 cm (water).
# RWBU initial-conditions NH4/NO3 are NA in the source.
# RWBU weather spans two seasons across calendar years.
# harvest_date is not reported (maturity_date is physiological maturity).

## ISSUES:
# - on_farm = TRUE is kept from the earlier draft; the five source experiments are described
#   in separate papers and it was not verified for each site whether the trial was on a
#   farmer's field or a research station.
# - fertilization_method "broadcast; incorporated" was taken over from the earlier draft;
#   verify against the fertilizer-application method codes in the Field-mgmt sheets.
# - BEOU1401 above-ground biomass at maturity differs between the daily file (4.24 t/ha)
#   and the season summary (3.963 t/ha); both are kept (the in-season series as
#   dmy_total in the long table, the maturity value as dmy_total in the wide table).
# - The long table combines four record types (daily crop observations, in-season soil
#   water/N by layer, initial soil conditions by layer, and the static soil profile by
#   layer). Static soil-profile rows have no date (depth is their key).
# - soil_FC and soil_PWP are volumetric fractions (cm3/cm3) as in the source; terminag
#   does not specify a unit for these.

## NEW VARIABLES 
# yield_sd_           sd of grain yield across replicates (kg/ha)
# dmy_total_sd_       sd of above-ground biomass (kg/ha)
# LAI_sd_             sd of in-season leaf area index
# crop_N_sd_          sd of N in above-ground plant parts (kg/ha)
# soil_SWC_sd_        sd of soil water in the profile up to the max measurement depth (mm)
# soil_N_min_total_sd_ sd of mineral N in the profile up to the max measurement depth (kg/ha)
# soil_VWC_sd_        sd of volumetric soil water content of a layer (cm3/cm3)
# soil_N_sd_          sd of layer mineral N concentration (mg/kg)

## NOT PROCESSED:
# - Agmip_LowInput_template_variable_definition0717-2.xlsx: codebook only (variable
#   definitions and units); used to interpret the observation files, no data.
# - readme_dataverse.txt: file inventory, no data.
# - Weather sheet station rows, "tav" and "tamp": long-term average temperature and
#   amplitude (model inputs, not measurements).
# - AgMIP_Smallholder_Observation_SoilWN_layers_*.txt: "SoilW.layer.mm" and
#   "SoilN.layer.kg.ha" are derived from the processed cm3/cm3 and ppm values
#   (x layer thickness / x bulk density), redundant.
# - AgMIP_Smallholder_Observation_summary_*.txt: Planting.date, Ant.date and Mat.date
#   duplicate the Field-mgmt sheet values already processed; MaxLAI (and its sd) is
#   exactly the maximum of the daily LAI series (verified), redundant.
# - Field-mgmt sheets: institution, suite_name, local_name, exp_dur (administrative).


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
          ## Falconnier et al. 2020, Global Change Biologye
          publication = "doi:10.1111/gcb.15261",
          project = "AgMIP",
          carob_date = "2026-08-03",
          design = NA,
          data_type = "experiment",
          treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
          response_vars = "yield;dmy_total;LAI",
          carob_contributor = "Stella Muthoni;Robert Hijmans",
          carob_LLM = "Claude Fable 5",
          notes = NA,
          carob_completion = 90,
          carob_effort = 16
  )
  
  # f1 (variable-definition codebook) and the readme are not read; see NOT PROCESSED
  
  # Field management, soil and daily weather
  f2  <- ff[basename(ff) == "BEOU_field_mgmt_soils_weather070518LI-2.xlsx"] # Benin
  f3  <- ff[basename(ff) == "ETBA_field_mgmt_soils_weather070518LI-2.xlsx"]
  f4  <- ff[basename(ff) == "GHKP_field_mgmt_soils_weather071718LI-2.xlsx"]
  f5  <- ff[basename(ff) == "MANT_field_mgmt_soils_weather062018LI-2.xlsx"]
  f6  <- ff[basename(ff) == "RWBU_field_mgmt_soils_weather060818LI-2.xlsx"]
  
  # observations (txt, "na" is the missing-value flag)
  f7  <- ff[basename(ff) == "AgMIP_Smallholder_Observation_daily_average.txt"]
  f8  <- ff[basename(ff) == "AgMIP_Smallholder_Observation_daily_sd.txt"]
  f9  <- ff[basename(ff) == "AgMIP_Smallholder_Observation_SoilWN_layers_average.txt"]
  f10 <- ff[basename(ff) == "AgMIP_Smallholder_Observation_SoilWN_layers_sd.txt"]
  f11 <- ff[basename(ff) == "AgMIP_Smallholder_Observation_summary_average.txt"]
  f12 <- ff[basename(ff) == "AgMIP_Smallholder_Observation_summary_sd.txt"]
  
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
  
  # small data.frames, one per source table
  d2_exp <- data.frame(
    trial_id  = r2a_exp$exname,
    location_id = "BEOU",
    country   = "Benin",
    location  = carobiner::fix_name(tolower(r2a_exp$site_name[1]), "title"),
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
    previous_crop = tolower(r2a_prevcrop$`!...5`),
    residue_prevcrop = r2a_prevcrop$icrag + r2a_prevcrop$icrt,
    residue_prevcrop_used = (r2a_prevcrop$icrag + r2a_prevcrop$icrt) > 0
  )
  d2_tillage <- data.frame(
    trial_id = r2a_tillage$exname,
    land_prep_traction = r2a_tillage$text,
    land_prep_date = r2a_tillage$`yyyy-mm-dd`,
    tillage_depth = r2a_tillage$cm
  )
  d2_planting <- data.frame(
    trial_id = r2a_planting$exname,
    planting_date = r2a_planting$`yyyy-mm-dd`,
    variety = r2a_planting$text...10,
    variety_type = r2a_planting$text...12,
    plant_density = r2a_planting$`#/m2` * 10000,
    row_spacing = r2a_planting$cm,
    planting_depth = r2a_planting$mm / 10  # mm -> cm
  )
  d2_summary <- data.frame(
    trial_id = r2a_summary$exname,
    emergence_date = r2a_summary$pldae,
    flowering_date = r2a_summary$adat,
    maturity_date = r2a_summary$mdat
  )
  
  d2a <- Reduce(function(x, y) merge(x, y, by = "trial_id", all.x = TRUE),
                list(d2_exp, d2_prevcrop, d2_tillage, d2_planting, d2_summary))
  
  d2a$crop <- "maize"
  d2a$irrigated <- FALSE
  # explicit "!No fertiliser" / "!No organic amendment" in source: zero, not unknown
  d2a$OM_used <- FALSE
  d2a$N_organic <- 0
  d2a$P_organic <- 0
  d2a$K_organic <- 0
  d2a$N_fertilizer <- 0
  d2a$P_fertilizer <- 0
  d2a$K_fertilizer <- 0
  d2a$N_splits <- 0L
  d2a$fertilizer_amount <- 0
  d2a$fertilization_method <- "none"
  
  # soil profile for BEOU - kept separate, combined across sites later
  d2_soil <- data.frame(
    trial_id = "BEOU150001",   # soil_id, remapped to the trials below
    depth = r2b_layers$sllb,
    soil_texture = r2b_profile$sltx,
    soil_bd     = r2b_layers$slbdm,
    soil_SOC    = r2b_layers$sloc,
    soil_clay   = r2b_layers$slcly,
    soil_silt   = r2b_layers$slsil,
    soil_gravel = r2b_layers$slcf,
    soil_CEC    = r2b_layers$slcec,
    soil_P      = r2b_layers$slpx,
    soil_K_exch = r2b_layers$slke,
    soil_pH     = r2b_layers$slhw,
    soil_N_total = r2b_layers$slni * 10000,  # % -> mg/kg
    soil_FC     = r2b_layers$sldul,          # cm3/cm3 (no unit in terminag)
    soil_PWP    = r2b_layers$slll,           # cm3/cm3
    soil_SP     = r2b_layers$slsat * 100     # cm3/cm3 -> %
  )
  
  # weather for BEOU - kept separate, combined across sites later
  d2_weather <- data.frame(
    location_id = "BEOU",
    country = "Benin",
    station_id = r2c_station$wst_id,
    longitude = r2c_station$wst_long,
    latitude = r2c_station$wst_lat,
    elevation = r2c_station$wst_elev,
    date = r2c_daily$w_date,
    tmax = r2c_daily$tmax,
    tmin = r2c_daily$tmin,
    prec = r2c_daily$rain,
    srad = r2c_daily$srad,
    wspd = r2c_daily$wind / 86.4,  # km/d -> m/s
    rhum = r2c_daily$rhum
  )
  
  # initial conditions (iclayers) - depth-resolved, one date per experiment
  d2_iclayers <- data.frame(
    trial_id = r2a_iclayers$exname,
    depth = r2a_iclayers$`icbl...2`,
    date = r2a_prevcrop$icdat[match(r2a_iclayers$exname, r2a_prevcrop$exname)],
    soil_NH4 = r2a_iclayers$icnh4,
    soil_NO3 = r2a_iclayers$icno3,
    soil_VWC = r2a_iclayers$ich2o  # cm3/cm3
  )
  
  ############################################################
  # ETBA (Ethiopia)
  ############################################################
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
  # product weight from elemental N and the product N fraction (DAP 18% N, urea 46% N)
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
  
  d3_exp <- data.frame(
    trial_id  = r3a_exp$exname,
    location_id = "ETBA",
    country   = "Ethiopia",
    location  = carobiner::fix_name(tolower(r3a_exp$site_name[1]), "title"),
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
    previous_crop = tolower(r3a_prevcrop$`!...5`),
    residue_prevcrop = r3a_prevcrop$icrag + r3a_prevcrop$icrt,
    residue_prevcrop_used = (r3a_prevcrop$icrag + r3a_prevcrop$icrt) > 0
  )
  d3_tillage <- data.frame(
    trial_id = r3a_tillage$exname,
    land_prep_traction = r3a_tillage$text,
    land_prep_date = r3a_tillage$`yyyy-mm-dd`,
    tillage_depth = r3a_tillage$cm
  )
  d3_planting <- data.frame(
    trial_id = r3a_planting$exname,
    planting_date = r3a_planting$`yyyy-mm-dd`,
    variety = "ETBA_CUL",  # cultivar code used in the dataset; a hybrid (Falconnier et al. 2020)
    variety_type = "hybrid",
    plant_density = r3a_planting$`#/m2` * 10000,
    row_spacing = r3a_planting$cm,
    planting_depth = r3a_planting$mm / 10  # mm -> cm
  )
  d3_summary <- data.frame(
    trial_id = r3a_summary$exname,
    flowering_date = r3a_summary$adat,
    maturity_date = r3a_summary$mdat
  )
  
  d3a <- Reduce(function(x, y) merge(x, y, by = "trial_id", all.x = TRUE),
                list(d3_exp, d3_prevcrop, d3_tillage, d3_planting, d3_summary,
                     fert_totals_3, n_splits_3, fert_amount_3))
  
  d3a$emergence_date <- as.Date(NA)  # not recorded at ETBA
  d3a$crop <- "maize"
  d3a$irrigated <- FALSE
  d3a$OM_used <- FALSE
  # no organic amendment applied
  d3a$N_organic <- 0
  d3a$P_organic <- 0
  d3a$K_organic <- 0
  d3a$fertilization_method <- "broadcast; incorporated"
  
  d3_soil <- data.frame(
    trial_id = "ETBA140001",
    depth = r3b_layers$sllb,
    soil_texture = r3b_profile$sltx,
    soil_bd     = r3b_layers$slbdm,
    soil_SOC    = r3b_layers$sloc,
    soil_clay   = r3b_layers$slcly,
    soil_silt   = r3b_layers$slsil,
    soil_gravel = r3b_layers$slcf,
    soil_CEC    = r3b_layers$slcec,
    soil_P      = r3b_layers$slpx,
    soil_K_exch = r3b_layers$slke,
    soil_pH     = r3b_layers$slhw,
    soil_N_total = r3b_layers$slni * 10000,
    soil_FC     = r3b_layers$sldul,
    soil_PWP    = r3b_layers$slll,
    soil_SP     = r3b_layers$slsat * 100
  )
  
  d3_weather <- data.frame(
    location_id = "ETBA",
    country = "Ethiopia",
    station_id = r3c_station$wst_id,
    longitude = r3c_station$wst_long,
    latitude = r3c_station$wst_lat,
    elevation = r3c_station$wst_elev,
    date = r3c_daily$w_date,
    tmax = r3c_daily$tmax,
    tmin = r3c_daily$tmin,
    prec = r3c_daily$rain,
    srad = r3c_daily$srad,
    wspd = r3c_daily$wind / 86.4,  # km/d -> m/s
    rhum = r3c_daily$rhum
  )
  
  d3_iclayers <- data.frame(
    trial_id = r3a_iclayers$exname,
    depth = r3a_iclayers$icbl,
    date = r3a_prevcrop$icdat[match(r3a_iclayers$exname, r3a_prevcrop$exname)],
    soil_NH4 = r3a_iclayers$icnh4,
    soil_NO3 = r3a_iclayers$icno3,
    soil_VWC = r3a_iclayers$ich2o
  )
  
  ############################################################
  # GHKP (Ghana)
  ############################################################
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
  # product weight from the elemental rate and product nutrient fraction
  # (urea 46% N, TSP 19.23% P, MOP 49.8% K)
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
  
  d4_exp <- data.frame(
    trial_id  = r4a_exp$exname,
    location_id = "GHKP",
    country   = "Ghana",
    location  = carobiner::fix_name(tolower(r4a_exp$site_name[1]), "title"),
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
    previous_crop = tolower(r4a_prevcrop$`!...5`),
    residue_prevcrop = r4a_prevcrop$icrag + r4a_prevcrop$icrt,
    residue_prevcrop_used = (r4a_prevcrop$icrag + r4a_prevcrop$icrt) > 0
  )
  d4_planting <- data.frame(
    trial_id = r4a_planting$exname,
    planting_date = r4a_planting$`yyyy-mm-dd`,
    variety = "GHKP_CUL",  # cultivar code used in the dataset; an OPV (Falconnier et al. 2020)
    variety_type = "OPV",
    plant_density = r4a_planting$`#/m2` * 10000,
    row_spacing = r4a_planting$cm,
    planting_depth = r4a_planting$mm / 10  # mm -> cm
  )
  d4_summary <- data.frame(
    trial_id = r4a_summary$exname,
    emergence_date = r4a_summary$pldae,
    flowering_date = r4a_summary$adat,
    maturity_date = r4a_summary$mdat
  )
  
  d4a <- Reduce(function(x, y) merge(x, y, by = "trial_id", all.x = TRUE),
                list(d4_exp, d4_prevcrop, d4_planting, d4_summary,
                     fert_totals_4, n_splits_4, fert_amount_4))
  
  # no tillage at GHKP - confirmed "!No Tillage" in source
  d4a$land_prep_traction <- NA
  d4a$land_prep_date <- as.Date(NA)
  d4a$tillage_depth <- NA
  
  d4a$crop <- "maize"
  d4a$irrigated <- FALSE
  d4a$OM_used <- FALSE
  # no organic amendment applied
  d4a$N_organic <- 0
  d4a$P_organic <- 0
  d4a$K_organic <- 0
  d4a$fertilization_method <- "broadcast; incorporated"
  
  d4_soil <- data.frame(
    trial_id = r4b_layers$soil_id,
    depth = r4b_layers$sllb,
    soil_texture = r4b_profile$sltx[match(r4b_layers$soil_id, ifelse(r4b_profile$`%`=="A","GHKP080001","GHKP090001"))],
    soil_bd     = r4b_layers$slbdm,
    soil_SOC    = r4b_layers$sloc,
    soil_clay   = r4b_layers$slcly,
    soil_silt   = r4b_layers$slsil,
    soil_gravel = r4b_layers$slcf,
    soil_CEC    = r4b_layers$slcec,
    soil_P      = r4b_layers$slpx,
    soil_K_exch = r4b_layers$slke,
    soil_pH     = r4b_layers$slhw,
    soil_N_total = r4b_layers$slni * 10000,
    soil_FC     = r4b_layers$sldul,
    soil_PWP    = r4b_layers$slll,
    soil_SP     = r4b_layers$slsat * 100
  )
  
  d4_weather <- data.frame(
    location_id = "GHKP",
    country = "Ghana",
    station_id = r4c_station$wst_id,
    longitude = r4c_station$wst_long,
    latitude = r4c_station$wst_lat,
    elevation = r4c_station$wst_elev,
    date = r4c_daily$w_date,
    tmax = r4c_daily$tmax,
    tmin = r4c_daily$tmin,
    prec = r4c_daily$rain,
    srad = r4c_daily$srad,
    wspd = r4c_daily$wind / 86.4,  # km/d -> m/s
    rhum = r4c_daily$rhum
  )
  
  d4_iclayers <- data.frame(
    trial_id = r4a_iclayers$exname,
    depth = r4a_iclayers$`icbl...2`,
    date = r4a_prevcrop$icdat[match(r4a_iclayers$exname, r4a_prevcrop$exname)],
    soil_NH4 = r4a_iclayers$icnh4,
    soil_NO3 = r4a_iclayers$icno3,
    soil_VWC = r4a_iclayers$ich2o
  )
  
  ############################################################
  # MANT (Mali)
  ############################################################
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
  # nutrient content of the organic amendment is given as % in the source
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
  # product weight only computable for urea (46% N); other products lack a stated fraction
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
  
  d5_exp <- data.frame(
    trial_id  = r5a_exp$exname,
    location_id = "MANT",
    country   = "Mali",
    location  = carobiner::fix_name(tolower(r5a_exp$site_name[1]), "title"),
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
    previous_crop = tolower(r5a_prevcrop$`!...5`),
    residue_prevcrop = r5a_prevcrop$icrag + r5a_prevcrop$icrt,
    residue_prevcrop_used = (r5a_prevcrop$icrag + r5a_prevcrop$icrt) > 0
  )
  d5_tillage <- data.frame(
    trial_id = r5a_tillage$exname,
    land_prep_traction = r5a_tillage$text,
    land_prep_date = r5a_tillage$`yyyy-mm-dd`,
    tillage_depth = r5a_tillage$cm
  )
  d5_planting <- data.frame(
    trial_id = r5a_planting$exname,
    planting_date = r5a_planting$`yyyy-mm-dd`,
    variety = "MANT_CUL",  # cultivar code used in the dataset; an OPV (Falconnier et al. 2020)
    variety_type = "OPV",
    plant_density = r5a_planting$`#/m2` * 10000,
    row_spacing = r5a_planting$cm,
    planting_depth = r5a_planting$mm / 10  # mm -> cm
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
    flowering_date = r5a_summary$adat,
    maturity_date = r5a_summary$mdat
  )
  
  d5a <- Reduce(function(x, y) merge(x, y, by = "trial_id", all.x = TRUE),
                list(d5_exp, d5_prevcrop, d5_tillage, d5_planting, d5_organic, d5_summary,
                     fert_totals_5, n_splits_5, fert_amount_5))
  
  d5a$emergence_date <- as.Date(NA)  # not recorded at MANT
  d5a$crop <- "maize"
  d5a$irrigated <- FALSE
  d5a$fertilization_method <- "broadcast; incorporated"
  
  d5_soil <- data.frame(
    trial_id = "MANT100001",
    depth = r5b_layers$sllb,
    soil_texture = r5b_profile$sltx,
    soil_bd     = r5b_layers$slbdm,
    soil_SOC    = r5b_layers$sloc,
    soil_clay   = r5b_layers$slcly,
    soil_silt   = r5b_layers$slsil,
    soil_gravel = r5b_layers$slcf,
    soil_CEC    = r5b_layers$slcec,
    soil_P      = r5b_layers$slpx,
    soil_K_exch = r5b_layers$slke,
    soil_pH     = r5b_layers$slhw,
    soil_N_total = r5b_layers$slni * 10000,
    soil_FC     = r5b_layers$sldul,
    soil_PWP    = r5b_layers$slll,
    soil_SP     = r5b_layers$slsat * 100
  )
  
  d5_weather <- data.frame(
    location_id = "MANT",
    country = "Mali",
    station_id = r5c_station$wst_id,
    longitude = r5c_station$wst_long,
    latitude = r5c_station$wst_lat,
    elevation = r5c_station$wst_elev,
    date = r5c_daily$w_date,
    tmax = r5c_daily$tmax,
    tmin = r5c_daily$tmin,
    prec = r5c_daily$rain,
    srad = r5c_daily$srad,
    wspd = r5c_daily$wind / 86.4,  # km/d -> m/s
    rhum = r5c_daily$rhum
  )
  
  d5_iclayers <- data.frame(
    trial_id = r5a_iclayers$exname,
    depth = r5a_iclayers$icbl,
    date = r5a_prevcrop$icdat[match(r5a_iclayers$exname, r5a_prevcrop$exname)],
    soil_NH4 = r5a_iclayers$icnh4,
    soil_NO3 = r5a_iclayers$icno3,
    soil_VWC = r5a_iclayers$ich2o
  )
  
  ############################################################
  # RWBU (Rwanda)
  ############################################################
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
  # product weight from elemental N and the product N fraction (DAP 18% N, urea 46% N)
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
  
  d6_exp <- data.frame(
    trial_id  = r6a_exp$exname,
    location_id = "RWBU",
    country   = "Rwanda",
    location  = carobiner::fix_name(tolower(r6a_exp$site_name[1]), "title"),
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
    previous_crop = tolower(r6a_prevcrop$`!...5`),
    residue_prevcrop = r6a_prevcrop$icrag + r6a_prevcrop$icrt,
    residue_prevcrop_used = (r6a_prevcrop$icrag + r6a_prevcrop$icrt) > 0
  )
  d6_tillage <- data.frame(
    trial_id = r6a_tillage$exname,
    land_prep_traction = r6a_tillage$text,
    land_prep_date = r6a_tillage$`yyyy-mm-dd`,
    tillage_depth = r6a_tillage$cm
  )
  d6_planting <- data.frame(
    trial_id = r6a_planting$exname,
    planting_date = r6a_planting$`yyyy-mm-dd`,
    variety = "RWBU_CUL",  # cultivar code used in the dataset; an OPV (Falconnier et al. 2020)
    variety_type = "OPV",
    plant_density = r6a_planting$`#/m2` * 10000,
    row_spacing = r6a_planting$cm,
    planting_depth = r6a_planting$mm / 10  # mm -> cm
  )
  d6_summary <- data.frame(
    trial_id = r6a_summary$exname,
    flowering_date = r6a_summary$adat,
    maturity_date = r6a_summary$mdat
  )
  
  d6a <- Reduce(function(x, y) merge(x, y, by = "trial_id", all.x = TRUE),
                list(d6_exp, d6_prevcrop, d6_tillage, d6_planting, d6_summary,
                     fert_totals_6, n_splits_6, fert_amount_6))
  
  d6a$emergence_date <- as.Date(NA)  # not recorded at RWBU
  d6a$crop <- "maize"
  d6a$irrigated <- FALSE
  d6a$OM_used <- FALSE
  # no organic amendment applied
  d6a$N_organic <- 0
  d6a$P_organic <- 0
  d6a$K_organic <- 0
  d6a$fertilization_method <- "broadcast; incorporated"
  
  d6_soil <- data.frame(
    trial_id = "RWBU",   # RWBU soil_id does not follow the SITEyyNNNN pattern used elsewhere
    depth = r6b_layers$sllb,
    soil_texture = r6b_profile$sltx,
    soil_bd     = r6b_layers$slbdm,
    soil_SOC    = r6b_layers$sloc,
    soil_clay   = r6b_layers$slcly,
    soil_silt   = r6b_layers$slsil,
    soil_gravel = r6b_layers$slcf,
    soil_CEC    = r6b_layers$slcec,
    soil_P      = r6b_layers$slpx,
    soil_K_exch = r6b_layers$slke,
    soil_pH     = r6b_layers$slhw,
    soil_N_total = r6b_layers$slni * 10000,
    soil_FC     = r6b_layers$sldul,
    soil_PWP    = r6b_layers$slll,
    soil_SP     = r6b_layers$slsat * 100
  )
  
  d6_weather <- data.frame(
    location_id = "RWBU",
    country = "Rwanda",
    station_id = r6c_station$wst_id,
    longitude = r6c_station$wst_long,
    latitude = r6c_station$wst_lat,
    elevation = r6c_station$wst_elev,
    date = r6c_daily$w_date,
    tmax = r6c_daily$tmax,
    tmin = r6c_daily$tmin,
    prec = r6c_daily$rain,
    srad = r6c_daily$srad,
    wspd = r6c_daily$wind / 86.4,  # km/d -> m/s
    rhum = r6c_daily$rhum
  )
  
  # RWBU icnh4/icno3 are NA throughout - genuine source gap, not a read error
  d6_iclayers <- data.frame(
    trial_id = r6a_iclayers$exname,
    depth = r6a_iclayers$icbl,
    date = r6a_prevcrop$icdat[match(r6a_iclayers$exname, r6a_prevcrop$exname)],
    soil_NH4 = as.numeric(NA),
    soil_NO3 = as.numeric(NA),
    soil_VWC = r6a_iclayers$ich2o
  )
  
  ############################################################
  # Combine field management, soil profiles, and weather across all 5 sites
  ############################################################
  
  d <- rbind(d2a, d3a, d4a, d5a, d6a)
  d_weather   <- rbind(d2_weather, d3_weather, d4_weather, d5_weather, d6_weather)
  
  # static soil profiles: map the soil_id used above to the trial(s) it describes
  d_soilprofile <- rbind(d2_soil, d3_soil, d4_soil, d5_soil, d6_soil)
  soil_trials <- data.frame(
    trial_id = c("BEOU150001","BEOU150001", "ETBA140001","ETBA140001",
                 "GHKP080001","GHKP090001", "MANT100001","MANT100001", "RWBU","RWBU"),
    new_id = c("BEOU1401","BEOU1501", "ETBA1301","ETBA1401",
               "GHKP0801","GHKP0901", "MANT0901","MANT1001", "RWBU1401","RWBU1501")
  )
  d_soilprofile <- merge(d_soilprofile, soil_trials, by = "trial_id")
  d_soilprofile$trial_id <- d_soilprofile$new_id
  d_soilprofile$new_id <- NULL
  # SCS texture codes -> accepted terms
  texture_lookup <- c("CL" = "clay loam", "LOSA" = "loamy sand", "SALO" = "sandy loam")
  d_soilprofile$soil_texture <- texture_lookup[d_soilprofile$soil_texture]
  
  ############################################################
  # Observations (txt files; "na" flags missing values)
  ############################################################
  r7  <- read.delim(f7,  na.strings = c("na", "")) # daily obs, average
  r8  <- read.delim(f8,  na.strings = c("na", "")) # daily obs, sd (no CroP sd in source)
  r9  <- read.delim(f9,  na.strings = c("na", "")) # soil water/N by layer, average
  r10 <- read.delim(f10, na.strings = c("na", "")) # soil water/N by layer, sd
  r11 <- read.delim(f11, na.strings = c("na", "")) # season summary, average (one row per experiment)
  r12 <- read.delim(f12, na.strings = c("na", "")) # season summary, sd
  
  ### season summary (r11/r12) -> wide
  # grain yield is reported as dry matter (codebook: "t/DM/ha"), so yield_moisture = 0
  i11 <- match(d$trial_id, r11$Experiment.name)
  i12 <- match(d$trial_id, r12$Experiment.name)
  d$yield <- r11$Yield[i11] * 1000          # t/ha -> kg/ha
  d$yield_sd_ <- r12$Yield[i12] * 1000
  d$yield_part <- "grain"
  d$yield_moisture <- 0
  # summary MaxLAI (and its sd) is exactly the maximum of the daily LAI series (verified),
  # and summary Biom.ma goes into the long table with the daily series (see below)
  
  ### daily crop growth observations (r7/r8)
  # the same date is sometimes split across separate rows; collapse to one row per date
  r7_agg <- aggregate(cbind(Yield, Biom, LAI, CroN, CroP, SoilWTot.MaxMeasDepth, SoilNTot.MaxMeasDepth) ~ Experiment.name + Date,
                      data = r7, FUN = function(x) x[!is.na(x)][1], na.action = na.pass)
  r8_agg <- aggregate(cbind(Yield, Biom, LAI, CroN, SoilWTot.MaxMeasDepth, SoilNTot.MaxMeasDepth) ~ Experiment.name + Date,
                      data = r8, FUN = function(x) x[!is.na(x)][1], na.action = na.pass)
  
  r7a <- merge(r7_agg, r8_agg, by = c("Experiment.name", "Date"), suffixes = c("", "_sd"), all.x = TRUE)
  
  # the daily "Yield" is only reported at maturity and equals the season-summary yield,
  # which is already in the wide table; it is not repeated here
  d_daily <- data.frame(
    trial_id = r7a$Experiment.name,
    date = as.character(as.Date(r7a$Date)),
    dmy_total = r7a$Biom * 1000,        # t/ha -> kg/ha
    dmy_total_sd_ = r7a$Biom_sd * 1000,
    LAI = r7a$LAI,
    LAI_sd_ = r7a$LAI_sd,
    crop_N = r7a$CroN,
    crop_N_sd_ = r7a$CroN_sd,
    crop_P = r7a$CroP,
    soil_SWC = r7a$SoilWTot.MaxMeasDepth,
    soil_SWC_sd_ = r7a$SoilWTot.MaxMeasDepth_sd,
    soil_N_min_total = r7a$SoilNTot.MaxMeasDepth,
    soil_N_min_total_sd_ = r7a$SoilNTot.MaxMeasDepth_sd
  )
  value_cols <- c("dmy_total", "LAI", "crop_N", "crop_P", "soil_SWC", "soil_N_min_total")
  # drop only rows where every measured variable is NA
  d_daily <- d_daily[rowSums(!is.na(d_daily[value_cols])) > 0, ]
  
  # biomass, crop N/P and profile mineral N at maturity from the season summary belong
  # with the daily series, dated at maturity. Where both tables report a value at the
  # maturity date they are identical (verified; except BEOU1401 biomass, see ISSUES), so
  # the summary only fills gaps in the daily rows; BEOU1501 has no daily row at its
  # maturity date and gets a new row
  i_sd <- match(r11$Experiment.name, r12$Experiment.name)
  d_mat <- data.frame(
    trial_id = r11$Experiment.name,
    date = r11$Mat.date,
    dmy_total = r11$Biom.ma * 1000,        # t/ha -> kg/ha
    dmy_total_sd_ = r12$Biom.ma[i_sd] * 1000,
    crop_N = r11$CroN.ma,
    crop_N_sd_ = r12$CroN.ma[i_sd],
    crop_P = r11$CroP.ma,
    soil_N_min_total = r11$SoilN
  )
  mat_vars <- c("dmy_total", "dmy_total_sd_", "crop_N", "crop_N_sd_", "crop_P", "soil_N_min_total")
  i_mat <- match(paste(d_mat$trial_id, d_mat$date), paste(d_daily$trial_id, d_daily$date))
  for (v in mat_vars) {
    j <- which(!is.na(i_mat) & !is.na(d_mat[[v]]))
    k <- i_mat[j]
    fill <- is.na(d_daily[[v]][k])
    # only fill gaps; where both tables have a value the daily one is kept
    d_daily[[v]][k[fill]] <- d_mat[[v]][j[fill]]
  }
  d_mat <- d_mat[is.na(i_mat), ]
  d_mat <- d_mat[rowSums(!is.na(d_mat[mat_vars])) > 0, ]
  
  ### in-season soil water and mineral N by layer (r9/r10)
  r9$Date  <- as.Date(r9$Date,  format = "%d.%m.%Y")   # r9's own format
  r10$Date <- as.Date(r10$Date, format = "%Y-%m-%d")   # r10's own format
  r9_10 <- merge(r9, r10, by = c("Experiment.name", "Date", "Soil.layer.base.depth"),
                 suffixes = c("", "_sd"), all.x = TRUE)
  
  # SoilW.layer.mm and SoilN.layer.kg.ha are derived from these values (see NOT PROCESSED)
  d_soilwn <- data.frame(
    trial_id = r9_10$Experiment.name,
    depth = r9_10$Soil.layer.base.depth,
    date  = as.character(r9_10$Date),
    soil_VWC = r9_10$SoilW.layer.cm3.cm3,
    soil_VWC_sd_ = r9_10$SoilW.layer.cm3.cm3_sd,
    soil_N = r9_10$SoilN.layer.ppm,      # mineral N (NO3+NH4), mg/kg
    soil_N_sd_ = r9_10$SoilN.layer.ppm_sd
  )
  d_soilwn <- d_soilwn[!is.na(d_soilwn$soil_VWC) | !is.na(d_soilwn$soil_N), ]
  
  ### initial conditions (per site, per layer)
  d_iclayers <- rbind(d2_iclayers, d3_iclayers, d4_iclayers, d5_iclayers, d6_iclayers)
  d_iclayers$date <- as.character(d_iclayers$date)
  d_iclayers <- d_iclayers[rowSums(!is.na(d_iclayers[c("soil_NH4","soil_NO3","soil_VWC")])) > 0, ]
  
  ### combine all long record types
  d_long <- carobiner::bindr(d_daily, d_mat, d_soilwn, d_iclayers, d_soilprofile)
  
  ### d date conversions
  d$land_prep_date <- as.character(d$land_prep_date)
  d$planting_date <- as.character(d$planting_date)
  d$emergence_date <- as.character(d$emergence_date)
  d$flowering_date <- as.character(d$flowering_date)
  d$maturity_date <- as.character(d$maturity_date)
  prevcrop_term_lookup <- c("fallow" = "none", "dry bean" = "common bean")
  d$previous_crop <- ifelse(
    d$previous_crop %in% names(prevcrop_term_lookup),
    prevcrop_term_lookup[d$previous_crop], d$previous_crop)
  
  d_weather$date <- as.character(d_weather$date)
  
  carobiner::write_files(path, meta, wide=d, long=d_long, wth=d_weather)
}
