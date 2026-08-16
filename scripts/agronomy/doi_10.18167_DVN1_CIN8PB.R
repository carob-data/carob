# R script for "carob"
# license: GPL (>=3)

## NOTES: 
# 5 sites: BEOU (Benin), ETBA (Ethiopia), GHKP (Ghana), MANT (Mali), RWBU (Rwanda), 2 experiments (seasons) each. 
# GHKP TRT_NAME encodes N-P rate and residue as free text, not parsed. 
# RWBU soil_id does not follow the SITEyyNNNN pattern used elsewhere. 
# RWBU initial-conditions layer block has a stray value outside the defined columns, left as NA rather than guessed. 
# BEOU soils sheet showed only a profile summary row, no deeper layer table - needs re-checking against the raw data


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
		data_organization = "CIRAD; UGHA; APNI; IUCN; CIMMYT; ICRISAT",
		publication = "",
		project = NA,
		carob_date = "2026-08-03",
		design = "on-farm calibration trials, 2 seasons per site, 5 sites",
		data_type = "trial",
		treatment_vars = "fertilizer N, fertilizer P, fertilizer K, organic amendment, previous crop, tillage",
		response_vars = "yield, aboveground biomass, LAI, crop N, crop P, soil water, soil N",
		carob_contributor = "Stella Muthoni",
		completion = 60,
		notes = NA,
		carob_completion = 60,
		carob_effort = 10
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
  
  
  ### Reading small tables for Benin farm management, soils and weather
  r2a_exp  <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=19, n_max=2)
  
  r2a_prevcrop <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=27, n_max=2)
  r2a_prevcrop$exname <- ifelse(r2a_prevcrop$`!...1` == 1, "BEOU1501", "BEOU1401")
  r2a_prevcrop$icdat  <- as.Date(r2a_prevcrop$icdat)
  
  r2a_iclayers <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=33, n_max=6)
  r2a_iclayers$exname <- ifelse(r2a_iclayers$`!...1` == 1, "BEOU1501", "BEOU1401")
  
  r2a_tillage <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=44, n_max=2)
  r2a_tillage$exname <- ifelse(r2a_tillage$`! Definitions` == 1, "BEOU1501", "BEOU1401")
  
  r2a_planting <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=48, n_max=2)
  r2a_planting$exname <- ifelse(r2a_planting$`! Definitions` == 1, "BEOU1501", "BEOU1401")
  
  r2a_summary  <- carobiner::read.excel(f2, sheet="Field-mgmt", skip=67, n_max=2)
  r2a_summary$exname <- ifelse(r2a_summary$`#` == 1, "BEOU1501", "BEOU1401")
  
  r2b_profile <- carobiner::read.excel(f2, sheet="Soils", skip=12, n_max=1)
  r2b_layers  <- carobiner::read.excel(f2, sheet="Soils", skip=17, n_max=3)
  
  r2c_station <- carobiner::read.excel(f2, sheet="Weather", skip=3, n_max=1)    # station id, lat/long/elevation, avg temp, temp amplitude
  r2c_daily   <- carobiner::read.excel(f2, sheet="Weather", skip=9, n_max=730)
  r2c_daily$exname <- ifelse(r2c_daily$`!YEAR` == 2014, "BEOU1401", "BEOU1501")
  
  d2_beou <- data.frame(
    trial_id  = r2a_iclayers$exname,
    depth     = r2a_iclayers$`icbl...2`,
    location_id = "BEOU",
    country   = "Benin",
    location  = r2a_exp$site_name[1],
    year      = ifelse(r2a_iclayers$exname == "BEOU1501", "2015", "2014"),
    latitude  = r2a_exp$fl_lat,
    longitude = r2a_exp$fl_long,
    elevation = r2a_exp$flele,
    geo_from_source = TRUE,
    treatment = r2a_exp$TRT_NAME[1],
    station_id = r2a_exp$wst_id[1],
    
    crop = "Maize",
    previous_crop = "Maize",
    residue_prevcrop = 0,
    residue_prevcrop_used = FALSE,
    residue_prevcrop_N = 0,


############ 
## this part does not look good. 
## instead make a data.frame for prevcrop and then merge by exname 

    soil_sample_date = as.Date(ifelse(r2a_iclayers$exname == "BEOU1501",
		    r2a_prevcrop$icdat[r2a_prevcrop$exname == "BEOU1501"],
		    r2a_prevcrop$icdat[r2a_prevcrop$exname == "BEOU1401"]), origin = "1970-01-01"),
    
    land_prep_traction = "Animal-drawn implement",
    tillage_date  = as.Date(ifelse(r2a_iclayers$exname == "BEOU1501",
		 r2a_tillage$`yyyy-mm-dd`[r2a_tillage$exname == "BEOU1501"],
		 r2a_tillage$`yyyy-mm-dd`[r2a_tillage$exname == "BEOU1401"])),
    tillage_depth = ifelse(r2a_iclayers$exname == "BEOU1501",
                           r2a_tillage$cm[r2a_tillage$exname == "BEOU1501"],
                           r2a_tillage$cm[r2a_tillage$exname == "BEOU1401"]),
    
    planting_date = as.Date(ifelse(r2a_iclayers$exname == "BEOU1501",
		 r2a_planting$`yyyy-mm-dd`[r2a_planting$exname == "BEOU1501"],
		 r2a_planting$`yyyy-mm-dd`[r2a_planting$exname == "BEOU1401"])),
    variety = "BEOU_CUL",
    variety_type = "Open pollinated variety (OPV)",
    plant_density = ifelse(r2a_iclayers$exname == "BEOU1501",
                           r2a_planting$`#/m2`[r2a_planting$exname == "BEOU1501"],
                           r2a_planting$`#/m2`[r2a_planting$exname == "BEOU1401"]) * 10000,
    row_spacing = ifelse(r2a_iclayers$exname == "BEOU1501",
                         r2a_planting$cm[r2a_planting$exname == "BEOU1501"],
                         r2a_planting$cm[r2a_planting$exname == "BEOU1401"]),
    
    
    emergence_date = as.Date(ifelse(r2a_iclayers$exname == "BEOU1501",
		  r2a_summary$pldae[r2a_summary$exname == "BEOU1501"],
		  r2a_summary$pldae[r2a_summary$exname == "BEOU1401"])),
    flowering_date = as.Date(ifelse(r2a_iclayers$exname == "BEOU1501",
		  r2a_summary$adat[r2a_summary$exname == "BEOU1501"],
		  r2a_summary$adat[r2a_summary$exname == "BEOU1401"])),
    maturity_date  = as.Date(ifelse(r2a_iclayers$exname == "BEOU1501",
		  r2a_summary$mdat[r2a_summary$exname == "BEOU1501"],
		  r2a_summary$mdat[r2a_summary$exname == "BEOU1401"])),
############

	planting_depth = 50, # unit, where do you get that from?
    
    irrigated = FALSE,
    OM_used   = FALSE,
    N_organic = NA,
    P_organic = NA,
    K_organic = NA,
    
    soil_id  = "BEOU150001",
    soil_slope = 0,
    soil_depth = 60,  # where do you get that from (soil_depth is not the same as the depth of a soil sample)?  


############ 
## this is not good. part does not look good. 
## r2a_iclayers has multiple rows, you are assigning it as if it had a single row.

    soil_NH4 = r2a_iclayers$icnh4,
    soil_NO3 = r2a_iclayers$icno3,
    soil_GWC = r2a_iclayers$ich2o / r2b_layers$slbdm,
    
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
    soil_saturation = r2b_layers$slsat,
    soil_texture = r2b_profile$sltx,
    
    temp = r2c_station$tav[1],
    temp_amplitude = r2c_station$tamp[1],
    
    # no fertilizer at BEOU - confirmed in source ("!No fertiliser")
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    N_splits = NA,
    fertilizer_amount = NA,
    fertilization_method = NA
  )
  
  
  
  ### Reading small tables for Ethiopia farm management, soils and weather
  r3a_exp      <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=19, n_max=2)   # experiment metadata
  
  r3a_prevcrop <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=27, n_max=2)   # previous crop and residue
  r3a_prevcrop$exname <- ifelse(r3a_prevcrop$`!...1` == 1, "ETBA1401", "ETBA1301")
  r3a_prevcrop$icdat  <- as.Date(r3a_prevcrop$icdat)
  
  r3a_iclayers <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=33, n_max=14)  # initial soil water/N by depth, 7 depths x 2 experiments
  r3a_iclayers$exname <- ifelse(r3a_iclayers$`!` == 1, "ETBA1401", "ETBA1301")
  
  r3a_tillage <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=51, n_max=2)    # tillage event
  r3a_tillage$exname <- c("ETBA1401", "ETBA1301")   # id column blank in source, assigned by row position
  r3a_tillage$`yyyy-mm-dd` <- as.Date(r3a_tillage$`yyyy-mm-dd`)
  
  r3a_planting <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=55, n_max=2)   # planting event
  r3a_planting$exname <- c("ETBA1401", "ETBA1301")  # id column blank in source, assigned by row position
  r3a_planting$`yyyy-mm-dd` <- as.Date(r3a_planting$`yyyy-mm-dd`)
  
  r3a_fert <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=66, n_max=6)       # fertilizer events, 3 x 2 experiments
  r3a_fert$exname <- ifelse(r3a_fert$`%` == 1, "ETBA1401", "ETBA1301")
  r3a_fert$`yyyy-mm-dd` <- as.Date(r3a_fert$`yyyy-mm-dd`)
  fert_totals_3 <- aggregate(cbind(`kg[N]/ha`, `kg[P]/ha`, `kg[K]/ha`) ~ exname, data = r3a_fert, sum)
  names(fert_totals_3) <- c("exname", "N_total", "P_total", "K_total")
  n_splits_3 <- aggregate(`kg[N]/ha` ~ exname, data = r3a_fert[r3a_fert$`kg[N]/ha` > 0, ], length)
  names(n_splits_3) <- c("exname", "n_splits")
  r3a_fert$product_kg <- ifelse(r3a_fert$`!Fertilizer type - code signification` == "Di-ammonium Phosphate", r3a_fert$`kg[N]/ha` / 0.18,
                          ifelse(r3a_fert$`!Fertilizer type - code signification` == "Urea", r3a_fert$`kg[N]/ha` / 0.46, NA))
  fert_amount_3 <- aggregate(product_kg ~ exname, data = r3a_fert, sum)
  
  r3a_summary  <- carobiner::read.excel(f3, sheet="Field-mgmt", skip=79, n_max=2)   # phenology dates
  r3a_summary$exname <- ifelse(r3a_summary$`#` == 1, "ETBA1401", "ETBA1301")
  r3a_summary$pldae  <- as.Date(r3a_summary$pldae)
  r3a_summary$adat   <- as.Date(r3a_summary$adat)
  r3a_summary$mdat   <- as.Date(r3a_summary$mdat)
  
  r3b_profile <- carobiner::read.excel(f3, sheet="Soils", skip=12, n_max=1)
  r3b_layers  <- carobiner::read.excel(f3, sheet="Soils", skip=17, n_max=7)
  
  r3c_station <- carobiner::read.excel(f3, sheet="Weather", skip=3, n_max=1)    # station id, lat/long/elevation
  r3c_daily   <- carobiner::read.excel(f3, sheet="Weather", skip=9, n_max=730)  # daily weather, 730 days from 2013-01-01
  r3c_daily$exname <- ifelse(r3c_daily$`!YEAR` == 2013, "ETBA1301", "ETBA1401")
  
  d3_etba <- data.frame(
    trial_id  = r3a_iclayers$exname,
    depth     = r3a_iclayers$icbl,
    location_id = "ETBA",
    country   = "Ethiopia",
    location  = r3a_exp$site_name[1],
    year      = ifelse(r3a_iclayers$exname == "ETBA1401", "2014", "2013"),
    latitude  = r3a_exp$fl_lat,
    longitude = r3a_exp$fl_long,
    elevation = r3a_exp$flele,
    geo_from_source = TRUE,
    treatment = r3a_exp$TRT_NAME[1],
    station_id = r3a_exp$wst_id[1],
    
    crop = "Maize",
    previous_crop = "Maize",
    residue_prevcrop = 0,
    residue_prevcrop_used = FALSE,
    residue_prevcrop_N = 0,
    soil_sample_date = as.Date(ifelse(r3a_iclayers$exname == "ETBA1401",
		    r3a_prevcrop$icdat[r3a_prevcrop$exname == "ETBA1401"],
		    r3a_prevcrop$icdat[r3a_prevcrop$exname == "ETBA1301"]), origin = "1970-01-01"),
    
    land_prep_traction = "Animal-drawn implement",
    tillage_date  = as.Date(ifelse(r3a_iclayers$exname == "ETBA1401",
		 r3a_tillage$`yyyy-mm-dd`[r3a_tillage$exname == "ETBA1401"],
		 r3a_tillage$`yyyy-mm-dd`[r3a_tillage$exname == "ETBA1301"])),
    tillage_depth = ifelse(r3a_iclayers$exname == "ETBA1401",
                           r3a_tillage$cm[r3a_tillage$exname == "ETBA1401"],
                           r3a_tillage$cm[r3a_tillage$exname == "ETBA1301"]),
    
    planting_date = as.Date(ifelse(r3a_iclayers$exname == "ETBA1401",
		 r3a_planting$`yyyy-mm-dd`[r3a_planting$exname == "ETBA1401"],
		 r3a_planting$`yyyy-mm-dd`[r3a_planting$exname == "ETBA1301"])),
    variety = "ETBA_CUL",
    variety_type = "Hybrid",
    plant_density = ifelse(r3a_iclayers$exname == "ETBA1401",
                           r3a_planting$`#/m2`[r3a_planting$exname == "ETBA1401"],
                           r3a_planting$`#/m2`[r3a_planting$exname == "ETBA1301"]) * 10000,
    row_spacing = ifelse(r3a_iclayers$exname == "ETBA1401",
                         r3a_planting$cm[r3a_planting$exname == "ETBA1401"],
                         r3a_planting$cm[r3a_planting$exname == "ETBA1301"]),
    planting_depth = 50,
    
    emergence_date = as.Date(NA),   # not recorded at ETBA
    flowering_date = as.Date(ifelse(r3a_iclayers$exname == "ETBA1401",
		  r3a_summary$adat[r3a_summary$exname == "ETBA1401"],
		  r3a_summary$adat[r3a_summary$exname == "ETBA1301"])),
    maturity_date  = as.Date(ifelse(r3a_iclayers$exname == "ETBA1401",
		  r3a_summary$mdat[r3a_summary$exname == "ETBA1401"],
		  r3a_summary$mdat[r3a_summary$exname == "ETBA1301"])),
    
    irrigated = FALSE,
    OM_used   = FALSE,
    N_organic = NA,
    P_organic = NA,
    K_organic = NA,
    
    soil_id  = "ETBA140001",
    soil_slope = 0,
    soil_depth = 120,
    soil_NH4 = r3a_iclayers$icnh4,
    soil_NO3 = r3a_iclayers$icno3,
    soil_GWC = r3a_iclayers$ich2o / r3b_layers$slbdm,
    
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
    soil_saturation = r3b_layers$slsat,
    soil_texture = r3b_profile$sltx,
    
    temp = r3c_station$tav[1],
    temp_amplitude = r3c_station$tamp[1],
    
    N_fertilizer = ifelse(r3a_iclayers$exname == "ETBA1401",
                          fert_totals_3$N_total[fert_totals_3$exname == "ETBA1401"],
                          fert_totals_3$N_total[fert_totals_3$exname == "ETBA1301"]),
    P_fertilizer = ifelse(r3a_iclayers$exname == "ETBA1401",
                          fert_totals_3$P_total[fert_totals_3$exname == "ETBA1401"],
                          fert_totals_3$P_total[fert_totals_3$exname == "ETBA1301"]),
    K_fertilizer = ifelse(r3a_iclayers$exname == "ETBA1401",
                          fert_totals_3$K_total[fert_totals_3$exname == "ETBA1401"],
                          fert_totals_3$K_total[fert_totals_3$exname == "ETBA1301"]),
    N_splits = ifelse(r3a_iclayers$exname == "ETBA1401",
                      n_splits_3$n_splits[n_splits_3$exname == "ETBA1401"],
                      n_splits_3$n_splits[n_splits_3$exname == "ETBA1301"]),
    fertilizer_amount = ifelse(r3a_iclayers$exname == "ETBA1401",
                               fert_amount_3$product_kg[fert_amount_3$exname == "ETBA1401"],
                               fert_amount_3$product_kg[fert_amount_3$exname == "ETBA1301"]),
    fertilization_method = "Broadcast, incorporated"
  )
  
  ### Reading small tables for Ghana farm management, soils and weather
  r4a_exp      <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=19, n_max=2)
  r4a_prevcrop <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=27, n_max=2)
  r4a_prevcrop$exname <- ifelse(r4a_prevcrop$`!...1` == 1, "GHKP0801", "GHKP0901")
  r4a_prevcrop$icdat  <- as.Date(r4a_prevcrop$icdat)
  
  r4a_iclayers <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=33, n_max=12)  # 6 depths x 2 experiments
  r4a_iclayers$exname <- ifelse(r4a_iclayers$`!...1` == 1, "GHKP0801", "GHKP0901")
  
  # GHKP has no tillage
  
  r4a_planting <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=53, n_max=2)
  r4a_planting$exname <- ifelse(r4a_planting$`! Definitions` == 1, "GHKP0801", "GHKP0901")
  r4a_planting$`yyyy-mm-dd` <- as.Date(r4a_planting$`yyyy-mm-dd`)
  
  r4a_fert <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=64, n_max=8)
  r4a_fert$exname <- ifelse(r4a_fert$`%` == 1, "GHKP0801", "GHKP0901")
  r4a_fert$product_kg <- ifelse(r4a_fert$`!Fertilizer type - code signification` == "Urea", r4a_fert$`kg[N]/ha` / 0.46,
                                ifelse(r4a_fert$`!Fertilizer type - code signification` == "Triple Super Phosphate", r4a_fert$`kg[P]/ha` / 0.1923,
		     r4a_fert$`kg[K]/ha` / 0.498))
  fert_totals <- aggregate(cbind(`kg[N]/ha`, `kg[P]/ha`, `kg[K]/ha`) ~ exname, data = r4a_fert, sum)
  names(fert_totals) <- c("exname", "N_total", "P_total", "K_total")
  n_splits <- aggregate(`kg[N]/ha` ~ exname, data = r4a_fert[r4a_fert$`kg[N]/ha` > 0, ], length)
  names(n_splits) <- c("exname", "n_splits")
  fert_amount <- aggregate(product_kg ~ exname, data = r4a_fert, sum)
  
  r4a_summary <- carobiner::read.excel(f4, sheet="Field-mgmt", skip=79, n_max=2)
  r4a_summary$exname <- ifelse(r4a_summary$`#` == 1, "GHKP0801", "GHKP0901")
  r4a_summary$pldae  <- as.Date(r4a_summary$pldae)
  r4a_summary$adat   <- as.Date(r4a_summary$adat)
  r4a_summary$mdat   <- as.Date(r4a_summary$mdat)
  
  r4b_profile <- carobiner::read.excel(f4, sheet="Soils", skip=12, n_max=2)   # two profiles
  r4b_layers  <- carobiner::read.excel(f4, sheet="Soils", skip=18, n_max=12)  # A's 6 depths then C's 6 depths
  r4b_layers$soil_id <- ifelse(r4b_layers$`%` == "A", "GHKP080001", "GHKP090001")
  
  r4c_station <- carobiner::read.excel(f4, sheet="Weather", skip=3, n_max=1)   # station id, lat/long/elevation, avg temp, temp amplitude
  r4c_daily <- carobiner::read.excel(f4, sheet="Weather", skip=9, n_max=730)
  r4c_daily$exname <- ifelse(r4c_daily$`!YEAR` == 2008, "GHKP0801", "GHKP0901")
  
  d4_ghkp <- data.frame(
    trial_id  = r4a_iclayers$exname,
    depth     = r4a_iclayers$`icbl...2`,
    location_id = "GHKP",
    country   = "Ghana",
    location  = r4a_exp$site_name[1],
    year      = ifelse(r4a_iclayers$exname == "GHKP0801", "2008", "2009"),
    latitude  = r4a_exp$fl_lat,
    longitude = r4a_exp$fl_long,
    elevation = r4a_exp$flele,
    geo_from_source = TRUE,
    treatment = ifelse(r4a_iclayers$exname == "GHKP0801",
                       r4a_exp$TRT_NAME[r4a_exp$exname == "GHKP0801"],
                       r4a_exp$TRT_NAME[r4a_exp$exname == "GHKP0901"]),
    station_id = r4a_exp$wst_id[1],
    
    crop = "Maize",
    previous_crop = ifelse(r4a_iclayers$exname == "GHKP0801",
                           r4a_prevcrop$icpcr[r4a_prevcrop$exname == "GHKP0801"],
                           r4a_prevcrop$icpcr[r4a_prevcrop$exname == "GHKP0901"]),
    residue_prevcrop = 0,
    residue_prevcrop_used = FALSE,
    residue_prevcrop_N = 0,
    soil_sample_date = as.Date(ifelse(r4a_iclayers$exname == "GHKP0801",
		    r4a_prevcrop$icdat[r4a_prevcrop$exname == "GHKP0801"],
		    r4a_prevcrop$icdat[r4a_prevcrop$exname == "GHKP0901"]), origin = "1970-01-01"),
    
    # no tillage at GHKP - confirmed "!No Tillage" in source
    land_prep_traction = NA,
    tillage_date  = as.Date(NA),
    tillage_depth = NA,
    
    planting_date = as.Date(ifelse(r4a_iclayers$exname == "GHKP0801",
		 r4a_planting$`yyyy-mm-dd`[r4a_planting$exname == "GHKP0801"],
		 r4a_planting$`yyyy-mm-dd`[r4a_planting$exname == "GHKP0901"])),
    variety = "GHKP_CUL",
    variety_type = "Open Pollinated Variety (OPV)",
    plant_density = ifelse(r4a_iclayers$exname == "GHKP0801",
                           r4a_planting$`#/m2`[r4a_planting$exname == "GHKP0801"],
                           r4a_planting$`#/m2`[r4a_planting$exname == "GHKP0901"]) * 10000,
    row_spacing = ifelse(r4a_iclayers$exname == "GHKP0801",
                         r4a_planting$cm[r4a_planting$exname == "GHKP0801"],
                         r4a_planting$cm[r4a_planting$exname == "GHKP0901"]),
    planting_depth = 50,
    
    emergence_date = as.Date(ifelse(r4a_iclayers$exname == "GHKP0801",
		  r4a_summary$pldae[r4a_summary$exname == "GHKP0801"],
		  r4a_summary$pldae[r4a_summary$exname == "GHKP0901"])),
    flowering_date = as.Date(ifelse(r4a_iclayers$exname == "GHKP0801",
		  r4a_summary$adat[r4a_summary$exname == "GHKP0801"],
		  r4a_summary$adat[r4a_summary$exname == "GHKP0901"])),
    maturity_date  = as.Date(ifelse(r4a_iclayers$exname == "GHKP0801",
		  r4a_summary$mdat[r4a_summary$exname == "GHKP0801"],
		  r4a_summary$mdat[r4a_summary$exname == "GHKP0901"])),
    
    irrigated = FALSE,
    OM_used   = FALSE,
    N_organic = NA,
    P_organic = NA,
    K_organic = NA,
    
    soil_id  = r4b_layers$soil_id,
    soil_slope = 0,
    soil_depth = 100,
    soil_NH4 = r4a_iclayers$icnh4,
    soil_NO3 = r4a_iclayers$icno3,
    soil_GWC = r4a_iclayers$ich2o / r4b_layers$slbdm,
    
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
    soil_saturation = r4b_layers$slsat,
    soil_texture = r4b_profile$sltx[match(r4b_layers$soil_id, r4b_profile$soil_id)],
    
    temp = r4c_station$tav[1],
    temp_amplitude = r4c_station$tamp[1],
    
    N_fertilizer = ifelse(r4a_iclayers$exname == "GHKP0801",
                          fert_totals$N_total[fert_totals$exname == "GHKP0801"],
                          fert_totals$N_total[fert_totals$exname == "GHKP0901"]),
    P_fertilizer = ifelse(r4a_iclayers$exname == "GHKP0801",
                          fert_totals$P_total[fert_totals$exname == "GHKP0801"],
                          fert_totals$P_total[fert_totals$exname == "GHKP0901"]),
    K_fertilizer = ifelse(r4a_iclayers$exname == "GHKP0801",
                          fert_totals$K_total[fert_totals$exname == "GHKP0801"],
                          fert_totals$K_total[fert_totals$exname == "GHKP0901"]),
    N_splits = ifelse(r4a_iclayers$exname == "GHKP0801",
                      n_splits$n_splits[n_splits$exname == "GHKP0801"],
                      n_splits$n_splits[n_splits$exname == "GHKP0901"]),
    fertilizer_amount = ifelse(r4a_iclayers$exname == "GHKP0801",
                               fert_amount$product_kg[fert_amount$exname == "GHKP0801"],
                               fert_amount$product_kg[fert_amount$exname == "GHKP0901"]),
    fertilization_method = "Broadcast, incorporated"
  )
  
  ###
  r5a_exp      <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=19, n_max=2)
  r5a_prevcrop <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=27, n_max=2)
  r5a_prevcrop$exname <- ifelse(r5a_prevcrop$`!...1` == 1, "MANT1001", "MANT0901")
  r5a_prevcrop$icdat  <- as.Date(r5a_prevcrop$icdat)
  
  r5a_iclayers <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=33, n_max=14)  # 7 depths x 2 experiments
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
  names(fert_totals_5) <- c("exname", "N_total", "P_total", "K_total")
  n_splits_5 <- aggregate(`kg[N]/ha` ~ exname, data = r5a_fert[r5a_fert$`kg[N]/ha` > 0, ], length)
  names(n_splits_5) <- c("exname", "n_splits")
  r5a_fert$product_kg <- ifelse(r5a_fert$`!Fertilizer type - code signification` == "Urea", r5a_fert$`kg[N]/ha` / 0.46, NA)
  fert_amount_5 <- aggregate(product_kg ~ exname, data = r5a_fert, sum, na.rm = TRUE)
  
  r5a_summary <- carobiner::read.excel(f5, sheet="Field-mgmt", skip=86, n_max=2)
  r5a_summary$exname <- ifelse(r5a_summary$`#` == 1, "MANT1001", "MANT0901")
  r5a_summary$pldae  <- as.Date(r5a_summary$pldae)
  r5a_summary$adat   <- as.Date(r5a_summary$adat)
  r5a_summary$mdat   <- as.Date(r5a_summary$mdat)
  
  r5b_profile <- carobiner::read.excel(f5, sheet="Soils", skip=12, n_max=1)
  r5b_layers  <- carobiner::read.excel(f5, sheet="Soils", skip=17, n_max=7)
  
  r5c_station <- carobiner::read.excel(f5, sheet="Weather", skip=3, n_max=1)
  r5c_daily   <- carobiner::read.excel(f5, sheet="Weather", skip=9, n_max=730)
  r5c_daily$exname <- ifelse(r5c_daily$`!YEAR` == 2009, "MANT0901", "MANT1001")
  
  d5_mant <- data.frame(
    trial_id  = r5a_iclayers$exname,
    depth     = r5a_iclayers$icbl,
    location_id = "MANT",
    country   = "Mali",
    location  = r5a_exp$site_name[1],
    year      = ifelse(r5a_iclayers$exname == "MANT1001", "2010", "2009"),
    latitude  = r5a_exp$fl_lat,
    longitude = r5a_exp$fl_long,
    elevation = r5a_exp$flele,
    geo_from_source = TRUE,
    treatment = r5a_exp$TRT_NAME[1],
    station_id = r5a_exp$wst_id[1],
    
    crop = "Maize",
    previous_crop = "Cotton",
    residue_prevcrop = 0,
    residue_prevcrop_used = FALSE,
    residue_prevcrop_N = 0,
    soil_sample_date = as.Date(ifelse(r5a_iclayers$exname == "MANT1001",
		    r5a_prevcrop$icdat[r5a_prevcrop$exname == "MANT1001"],
		    r5a_prevcrop$icdat[r5a_prevcrop$exname == "MANT0901"]), origin = "1970-01-01"),
    
    land_prep_traction = "Animal-drawn implement",
    tillage_date  = as.Date(ifelse(r5a_iclayers$exname == "MANT1001",
		 r5a_tillage$`yyyy-mm-dd`[r5a_tillage$exname == "MANT1001"],
		 r5a_tillage$`yyyy-mm-dd`[r5a_tillage$exname == "MANT0901"])),
    tillage_depth = ifelse(r5a_iclayers$exname == "MANT1001",
                           r5a_tillage$cm[r5a_tillage$exname == "MANT1001"],
                           r5a_tillage$cm[r5a_tillage$exname == "MANT0901"]),
    
    planting_date = as.Date(ifelse(r5a_iclayers$exname == "MANT1001",
		 r5a_planting$`yyyy-mm-dd`[r5a_planting$exname == "MANT1001"],
		 r5a_planting$`yyyy-mm-dd`[r5a_planting$exname == "MANT0901"])),
    variety = "MANT_CUL",
    variety_type = "Open Pollinated Variety (OPV)",
    plant_density = ifelse(r5a_iclayers$exname == "MANT1001",
                           r5a_planting$`#/m2`[r5a_planting$exname == "MANT1001"],
                           r5a_planting$`#/m2`[r5a_planting$exname == "MANT0901"]) * 10000,
    row_spacing = ifelse(r5a_iclayers$exname == "MANT1001",
                         r5a_planting$cm[r5a_planting$exname == "MANT1001"],
                         r5a_planting$cm[r5a_planting$exname == "MANT0901"]),
    planting_depth = 50,
    
    emergence_date = as.Date(NA),   # not recorded at MANT
    flowering_date = as.Date(ifelse(r5a_iclayers$exname == "MANT1001",
		  r5a_summary$adat[r5a_summary$exname == "MANT1001"],
		  r5a_summary$adat[r5a_summary$exname == "MANT0901"])),
    maturity_date  = as.Date(ifelse(r5a_iclayers$exname == "MANT1001",
		  r5a_summary$mdat[r5a_summary$exname == "MANT1001"],
		  r5a_summary$mdat[r5a_summary$exname == "MANT0901"])),
    
    irrigated = FALSE,
    OM_used   = TRUE,
    N_organic = ifelse(r5a_iclayers$exname == "MANT1001",
                       r5a_organic$N_organic_kg[r5a_organic$exname == "MANT1001"],
                       r5a_organic$N_organic_kg[r5a_organic$exname == "MANT0901"]),
    P_organic = ifelse(r5a_iclayers$exname == "MANT1001",
                       r5a_organic$P_organic_kg[r5a_organic$exname == "MANT1001"],
                       r5a_organic$P_organic_kg[r5a_organic$exname == "MANT0901"]),
    K_organic = ifelse(r5a_iclayers$exname == "MANT1001",
                       r5a_organic$K_organic_kg[r5a_organic$exname == "MANT1001"],
                       r5a_organic$K_organic_kg[r5a_organic$exname == "MANT0901"]),
    
    soil_id  = "MANT100001",
    soil_slope = 0,
    soil_depth = 120,
    soil_NH4 = r5a_iclayers$icnh4,  
    soil_NO3 = r5a_iclayers$icno3,
    soil_GWC = r5a_iclayers$ich2o / r5b_layers$slbdm,
    
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
    soil_saturation = r5b_layers$slsat,
    soil_texture = r5b_profile$sltx,
    
    temp = r5c_station$tav[1],
    temp_amplitude = r5c_station$tamp[1],
    
    N_fertilizer = ifelse(r5a_iclayers$exname == "MANT1001",
                          fert_totals_5$N_total[fert_totals_5$exname == "MANT1001"],
                          fert_totals_5$N_total[fert_totals_5$exname == "MANT0901"]),
    P_fertilizer = ifelse(r5a_iclayers$exname == "MANT1001",
                          fert_totals_5$P_total[fert_totals_5$exname == "MANT1001"],
                          fert_totals_5$P_total[fert_totals_5$exname == "MANT0901"]),
    K_fertilizer = ifelse(r5a_iclayers$exname == "MANT1001",
                          fert_totals_5$K_total[fert_totals_5$exname == "MANT1001"],
                          fert_totals_5$K_total[fert_totals_5$exname == "MANT0901"]),
    N_splits = ifelse(r5a_iclayers$exname == "MANT1001",
                      n_splits_5$n_splits[n_splits_5$exname == "MANT1001"],
                      n_splits_5$n_splits[n_splits_5$exname == "MANT0901"]),
    fertilizer_amount = ifelse(r5a_iclayers$exname == "MANT1001",
                               fert_amount_5$product_kg[fert_amount_5$exname == "MANT1001"],
                               fert_amount_5$product_kg[fert_amount_5$exname == "MANT0901"]),
    fertilization_method = "Broadcast, incorporated"
  )
  
  ###
  r6a_exp      <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=19, n_max=2)
  r6a_prevcrop <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=27, n_max=2)
  r6a_prevcrop$exname <- ifelse(r6a_prevcrop$`!...1` == 1, "RWBU1501", "RWBU1401")
  r6a_prevcrop$icdat  <- as.Date(r6a_prevcrop$icdat)
  
  r6a_iclayers <- carobiner::read.excel(f6, sheet="Field-mgmt", skip=33, n_max=12)  # 6 depths x 2 experiments
  r6a_iclayers$exname <- ifelse(r6a_iclayers$`!` == 1, "RWBU1501", "RWBU1401")
  
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
  names(fert_totals_6) <- c("exname", "N_total", "P_total", "K_total")
  n_splits_6 <- aggregate(`kg[N]/ha` ~ exname, data = r6a_fert[r6a_fert$`kg[N]/ha` > 0, ], length)
  names(n_splits_6) <- c("exname", "n_splits")
  r6a_fert$product_kg <- ifelse(r6a_fert$`!Fertilizer type - code signification` == "Di-ammonium Phosphate", r6a_fert$`kg[N]/ha` / 0.18,
                                ifelse(r6a_fert$`!Fertilizer type - code signification` == "Urea", r6a_fert$`kg[N]/ha` / 0.46, NA))
  fert_amount_6 <- aggregate(product_kg ~ exname, data = r6a_fert, sum)
  
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
  r6c_daily$exname <- NA
  r6c_daily$exname[r6c_daily$w_date >= as.Date("2013-09-24") & r6c_daily$w_date <= as.Date("2014-01-22")] <- "RWBU1401"
  r6c_daily$exname[r6c_daily$w_date >= as.Date("2014-10-09") & r6c_daily$w_date <= as.Date("2015-02-04")] <- "RWBU1501"
  
  d6_rwbu <- data.frame(
    trial_id  = r6a_iclayers$exname,
    depth     = r6a_iclayers$icbl,
    location_id = "RWBU",
    country   = "Rwanda",
    location  = r6a_exp$site_name[1],
    year      = ifelse(r6a_iclayers$exname == "RWBU1501", "2014", "2013"),
    latitude  = r6a_exp$fl_lat,
    longitude = r6a_exp$fl_long,
    elevation = r6a_exp$flele,
    geo_from_source = TRUE,
    treatment = r6a_exp$TRT_NAME[1],
    station_id = r6a_exp$wst_id[1],
    
    crop = "Maize",
    previous_crop = "Dry bean",
    residue_prevcrop = 0,
    residue_prevcrop_used = FALSE,
    residue_prevcrop_N = 0,
    soil_sample_date = as.Date(ifelse(r6a_iclayers$exname == "RWBU1501",
		    r6a_prevcrop$icdat[r6a_prevcrop$exname == "RWBU1501"],
		    r6a_prevcrop$icdat[r6a_prevcrop$exname == "RWBU1401"]), origin = "1970-01-01"),
    
    land_prep_traction = "Animal-drawn implement",
    tillage_date  = as.Date(ifelse(r6a_iclayers$exname == "RWBU1501",
		 r6a_tillage$`yyyy-mm-dd`[r6a_tillage$exname == "RWBU1501"],
		 r6a_tillage$`yyyy-mm-dd`[r6a_tillage$exname == "RWBU1401"])),
    tillage_depth = ifelse(r6a_iclayers$exname == "RWBU1501",
                           r6a_tillage$cm[r6a_tillage$exname == "RWBU1501"],
                           r6a_tillage$cm[r6a_tillage$exname == "RWBU1401"]),
    
    planting_date = as.Date(ifelse(r6a_iclayers$exname == "RWBU1501",
		 r6a_planting$`yyyy-mm-dd`[r6a_planting$exname == "RWBU1501"],
		 r6a_planting$`yyyy-mm-dd`[r6a_planting$exname == "RWBU1401"])),
    variety = "RWBU_CUL",
    variety_type = "Open Pollinated Variety (OPV)",
    plant_density = ifelse(r6a_iclayers$exname == "RWBU1501",
                           r6a_planting$`#/m2`[r6a_planting$exname == "RWBU1501"],
                           r6a_planting$`#/m2`[r6a_planting$exname == "RWBU1401"]) * 10000,
    row_spacing = ifelse(r6a_iclayers$exname == "RWBU1501",
                         r6a_planting$cm[r6a_planting$exname == "RWBU1501"],
                         r6a_planting$cm[r6a_planting$exname == "RWBU1401"]),
    planting_depth = 50,
    
    emergence_date = as.Date(NA),
    flowering_date = as.Date(ifelse(r6a_iclayers$exname == "RWBU1501",
		  r6a_summary$adat[r6a_summary$exname == "RWBU1501"],
		  r6a_summary$adat[r6a_summary$exname == "RWBU1401"])),
    maturity_date  = as.Date(ifelse(r6a_iclayers$exname == "RWBU1501",
		  r6a_summary$mdat[r6a_summary$exname == "RWBU1501"],
		  r6a_summary$mdat[r6a_summary$exname == "RWBU1401"])),
    
    irrigated = FALSE,
    OM_used   = FALSE,
    N_organic = NA,
    P_organic = NA,
    K_organic = NA,
    
    soil_id  = "RWBU",
    soil_slope = 0,
    soil_depth = 100,
    soil_NH4 = NA,
    soil_NO3 = NA,
    soil_GWC = r6a_iclayers$ich2o / r6b_layers$slbdm,
    
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
    soil_saturation = r6b_layers$slsat,
    soil_texture = r6b_profile$sltx,
    
    temp = r6c_station$tav[1],
    temp_amplitude = r6c_station$tamp[1],
    
    N_fertilizer = ifelse(r6a_iclayers$exname == "RWBU1501",
                          fert_totals_6$N_total[fert_totals_6$exname == "RWBU1501"],
                          fert_totals_6$N_total[fert_totals_6$exname == "RWBU1401"]),
    P_fertilizer = ifelse(r6a_iclayers$exname == "RWBU1501",
                          fert_totals_6$P_total[fert_totals_6$exname == "RWBU1501"],
                          fert_totals_6$P_total[fert_totals_6$exname == "RWBU1401"]),
    K_fertilizer = ifelse(r6a_iclayers$exname == "RWBU1501",
                          fert_totals_6$K_total[fert_totals_6$exname == "RWBU1501"],
                          fert_totals_6$K_total[fert_totals_6$exname == "RWBU1401"]),
    N_splits = ifelse(r6a_iclayers$exname == "RWBU1501",
                      n_splits_6$n_splits[n_splits_6$exname == "RWBU1501"],
                      n_splits_6$n_splits[n_splits_6$exname == "RWBU1401"]),
    fertilizer_amount = ifelse(r6a_iclayers$exname == "RWBU1501",
                               fert_amount_6$product_kg[fert_amount_6$exname == "RWBU1501"],
                               fert_amount_6$product_kg[fert_amount_6$exname == "RWBU1401"]),
    fertilization_method = "Broadcast, incorporated"
  )
  
  #d_fieldmgmt_soils_weather <- rbind(d2_beou, d3_etba, d4_ghkp, d5_mant, d6_rwbu)
  
  r7  <- read.delim(f7,  header=TRUE) # daily observations, average: Yield, Biomass, LAI, CroN, CroP, SoilWTot, SoilNTot
  r8  <- read.delim(f8,  header=TRUE) # daily observations, sd: same variables except CroP (no sd reported for crop P in source)
  r9  <- read.delim(f9,  header=TRUE) # soil water/N by layer, average: depth-resolved SoilW/SoilN, multiple dates per experiment
  r10 <- read.delim(f10, header=TRUE) # soil water/N by layer, sd: same structure as r9, standard deviation
  r11 <- read.delim(f11, header=TRUE) # season summary, average: one row per experiment - Planting/Ant/Mat dates, final Yield, Biom.ma, MaxLAI, CroN.ma, CroP.ma, SoilN
  r12 <- read.delim(f12, header=TRUE) # season summary, sd: same variables as r11, standard deviation
  
  r13 <- readLines(f13)
  
  
  carobiner::write_files(path, meta, d)
}
