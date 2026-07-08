# R script for "carob"
# license: GPL (>=3)


## This need to be fixed. yield is per unit area. Now we have production per plot but that is of not much value
## without knowing plot size. We have plot size, so it can be computed. 
## (if we have all crops/fields for a household then _production_ can also be of interest))
# - yield is total harvest kg per crop per plot, NOT per hectare
# I think the same applies to fertilizer amounts and other amounts if there are any. Hence:
# - N_fertilizer max 3200 kg and fertilizer_amount max 10000 kg retained as reported
# - pineapple yield (8-16 kg) is low but plausible for small subsistence plots


# npk_content needs to be fixed. It needs to be P, not P2O5; and K, not K2O

# - new terms suggested: total_livestock (head count) and livestock_value (TZS)
## we have TLU (tropical livestock units) a weighted sum of all aninmals
## but here it is better to make a 'long' table: "hhid", "animal", "heads" and add that to write_files


## NOTES
# - 19 linked .dta files; 607 households across Tanzania, 2016 (TAMASA project)
# - GPS + admin for all 607 hh from linked dataset hdl:11529/10548230
#   (this dataset's own GPS is coded 999/missing)
# - debe = 20-litre tin; crop-specific kg/debe from google search
# - N/P/K rates for 58 focal plot households only; from published nutrient tables


## ISSUES
## what does r_cmty contain?
# - r_cmty not linked - poor geographic match rate (<30%)
# - not linked: fertavail (55 hh fertilizer unavailability) and groupmemb (social activities)


carob_script <- function(path) {
  
"
TAMASA Tanzania Agronomy Panel Survey (APS) 2016

Detailed farm household survey covering 607 households across 3 sentinel sites
in Tanzania (Karatu/Arumeru in Arusha, Nkasi in Rukwa). Covers crop harvest
quantities for main and short seasons across 36+ crop types, fertilizer use,
labor inputs, livestock, seed sources, and household demographics. Part of the
BMGF-funded Taking Maize Agronomy to Scale in Africa (TAMASA) project.
GPS from linked dataset hdl:11529/10548230.
"
  
  uri <- "hdl:11529/10548039"
  group <- "survey"
  ff <- carobiner::get_data(uri, path, group)
  ff_gps <- carobiner::get_data("hdl:11529/10548230", path, "survey")
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
      data_organization = "CIMMYT",
      publication = NA,
      project = "TAMASA",
      design = NA,
      data_type = "survey",
      treatment_vars = "none",
      response_vars = "yield",
      notes = "NA",
      carob_contributor = "Stella Muthoni",
      carob_date = "2026-07-07",
      carob_completion = 60,
      carob_effort = 12 #mostly trying to get as much as possible
  )


  ## ---- load various data files ----
  r_plotmc  <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_plotmc.dta"])  # yield, long rains
  r_plotmcc <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_plotmcc.dta"]) # yield, long rains
  r_plotsc  <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_plotsc.dta"])  # yield, short season
  r_plotscc <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_plotscc.dta"]) # yield, short season
  r_hh_plot <- haven::read_dta(ff[basename(ff) == "TZAPS16_hh_plot.dta"])      # plot data
  r_plotmlf <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_plotmlf.dta"]) # hh labour long rains
  r_plotmlh <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_plotmlh.dta"]) # hired labour long rains
  r_plotslf <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_plotslf.dta"]) # hh labor short rains
  r_plotslh <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_plotslh.dta"]) # hired labour short rains
  r_mroster <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_mroster.dta"]) # hh demographics
  r_lstock  <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_lstock.dta"])  #house hold level livestock
  r_othinp  <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_othinp.dta"])  #fertilizer input
  r_seed    <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_seed.dta"])    #seed_source
  r_weed    <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_weed.dta"])    #weeding type
  r_main    <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp.dta"]) #607 hh
  #r_fertavail <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_fertavail.dta"])
  #r_groupmemb <- haven::read_dta(ff[basename(ff) == "TZAPS16_hhfp_groupmemb.dta"])

  r_gps <- carobiner::read.excel(ff_gps[basename(ff_gps) == "TAMASA_TZ_APS_HH_2016.xlsx"], sheet="Data")

  
  ## ---- unit conversion function for harvest quantities ----
  calculate_yield <- function(df) {
    # Keep your reference vectors safe inside the function
    debe_kg <- c("maize"=20, "beans"=16, "cowpeas"=16, "pigeon_pea"=16, "sunflower"=8, "groundnut"=6, "sesame"=11, "sorghum"=15, "finger_millet"=17, "wheat"=16, "paddy"=11, "soyabeans"=16, "field_peas"=16, "green_gram"=16, "chick_peas"=16, "coffee"=10, "avocado"=13, "irish_potatoes"=15, "sweet_potatoes"=14, "tomatoes"=12, "chilies"=6, "fiwi"=16, "legume_other"=16, "other"=12, "pyrethrum"=4)
    bunch_kg <- c("banana"=15, "veg_other"=3)
    bale_kg  <- c("sugar_cane"=80, "tobacco"=80, "beans"=50, "cassava"=50, "groundnut"=50, "veg_other"=20)
    bucket_10_kg <- c("irish_potatoes"=8, "tomatoes"=8, "maize"=7, "avocado"=8, "pineapple"=8, "sunflower"=6, "beans"=8, "okra"=3)
    bucket_5_kg  <- c("irish_potatoes"=4, "tomatoes"=4, "beans"=4, "cowpeas"=4, "groundnut"=4, "field_peas"=4, "sunflower"=3)
    
    standard_units <- c(
      "bag_100_kg" = 100, "bag_90_kg" = 90, "bag_50_kg" = 50, 
      "bag_25_kg" = 25, "bag_10_kg" = 10, "bag_5_kg" = 5,
      "tonne" = 1000, "kg" = 1, "litre" = 0.75, 
      "cart" = 200, "canter" = 3000, "pickup" = 1000, "crate" = 20
    )
    # Step A: Generate standard multipliers matching THIS specific dataframe's rows
    mult <- standard_units[df$harvest_unit]
    
    # Step B: Identify unit rows for THIS dataframe
    is_debe   <- df$harvest_unit == "debe" & !is.na(df$harvest_unit)
    is_bunch  <- df$harvest_unit == "bunch" & !is.na(df$harvest_unit)
    is_bale   <- df$harvest_unit == "bale" & !is.na(df$harvest_unit)
    is_b10    <- df$harvest_unit == "bucket_10_litre" & !is.na(df$harvest_unit)
    is_b5     <- df$harvest_unit == "bucket_5_litre" & !is.na(df$harvest_unit)
    
    # Step C: Fill in crop-specific multipliers cleanly
    mult[is_debe]   <- ifelse(!is.na(debe_kg[df$crop[is_debe]]), debe_kg[df$crop[is_debe]], 12)
    mult[is_bunch]  <- ifelse(!is.na(bunch_kg[df$crop[is_bunch]]), bunch_kg[df$crop[is_bunch]], 5)
    mult[is_bale]   <- ifelse(!is.na(bale_kg[df$crop[is_bale]]), bale_kg[df$crop[is_bale]], 50)
    mult[is_b10]    <- ifelse(!is.na(bucket_10_kg[df$crop[is_b10]]), bucket_10_kg[df$crop[is_b10]], 7.5)
    mult[is_b5]     <- ifelse(!is.na(bucket_5_kg[df$crop[is_b5]]), bucket_5_kg[df$crop[is_b5]], 3.75)
    
    # Return the final calculated vector
    return(df$harvest_quantity * mult)
  }
  
  ### do not set crops to none. 
  crop_lookup <- c(
    "amaranths"="amaranth", "beans"="common bean", "chick_peas"="chickpea", "chilies"="chili pepper", "cowpeas"="cowpea", "field_peas"="pea", "finger_millet"="finger millet", "green_gram"="mung bean", "irish_potatoes"="potato", "onions"="onion", "paddy"="rice",    "pigeon_pea"="pigeon pea", "pumpkins"="pumpkin", "soyabeans"="soybean", "sugar_cane"="sugarcane", "sweet_potatoes"="sweetpotato",    "tomatoes"="tomato", "yams"="yam", "fiwi"="lablab", "peaches"="peach", "pigeon"="pigeon pea","Cowpeas"="cowpea", "Tomatoes"="tomato", "legume_other"="legume", "fruit_other"="fruit", "veg_other"="vegetable", "fence_tree"="none", "firewood/fodder"="firewood;forage crop", "fence"="none", "tree"="trees", "forage"="forage crop", "other"="unknown"
  )
  
  # define the valid hh_index
  valid_hh <- unique(r_main$hh_index)
  
  ## ---- YIELD TABLE (main + short rains) ----
  ## base data frame; all other data linked at hh_index level (values repeat per crop row)
  
  # long rains: r_plotmcc -> r_plotmc (plotmc_index) -> r_main (hh_index)
  d_maincrop <- merge(r_plotmcc, r_plotmc[r_plotmc$hh_index %in% valid_hh, c("plotmc_index", "hh_index")],
                      by="plotmc_index", all.x=TRUE)
  d_maincrop <- d_maincrop[, c("hh_index","plotmc_index","plotmcc_index","m_cropg","m_crop4","m_crop7","m_crop8")]
  names(d_maincrop) <- c("hh_index", "plot_id", "plot_crop_index", "yield_part", "crop", "harvest_quantity", "harvest_unit")
  ## could be investigated more, but removing these for now.
  d_maincrop <- d_maincrop[!is.na(d_maincrop$hh_index), ]
  d_maincrop$season        <- "long rains"
  d_maincrop$plot_id       <- as.character(d_maincrop$plot_id)
  d_maincrop$yield         <- calculate_yield(d_maincrop)
  d_maincrop$yield[d_maincrop$yield < 0 | d_maincrop$crop == "timber"] <- NA
  d_maincrop$yield_part    <- ifelse(d_maincrop$yield_part == "cereals", "grain",
         ifelse(d_maincrop$yield_part == "roots_tubers", "tubers",
                ifelse(d_maincrop$yield_part == "legumes_oilseeds", "seed", "fruit")))
  d_maincrop$yield_moisture <- NA
  d_maincrop$yield_isfresh  <- FALSE
  
  # short rains: r_plotscc -> r_plotsc (plotsc_index) -> r_main (hh_index)
  d_shortcrop <- merge(r_plotscc,
                       r_plotsc[r_plotsc$hh_index %in% valid_hh, c("plotsc_index","hh_index")],
                       by="plotsc_index", all.x=TRUE)
  d_shortcrop <- d_shortcrop[, c("hh_index","plotsc_index","plotscc_index","s_cropg","s_crop4","s_crop7","s_crop8")]
  names(d_shortcrop) <- c("hh_index","plot_id","plot_crop_index","yield_part","crop","harvest_quantity","harvest_unit")
  d_shortcrop$season        <- "short rains"
  d_shortcrop$plot_id       <- as.character(d_shortcrop$plot_id)
  d_shortcrop$yield         <- calculate_yield(d_shortcrop)
  d_shortcrop$yield[d_shortcrop$yield < 0] <- NA
  d_shortcrop$yield_part    <- ifelse(d_shortcrop$yield_part == "cereals", "grain",
          ifelse(d_shortcrop$yield_part == "roots_tubers", "tubers",
                 ifelse(d_shortcrop$yield_part == "legumes_oilseeds", "seed", "fruit")))
  d_shortcrop$yield_moisture <- NA
  ## why was this set to FALSE? 
  d_shortcrop$yield_isfresh  <- TRUE  
  
  # combine and keep only terminag-relevant columns
  d_yield_col <- c("hh_index","plot_id","yield_part","crop","season", "yield","yield_moisture","yield_isfresh")
  d_yield <- rbind(d_maincrop[, d_yield_col], d_shortcrop[, d_yield_col])

  d_yield <- d_yield[!is.na(d_yield$yield), ]
  
  # standardize crop names in d_yield using crop_lookup
  i_in <- d_yield$crop %in% names(crop_lookup)
  d_yield$crop[i_in] <- crop_lookup[d_yield$crop[i_in]]
  
  # fertilizer data
  r_othinp$hh_index <- r_main$hh_index[match(r_othinp$pp_index, r_main$pp_index)]
  standard_fert_units <- c("bag_100_kg"=100, "bag_50_kg"=50, "bag_25_kg"=25, 
    "bag_10_kg"=10, "bag_5_kg"=5, "kg"=1, "debe"=2)
  r_othinp$fert_qty_kg <- r_othinp$amnd4 * standard_fert_units[r_othinp$amnd5]
  r_othinp$fert_qty_kg[r_othinp$amnd5 %in% c("day","grams")] <- NA
  
  npk_content <- data.frame(
    inptype = c("urea","can_26_0_0","sa_21_0_0","dap","dap_and_urea",
                "npk","npk_17_17_17","npk_20_10_10","npk_20_20_0",
                "mijingu1100","urea_and_can"),
    N_frac = c(0.46, 0.26, 0.21, 0.18, 0.32, 0.17, 0.17, 0.20, 0.20, 0.00, 0.36),
    P_frac = c(0.00, 0.00, 0.00, 0.46, 0.23, 0.17, 0.17, 0.10, 0.20, 0.12, 0.00),
    K_frac = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.17, 0.17, 0.10, 0.00, 0.00, 0.00)
  )
  r_othinp <- merge(r_othinp, npk_content, by="inptype", all.x=TRUE)
  r_othinp$N_fertilizer <- r_othinp$fert_qty_kg * r_othinp$N_frac
  r_othinp$P_fertilizer <- r_othinp$fert_qty_kg * r_othinp$P_frac
  r_othinp$K_fertilizer <- r_othinp$fert_qty_kg * r_othinp$K_frac

# to be done:
#  org_npk_content <- data.frame(inptype = "chickenmanure", N_frac = 0.02, P_frac = 0.01, K_frac = 0.01 )

  d_fert <- aggregate(cbind(N_fertilizer, P_fertilizer, K_fertilizer) ~ hh_index, data=r_othinp, FUN=sum, na.rm=TRUE)
  ftype <- aggregate(inptype ~ hh_index, data=r_othinp[r_othinp$inptype %in% npk_content$inptype,], \(x) paste(x, collapse=";"))
  ft <- toupper(ftype$inptype)
  ft <- gsub("MIJINGU1100", "Minjingu 1100", ft)
  ft <- gsub("UREA", "urea", ft)
  ft <- gsub("_AND_", ";", ft)
  ft <- gsub("SA", "AS", ft)
  ftype$fertilizer_type <- sapply(strsplit(ft, ";"), \(x) paste(sort(unique(sub("_.*", "", x))), collapse=";"))
  d_fert <- merge(d_fert, ftype[, c("hh_index", "fertilizer_type")], all.x=TRUE)
  
  #seed source data
  r_seed$hh_index <- r_main$hh_index[match(r_seed$pp_index, r_main$pp_index)]
  d_seed <- data.frame(hh_index = r_seed$hh_index,
    season = ifelse(r_seed$season == "main", "long rains", "short rains"),
    crop = r_seed$seed1,seed_source = r_seed$seed6)
  d_seed <- d_seed[!duplicated(d_seed[, c("hh_index","season","crop")]),]
  
  # weeding data
  r_weed$hh_index <- r_main$hh_index[match(r_weed$pp_index, r_main$pp_index)]
  d_weed <- data.frame(hh_index=r_weed$hh_index,
                       weeding_method = r_weed$weeding_type)
  d_weed <- aggregate(weeding_method ~ hh_index, data = d_weed, 
                      FUN = function(x) paste(x, collapse = ", "))
  
  #demographics data
  r_mroster_valid <- r_mroster[r_mroster$hh_index %in% valid_hh,]
  demograph_col <- c("hh_index","mem_age","mem_gender", "mem_relationship",
                     "mem_marital","mem_education","mem_infinc_bin", "mem_forinc_bin")
  d_mroster <- r_mroster_valid[, demograph_col]
  names(d_mroster) <- c("hh_index","age","sex", "hh_relationship", "civil_status", 
                        "education","informal_income", "formal_income")
  d_mroster$is_head <- d_mroster$hh_relationship == "head"
  d_mroster$hh_income_source <- "none"
  d_mroster$hh_income_source[d_mroster$formal_income == "yes" & d_mroster$informal_income == "yes"] <- "formal/informal"
  d_mroster$hh_income_source[d_mroster$formal_income == "yes" & d_mroster$informal_income == "no"]  <- "formal"
  d_mroster$hh_income_source[d_mroster$formal_income == "no"  & d_mroster$informal_income == "yes"] <- "informal"
  d_mroster <- d_mroster[, !(names(d_mroster) %in% c("formal_income", "informal_income"))]
  
  d_mroster <- d_mroster[d_mroster$is_head == TRUE & !is.na(d_mroster$is_head), ]
  d_mroster <- d_mroster[, !names(d_mroster) %in% c("hh_relationship", "is_head")]
  
  #livestock data
  d_livestock <- aggregate(cbind(total_livestock = grade_cow_lstock_apr16,
                                 livestock_value = grade_cow_lstock_curval) ~ hh_index,
                           data=r_lstock, FUN=sum, na.rm=TRUE)
  
  #Plot data - best linkage was at hh_index so i aggregated; per crops values are repeated
  r_hh_plot_valid <- r_hh_plot[r_hh_plot$hh_index %in% valid_hh,]
  r_hh_plot_valid$area_ha <- ifelse(r_hh_plot_valid$area_unit == "acre", r_hh_plot_valid$area_amt * 0.4047, r_hh_plot_valid$area_amt)
  d_farmland <- aggregate(area_ha ~ hh_index, data = r_hh_plot_valid, FUN = sum, na.rm = TRUE)
  names(d_farmland) <- c("hh_index", "farmland")
  d_dist <- aggregate(distance_from_home ~ hh_index, data = r_hh_plot_valid, FUN = mean, na.rm = TRUE)
  names(d_dist) <- c("hh_index", "field_distance")
  d_plot <- merge(d_farmland, d_dist, by = "hh_index", all = TRUE)
  
  #farm labour data
  ## avoid using column numbers. If something changes it can be very hard to spot error
  r_plotmlf_valid <- r_plotmlf[r_plotmlf$hh_index %in% valid_hh, 2:19] #main family labor
  r_plotmlf_valid_hh <- aggregate(r_plotmlf_valid[, 4:18], by = list(hh_index = r_plotmlf_valid$hh_index),FUN = sum,na.rm = TRUE)
  r_plotmlf_valid_hh$farm_labour_hh <- rowSums(r_plotmlf_valid_hh[, 2:12], na.rm = TRUE)
  r_plotmlf_valid_hh <- r_plotmlf_valid_hh[, c("hh_index", "farm_labour_hh")]

  ## avoid using column numbers. If something changes it can be very hard to spot error  
  r_plotmlh_valid <- r_plotmlh[r_plotmlh$hh_index %in% valid_hh, 2:24] #main hired labor
  hired_cols <- c(4,5,6,8,9,10,12,13,14,16,17,18,20,21,22)
  r_plotmlh_valid$farm_labour_hired_plot <- rowSums(r_plotmlh_valid[, hired_cols], na.rm = TRUE)
  r_plotmlh_valid_hh <- aggregate(farm_labour_hired_plot ~ hh_index, data = r_plotmlh_valid, 
      FUN = sum, na.rm = TRUE)
  names(r_plotmlh_valid_hh) <- c("hh_index", "farm_labour_hired")
  
  d_labor_main <- merge(r_plotmlf_valid_hh, r_plotmlh_valid_hh, by = "hh_index", all = TRUE)
  d_labor_main$season <- "long rains"
  
  r_plotslf_valid <- r_plotslf[r_plotslf$hh_index %in% valid_hh, 2:19] #short family labor
  r_plotslf_valid_hh <- aggregate(r_plotslf_valid[, 4:18], by = list(hh_index = r_plotslf_valid$hh_index),FUN = sum,na.rm = TRUE)
  r_plotslf_valid_hh$farm_labour_hh <- rowSums(r_plotslf_valid_hh[, 2:12], na.rm = TRUE)
  r_plotslf_valid_hh <- r_plotslf_valid_hh[, c("hh_index", "farm_labour_hh")]
  
  ## avoid using column numbers. If something changes it can be very hard to spot error
  r_plotslh_valid <- r_plotslh[r_plotslh$hh_index %in% valid_hh, 2:24] #short hired labor
  hired_cols <- c(4,5,6,8,9,10,12,13,14,16,17,18,20,21,22)
  r_plotslh_valid$farm_labour_hired_plot <- rowSums(r_plotslh_valid[, hired_cols], na.rm = TRUE)
  r_plotslh_valid_hh <- aggregate(farm_labour_hired_plot ~ hh_index, data = r_plotslh_valid, 
      FUN = sum, na.rm = TRUE)
  names(r_plotslh_valid_hh) <- c("hh_index", "farm_labour_hired")
  
  d_labor_short <- merge(r_plotslf_valid_hh, r_plotslh_valid_hh, by = "hh_index", all = TRUE)
  d_labor_short$season <- "short rains"
  
  d_labor_all <- rbind(d_labor_main, d_labor_short)
  
  ## complete GPS + admin for all 607 hh; links via uniqid = hh_index
  
  d_gps <- data.frame(
    hh_index = r_gps$uniqid,
    adm1 = carobiner::fix_name(r_gps$region_name, "title"),
    adm2 = carobiner::fix_name(r_gps$district_name, "title"),
    adm3 = carobiner::fix_name(r_gps$ward, "title"),
    location = r_gps$village,
    site = r_gps$hamlet,
    latitude = r_gps$x_gps_latitude,
    longitude = r_gps$x_gps_longitude,
    elevation = r_gps$x_gps_altitude,
    geo_from_source = TRUE
  )
  
  ## ------ main data base with the valid hh_index --------
  
  z <- r_main[, grep("till_1_", names(r_main))]
  nms <- gsub("till_1_", "", names(z))
  nms[nms=="handhoe"] <- "hoeing"
  nms[nms=="zerotill"] <- "none"
  nms[nms=="ridging"] <- "ridge tillage"
  nms[nms=="noother"] <- "unknown" # no/other
  z <- sapply(z, tolower) == "true"
  r_main$land_prep_method <- apply(z, 1, \(x) paste(nms[x], collapse=";"))

  
  r_main$bc_crop <- as.character(r_main$bc_crop)
  i_in <- r_main$bc_crop %in% names(crop_lookup)
  r_main$bc_crop[i_in] <- crop_lookup[r_main$bc_crop[i_in]]
  r_main$bc_crop[r_main$bc_crop == "" | r_main$bc_crop == "NA"] <- "none"
  
  d1 <- data.frame(
    pp_index = r_main$pp_index,
    hh_index = r_main$hh_index,
    trial_id = as.character(r_main$hh_index),
    planting_date = as.character(r_main$year),
    is_survey = TRUE,
    on_farm = TRUE,
    previous_crop = r_main$bc_crop,
    intercropped = r_main$i1_bin == "yes",
    irrigated = r_main$irrig_bin == "yes",
    fertilizer_used = r_main$fertilizer_bin == "yes",
    fertilizer_cost = as.numeric(ifelse(r_main$exp_main_inorganic %in% c("NA",""), NA, r_main$exp_main_inorganic)),
    OM_used = r_main$manure_bin == "yes",
    OM_cost = as.numeric(ifelse(r_main$exp_main_organic %in% c("NA",""), NA, r_main$exp_main_organic)),
    seed_cost = as.numeric(ifelse(r_main$exp_main_seed %in% c("NA",""), NA, r_main$exp_main_seed)),
    land_tenure = r_main$macq,
    soil_quality = r_main$fertility,
    striga_damage = r_main$striga_bin == "yes",
    disease_incidence = r_main$disease_bin,
    land_prep_method = r_main$land_prep_method,
    weeding_times = as.integer(r_main$weedings),
    hh_size = r_main$hhsize,
    market_distance = ifelse(r_main$mktkm == -99, NA, r_main$mktkm),
    land_form = r_main$slpe
  )
  
  # merge other dbases to d1 by hh_index 
  d1 <- merge(d1, d_gps, by="hh_index", all.x=TRUE)
  d1 <- merge(d1, d_mroster, by="hh_index", all.x=TRUE)
  d1 <- merge(d1, d_plot, by="hh_index", all.x=TRUE)
  d1 <- merge(d1, d_fert, by="hh_index", all.x=TRUE)
  d1 <- merge(d1, d_weed, by="hh_index", all.x=TRUE)
  d1 <- merge(d1, d_livestock, by="hh_index", all.x=TRUE)
  
  d <- merge(d_yield, d_labor_all, by=c("hh_index", "season"), all.x=TRUE)
  d <- merge(d, d_seed, by=c("hh_index","season","crop"), all.x=TRUE)
  d <- merge(d, d1, by="hh_index", all.x=TRUE)
  
  ## after final merge - apply crop lookup to catch any from d_seed
  ## apply crop lookup to d (catches any from d_seed merge)
  i_in <- d$crop %in% names(crop_lookup)
  d$crop[i_in] <- crop_lookup[d$crop[i_in]]
  
  d$previous_crop[is.na(d$previous_crop)] <- "none"
  d$land_prep_method[is.na(d$land_prep_method) | trimws(d$land_prep_method) == ""] <- "unknown"
  
  d$location[trimws(d$location) == ""] <- NA
  d$land_prep_method[trimws(d$land_prep_method) == ""] <- NA
  d$previous_crop[trimws(d$previous_crop) == ""] <- "none"
  d$country  <- "Tanzania"
  d$currency <- "TZS"
  d$trial_id <- paste(d$trial_id, d$plot_id, sep="-")
  d$hhid <- as.character(d$hh_index)

  d$hh_index <- NULL
  d$pp_index <- NULL
  d <- unique(d)
  
  # all scripts must end like this
  carobiner::write_files(path, meta, d)
}

