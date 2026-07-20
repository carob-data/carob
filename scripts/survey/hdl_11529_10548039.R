# R script for "carob"
# license: GPL (>=3)

## Suggested variable: production, total_livestock and livestock value

# What does "yield_is_fresh" mean? I assumed it meant dry/fresh harvest and since its survey, these are likely dry weights

## NOTES
# - 19 linked .dta files; 607 households across Tanzania, 2016 (TAMASA project)
# - GPS + admin for all 607 hh from linked dataset hdl:11529/10548230
#   (this dataset's own GPS is coded 999/missing)
# - debe = 20-litre tin; crop-specific kg/debe from google search
# - N/P/K rates for 58 focal plot households only; from published nutrient tables
# - Suggested variables: production, total_livestock and livestock value

## ISSUES
## yield outliers & out-of-bounds check warnings
##  1. Tiny/zero area_ha: plot_index 179 (hh 51), 271 (hh 81), 459 (hh 146), 1369 (hh 495)
##  2. Normal-sized plots (0.05-0.5 acre) with implausibly large harvest_quantity 
#         for the crop (possible harvest_unit mismatch, e.g. bag_100_kg recorded where a smaller unit was meant):
##           plot_index 192 (hh 56, chickpea), 24 (hh 7, groundnut), 440 (hh 139, paddy)
### "hhid(s) do not match between long and wide records" is expected -
#       9 households (20, 377, 378, 10003, 10027, 10036, 10045, 10058, 10064)
#          have livestock records but no recorded crop plots in either season.

## what does r_cmty contain?
#     42 obs with 218 variables. These are the villages in which different hh is located
#      details the average prices of different labor and crops at village level; Could be relevant
#       if explored further and getting average costs per crop/labor_type; needs more effort/thinking
# - r_cmty not linked
# - not linked: r_cmty (costs for different labour and crops at village level);
#        fertavail (55 hh fertilizer unavailability) and groupmemb (social activities)


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
  r_plotmc  <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_plotmc.dta"], FALSE)  # yield, long rains
  r_plotmcc <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_plotmcc.dta"], FALSE) # yield, long rains
  r_plotsc  <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_plotsc.dta"], FALSE)  # yield, short season
  r_plotscc <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_plotscc.dta"], FALSE) # yield, short season
  r_hh_plot <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hh_plot.dta"], FALSE)      # plot data
  r_plotmlf <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_plotmlf.dta"], FALSE) # hh labour long rains
  r_plotmlh <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_plotmlh.dta"], FALSE) # hired labour long rains
  r_plotslf <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_plotslf.dta"], FALSE) # hh labor short rains
  r_plotslh <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_plotslh.dta"], FALSE) # hired labour short rains
  r_mroster <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_mroster.dta"], FALSE) # hh demographics
  r_lstock  <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_lstock.dta"], FALSE)  #house hold level livestock
  r_othinp  <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_othinp.dta"], FALSE)  #fertilizer input
  r_seed    <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_seed.dta"], FALSE)    #seed_source
  r_weed    <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_weed.dta"], FALSE)    #weeding type
  r_main    <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp.dta"], FALSE) #607 hh
  r_cmyt    <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_cmty.dta"], FALSE)
  #r_fertavail <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_fertavail.dta"])
  #r_groupmemb <- carobiner::read.dta(ff[basename(ff) == "TZAPS16_hhfp_groupmemb.dta"])

  r_gps <- carobiner::read.excel(ff_gps[basename(ff_gps) == "TAMASA_TZ_APS_HH_2016.xlsx"], sheet="Data")
  
  # define the valid hh_index
  valid_hh <- unique(r_main$hh_index)
  
  ## --------- calculate plot data ---------------
  r_hh_plot_valid <- r_hh_plot[r_hh_plot$hh_index %in% valid_hh,]
  r_hh_plot_valid$area_ha <- ifelse(r_hh_plot_valid$area_unit == "acre",
                                    r_hh_plot_valid$area_amt * 0.4047,r_hh_plot_valid$area_amt)

  d_plot_level <- r_hh_plot_valid[, c("plot_index","area_ha","distance_from_home","land_use_main",
                                      "land_use_minor","ownership","year_plot_acquired","certificate",
                                      "irrigation","erosion_control")]
  d_plot_level$irrigation <- d_plot_level$irrigation == "yes" 
  
  ## join directly onto r_plotmc and r_plotsc via x_parent_index -> plot_index
  r_plotmc <- merge(r_plotmc,  d_plot_level, by.x="x_parent_index", by.y="plot_index", all.x=TRUE)
  r_plotsc <- merge(r_plotsc,  d_plot_level, by.x="x_parent_index", by.y="plot_index", all.x=TRUE)
  
  ## ---- unit conversion function for harvest quantities ----
  calculate_production <- function(df) {
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
  
  # calculate yield from production
  ## ---- yield = production / area actually occupied by this crop ----
  calculate_yield <- function(production, plot_area_ha, crop_area_frac) {
    area_occupied <- plot_area_ha * crop_area_frac
    yield <- production / area_occupied
    yield[!is.finite(yield)] <- NA   # catches area_occupied == 0, NA area, or NA frac
    return(yield)
  }
  
  ### those not in terminag set to "unknown". 
  crop_lookup <- c(
    "amaranths"="amaranth", "beans"="common bean", "chick_peas"="chickpea", "chilies"="chili pepper",
    "cowpeas"="cowpea", "field_peas"="pea", "finger_millet"="finger millet", "green_gram"="mung bean", 
    "irish_potatoes"="potato", "onions"="onion", "paddy"="rice",    "pigeon_pea"="pigeon pea", "pumpkins"="pumpkin",
    "soyabeans"="soybean", "sugar_cane"="sugarcane", "sweet_potatoes"="sweetpotato",    "tomatoes"="tomato",
    "yams"="yam", "fiwi"="lablab", "peaches"="peach", "pigeon"="pigeon pea","Cowpeas"="cowpea", "Tomatoes"="tomato",
    "legume_other"="legume", "fruit_other"="fruit", "veg_other"="vegetable", "fence_tree"="unknown",
    "firewood/fodder"="firewood;forage crop", "fence"="unknown", "tree"="trees", "forage"="forage crop",
    "other"="unknown","pyrethrum"="pyrethrum"
  )
  
  #plot columns to enable yield calculation
  plot_cols <- c("area_ha","distance_from_home","land_use_main","land_use_minor",
                 "ownership","year_plot_acquired","certificate","irrigation","erosion_control")
  
  ## ---- YIELD TABLE (main + short rains) ----
  ## base data frame; all other data linked at hh_index level (values repeat per crop row)
  
  # long rains: r_plotmcc -> r_plotmc (plotmc_index) -> r_main (hh_index)
  d_maincrop <- merge(r_plotmcc, r_plotmc[r_plotmc$hh_index %in% valid_hh,c("plotmc_index","hh_index", plot_cols)],
                      by="plotmc_index", all.x=TRUE)
  d_maincrop <- d_maincrop[, c("hh_index","plotmc_index","plotmcc_index","m_cropg","m_crop4",
                               "m_crop5","m_crop7","m_crop8", plot_cols)]
  names(d_maincrop) <- c("hh_index","plot_id","plot_crop_index","yield_part","crop","split_area","harvest_quantity",
                         "harvest_unit",paste0("plot_", plot_cols))
  split_area_frac <- c("shfld_whole" = 1,"shfld_half" = 0.5,"shfld_3quarters" = 0.75,
                       "shfld_1quarter"  = 0.25,"shfld_ltquarter" = 0.125   # assumed an 1/8 
                       )
  d_maincrop$crop_area_frac <- split_area_frac[d_maincrop$split_area]
  ## could be investigated more, but removing these for now.
  d_maincrop <- d_maincrop[!is.na(d_maincrop$hh_index), ]
  d_maincrop$season        <- "long rains"
  d_maincrop$plot_id       <- as.character(d_maincrop$plot_id)
  d_maincrop$production <- calculate_production(d_maincrop)
  d_maincrop$production[d_maincrop$production < 0 | d_maincrop$crop == "timber"] <- NA
  d_maincrop$yield <- calculate_yield(d_maincrop$production, d_maincrop$plot_area_ha, d_maincrop$crop_area_frac)
  d_maincrop$plot_area <- d_maincrop$plot_area_ha*10000
  d_maincrop$yield_part    <- ifelse(d_maincrop$yield_part == "cereals", "grain",
         ifelse(d_maincrop$yield_part == "roots_tubers", "tubers",
                ifelse(d_maincrop$yield_part == "legumes_oilseeds", "seed", "fruit")))
  d_maincrop$yield_moisture <- NA
  d_maincrop$yield_isfresh  <- FALSE
  
  
  # long rains: r_plotscc -> r_plotsc (plotmc_index) -> r_main (hh_index)
  d_shortcrop <- merge(r_plotscc, r_plotsc[r_plotsc$hh_index %in% valid_hh,c("plotsc_index","hh_index", plot_cols)],
                       by="plotsc_index", all.x=TRUE)
  d_shortcrop <- d_shortcrop[, c("hh_index","plotsc_index","plotscc_index","s_cropg","s_crop4","s_crop5","s_crop7","s_crop8", plot_cols)]
  names(d_shortcrop) <- c("hh_index","plot_id","plot_crop_index","yield_part","crop","split_area","harvest_quantity",
                          "harvest_unit",paste0("plot_", plot_cols))
  d_shortcrop$crop_area_frac <- split_area_frac[d_shortcrop$split_area]
  ## could be investigated more, but removing these for now.
  d_shortcrop <- d_shortcrop[!is.na(d_shortcrop$hh_index), ]
  d_shortcrop$season        <- "short rains"
  d_shortcrop$plot_id       <- as.character(d_shortcrop$plot_id)
  d_shortcrop$production <- calculate_production(d_shortcrop)
  d_shortcrop$production[d_shortcrop$production < 0 | d_shortcrop$crop == "timber"] <- NA
  d_shortcrop$yield <- calculate_yield(d_shortcrop$production, d_shortcrop$plot_area_ha, d_shortcrop$crop_area_frac)
  d_shortcrop$plot_area <- d_shortcrop$plot_area_ha*10000
  d_shortcrop$yield_part    <- ifelse(d_shortcrop$yield_part == "cereals", "grain",
                               ifelse(d_shortcrop$yield_part == "roots_tubers", "tubers",
                               ifelse(d_shortcrop$yield_part == "legumes_oilseeds", "seed", "fruit")))
  d_shortcrop$yield_moisture <- NA
  d_shortcrop$yield_isfresh  <- FALSE 
  
  # combine and keep only terminag-relevant columns
  yield_plot_cols <- c("hh_index","plot_id","season","yield_part", "crop",
                       "plot_area","production","yield","yield_moisture","yield_isfresh",
                       "plot_irrigation","plot_distance_from_home","plot_ownership")
  d_yield_plot <- rbind(d_maincrop[, yield_plot_cols], d_shortcrop[, yield_plot_cols])
  names(d_yield_plot) <- c("hh_index","plot_id","season","yield_part", "crop",
                           "plot_area","production","yield","yield_moisture","yield_isfresh",
                           "irrigated","field_distance","land_ownedby")
  d_yield_plot <- d_yield_plot[!is.na(d_yield_plot$yield), ]
  
  # standardize crop names in d_yield using crop_lookup
  i_in <- d_yield_plot$crop %in% names(crop_lookup)
  d_yield_plot$crop[i_in] <- crop_lookup[d_yield_plot$crop[i_in]]
  
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
    P_frac = c(0.00, 0.00, 0.00, 0.201, 0.1005, 0.074188, 0.074188, 0.04364, 0.08728, 0.139648, 0.00),
    K_frac = c(0.00, 0.00, 0.00, 0.000, 0.0000, 0.141117, 0.141117, 0.08301, 0.00000, 0.000000, 0.00)
  )
  
  org_npk_content <- data.frame(
    inptype = c("chickenmanure"),N_frac  = c(0.02),P_frac  = c(0.01),K_frac  = c(0.01))
  
  all_npk_content <- rbind(npk_content, org_npk_content)
  
  r_othinp <- merge(r_othinp, all_npk_content, by="inptype", all.x=TRUE)
  r_othinp$N_fertilizer <- r_othinp$fert_qty_kg * r_othinp$N_frac
  r_othinp$P_fertilizer <- r_othinp$fert_qty_kg * r_othinp$P_frac
  r_othinp$K_fertilizer <- r_othinp$fert_qty_kg * r_othinp$K_frac
  
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
  r_lstock_valid <- r_lstock[r_lstock$hh_index %in% valid_hh, ]
  d_livestock <- aggregate(cbind(total_livestock = grade_cow_lstock_apr16,
                                 livestock_value = grade_cow_lstock_curval) ~ hh_index,
                           data=r_lstock_valid, FUN=sum, na.rm=TRUE)
  
  #farm labour data
  m_flab_cols <- c("m_flab_l_md","m_flab_l_wd","m_flab_l_cd","m_flab_p_md","m_flab_p_wd","m_flab_p_cd",
                   "m_flab_w_md","m_flab_w_wd","m_flab_w_cd","m_flab_r_md","m_flab_r_wd",
                   "m_flab_r_cd","m_flab_h_md","m_flab_h_wd","m_flab_h_cd")
  r_plotmlf_valid <- r_plotmlf[r_plotmlf$hh_index %in% valid_hh,c("hh_index", m_flab_cols)] #main family labor
  r_plotmlf_valid_hh <- aggregate(r_plotmlf_valid[, m_flab_cols],by = list(hh_index = r_plotmlf_valid$hh_index),
                                  FUN = sum, na.rm = TRUE)
  r_plotmlf_valid_hh$farm_labour_hh <- rowSums(r_plotmlf_valid_hh[, m_flab_cols], na.rm = TRUE)
  r_plotmlf_valid_hh <- r_plotmlf_valid_hh[, c("hh_index", "farm_labour_hh")]

  m_hlab_cols <- c("m_hlab_l_md","m_hlab_l_wd","m_hlab_l_cd","m_hlab_p_md","m_hlab_p_wd","m_hlab_p_cd",
                        "m_hlab_w_md","m_hlab_w_wd","m_hlab_w_cd","m_hlab_r_md","m_hlab_r_wd","m_hlab_r_cd",
                        "m_hlab_h_md","m_hlab_h_wd","m_hlab_h_cd")
  
  r_plotmlh_valid <- r_plotmlh[r_plotmlh$hh_index %in% valid_hh,c("hh_index", m_hlab_cols)] #main hired labor
  r_plotmlh_valid$farm_labour_hired_plot <- rowSums(r_plotmlh_valid[, m_hlab_cols], na.rm = TRUE)
  r_plotmlh_valid_hh <- aggregate(farm_labour_hired_plot ~ hh_index, data = r_plotmlh_valid,
                                  FUN = sum, na.rm = TRUE)
  names(r_plotmlh_valid_hh) <- c("hh_index", "farm_labour_hired")
  
  d_labor_main <- merge(r_plotmlf_valid_hh, r_plotmlh_valid_hh, by = "hh_index", all = TRUE)
  d_labor_main$season <- "long rains"
  
  s_flab_cols <- c("s_flab_l_md","s_flab_l_wd","s_flab_l_cd","s_flab_p_md","s_flab_p_wd","s_flab_p_cd",
                   "s_flab_w_md","s_flab_w_wd","s_flab_w_cd","s_flab_r_md","s_flab_r_wd","s_flab_r_cd",
                   "s_flab_h_md","s_flab_h_wd","s_flab_h_cd")
  r_plotslf_valid <- r_plotslf[r_plotslf$hh_index %in% valid_hh,c("hh_index", s_flab_cols)] #short family labor
  r_plotslf_valid_hh <- aggregate(r_plotslf_valid[, s_flab_cols],by = list(hh_index = r_plotslf_valid$hh_index),
                                  FUN = sum, na.rm = TRUE)
  r_plotslf_valid_hh$farm_labour_hh <- rowSums(r_plotslf_valid_hh[, s_flab_cols], na.rm = TRUE)
  r_plotslf_valid_hh <- r_plotslf_valid_hh[, c("hh_index", "farm_labour_hh")]
  
  s_hlab_cols <- c("s_hlab_l_md","s_hlab_l_wd","s_hlab_l_cd","s_hlab_p_md","s_hlab_p_wd","s_hlab_p_cd",
                        "s_hlab_w_md","s_hlab_w_wd","s_hlab_w_cd","s_hlab_r_md","s_hlab_r_wd","s_hlab_r_cd",
                        "s_hlab_h_md","s_hlab_h_wd","s_hlab_h_cd")
  r_plotslh_valid <- r_plotslh[r_plotslh$hh_index %in% valid_hh,c("hh_index", s_hlab_cols)] #short hired labor
  r_plotslh_valid$farm_labour_hired_plot <- rowSums(r_plotslh_valid[, s_hlab_cols], na.rm = TRUE)
  r_plotslh_valid_hh <- aggregate(farm_labour_hired_plot ~ hh_index, data = r_plotslh_valid, FUN = sum, na.rm = TRUE)
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
    #irrigated = r_main$irrig_bin == "yes",
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
  d1 <- merge(d1, d_fert, by="hh_index", all.x=TRUE)
  d1 <- merge(d1, d_weed, by="hh_index", all.x=TRUE)
  d1 <- merge(d1, d_livestock, by="hh_index", all.x=TRUE)
  
  d <- merge(d_yield_plot, d_labor_all, by=c("hh_index", "season"), all.x=TRUE)
  d <- merge(d, d_seed, by=c("hh_index","season","crop"), all.x=TRUE)
  d <- merge(d, d1, by="hh_index", all.x=TRUE)
  
  ## after final merge - apply crop lookup to catch any from d_seed
  ## apply crop lookup to d (catches any from d_seed merge)
  i_in <- d$crop %in% names(crop_lookup)
  d$crop[i_in] <- crop_lookup[d$crop[i_in]]
  
  d$previous_crop[is.na(d$previous_crop)] <- "unknown"
  d$land_prep_method[is.na(d$land_prep_method) | trimws(d$land_prep_method) == ""] <- "unknown"
  
  d$location[trimws(d$location) == ""] <- NA
  d$land_prep_method[trimws(d$land_prep_method) == ""] <- NA
  d$previous_crop[trimws(d$previous_crop) == ""] <- "unknown"
  d$country  <- "Tanzania"
  d$currency <- "TZS"
  d$trial_id <- paste(d$trial_id, d$plot_id, sep="-")
  d$hhid <- as.character(d$hh_index)

  d$hh_index <- NULL
  d$pp_index <- NULL
  d <- unique(d)
  
  ### Create a long table for livestock data
  animal_lookup <- c("local_cow" = "cattle","local_bull" = "cattle","local_calves" = "cattle",
    "improved_cow" = "cattle","improved_bull" = "cattle","improved_calves"= "cattle",
    "sheep"= "sheep","goat_local" = "goat","goat_dairy" = "goat","chicken_indigenous" = "chicken",
    "chicken_improved" = "chicken","pigs" = "pig","rabbits" = "rabbit","ducks_geese" = "duck;goose",   
    "other" = "unknown")
  ## livestock data — long table
  d_livestock2 <- data.frame(
    hhid   = as.character(r_lstock_valid $hh_index),
    animal = animal_lookup[r_lstock_valid $lstock_type],
    heads  = r_lstock_valid $grade_cow_lstock_apr16
  )
  d_livestock2 <- d_livestock2[!is.na(d_livestock2$hhid), ]
  
  # all scripts must end like this
  carobiner::write_files(path, meta, d, d_livestock2)
  #carobiner::write_files(path, meta, d)
}

