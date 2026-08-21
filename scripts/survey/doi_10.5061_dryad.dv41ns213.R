# R script for "carob"
# license: GPL (>=3)

##NOTES
# 907 households across 6 countries (Ethiopia 64, Kenya 191, Malawi 168,
# Rwanda 204, South Africa 102, Tanzania 178)
# Data description says 12 pilot sites (2 per country) but no XY provided. 
# Multiple crop_grown & yield columns; Focused only on "SPSC-" for crops
# Crop data coverage differ sharply by country.
#   - Ethiopia, Malawi, South Africa, Tanzania: crop data is well covered.
#   - ZERO crop data for Kenya and Rwanda.
# Livestock data well covered across the countries.
# Yield converted from raw kg to kg/ha using plot_area; capped per-crop
# against documented max_yield where available (few outliers set to NA).

#ISSUES
# extracted only crops and livestock databases but the file has >2000 columns
# No household id column in source - hhid is built as country + a per-country sequential number.
# Although description says "12 sites" no XY is found.
# Out of bounds hh_size and plot_area left as is
   # probably because it needs to be "field_size" not "plot_area"


# hhid do not match between long and wide as d_crops doesnt have Kenya/Rwanda
# Suggested new terms: expenses_education; mulching_used (logical); agroecozone; road_distance, expenses_living
#                      expenses_health; savings; animal_age, animal_sex

carob_script <- function(path) {
  
  "InnovAfrica project endline survey data for Ethiopia, Kenya, Malawi, Rwanda, South Africa and Tanzania

A consortium of 16 institutions comprising five institutions from Europe
and eleven institutions from Africa implemented a project entitled
'Innovations in Technology, Institutional and Extension Approaches
towards Sustainable Agriculture and enhanced Food and Nutritional
Security in Africa (InnovAfrica)' in six countries of eastern and
southern Africa namely Ethiopia, Kenya, Malawi, Rwanda, South Africa
and Tanzania from June 2017 to November 2021. The InnovAfrica project
collected endline data from 12 pilot sites (two sites per country) in
the third years of the project."
  
  uri <- "doi:10.5061/dryad.dv41ns213"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=9, minor=NA,
                                  data_organization = "SUA; ILRI",
                                  publication = NA,
                                  project = "InnovAfrica",
                                  design = "structured household questionnaire and focus group discussion, endline survey, 12 pilot sites (2 per country) across 6 countries",
                                  data_type = "survey",
                                  treatment_vars = NA,
                                  response_vars = "yield;",
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-08-12",
                                  notes = NA,
                                  carob_completion = 40,
                                  carob_effort = 4
  )
  
  f1 <- ff[basename(ff) == "Endline survey data_May 2022.r1.xlsx"]
  f3 <- ff[basename(ff) == "README_ELr1.txt"]   # Data dictionary
  
  r1 <- carobiner::read.excel(f1, sheet=1, skip=1)   # row 1 is a title, row 2 is the real header
  r1$hhid <- as.character(1:nrow(r1))
  
  currency_lookup <- c("Ethiopia" = "ETB","Kenya" = "KES","Malawi" = "MWK","Rwanda" = "RWF",
                       "South Africa" = "ZAR","Tanzania" = "TZS")
  
  # household-level fields, replicated across every d_crops/d_livestock row
  hh_base <- function() {
    hh_income_v <- r1$HOH_ONFARMINCOME + r1$HOH_OFFFARMINCOME
    hh_income_source_v <- ifelse(
      !is.na(r1$HOH_ONFARMINCOME) & r1$HOH_ONFARMINCOME > 0 & !is.na(r1$HOH_OFFFARMINCOME) & r1$HOH_OFFFARMINCOME > 0,
      "on-farm; off-farm",
      ifelse(!is.na(r1$HOH_ONFARMINCOME) & r1$HOH_ONFARMINCOME > 0, "on-farm",
             ifelse(!is.na(r1$HOH_OFFFARMINCOME) & r1$HOH_OFFFARMINCOME > 0, "off-farm", NA))
    )
    
    data.frame(
      hhid = r1$hhid,
      country = r1$COUNTRY,
      currency = currency_lookup[r1$COUNTRY],
      elevation = r1$`ALT (m asl)`,
      longitude = NA,
      latitude = NA,
      is_survey = TRUE,
      on_farm = FALSE,
      geo_from_source = FALSE,
      market_distance = r1$DISTANCE_MARKET,
      sex = r1$RESP_SEX,
      is_head = r1$RESP_REL_HH == "Head of Household",
      farmer_gender = r1$HOH_SEX,
      occupation = r1$HOH_OCCUPATION,
      hh_size = r1$HOH_NUMBER_HH_MEMBERS,
      hh_income = hh_income_v,
      hh_income_source = hh_income_source_v,
      farmland = r1$LAND_TOTALSIZE_HA,
      cropland = r1$LAND_AREAFOODCROPHA,
      grassland = r1$LAND_AREANATURALHA,
      
      agroecozone = r1$AGROECOZONE,
      road_distance = r1$DISTANCE_ROAD,
      expenses_living = r1$EXPENSES_LIVE,
      expenses_education = r1$EXPENSES_EDU,
      expenses_health = r1$EXPENSES_HEALTH,
      savings = r1$SAVINGS
    )
  }
  
  yn_flag <- function(x) ifelse(is.na(x), FALSE, x == 1)
  
  min_tillage_used     <- yn_flag(r1$`SPSC_FARMING_PRACTICES_Minimum tillage`)
  planting_pits_used   <- yn_flag(r1$`SPSC_FARMING_PRACTICES_Planting pits`)
  conventional_tillage_used <- yn_flag(r1$`SPSC_FARMING_PRACTICES_None of the above (Conventional)`)
  tied_ridging_used    <- yn_flag(r1$`SPSC_FARMING_PRACTICES_Tied ridging`)
  
  # collapse the four tillage-method 
  land_prep_term_lookup <- c("Minimum tillage" = "minimum tillage","Planting pits" = "basins",
    "Conventional" = "conventional","Tied ridging" = "tied ridges")
  
  land_prep_method <- mapply(function(a, b, c, d) {
    parts <- c(a, b, c, d)
    parts <- parts[parts != ""]
    if (length(parts) == 0) "none" else paste(parts, collapse = "; ")
  },
  ifelse(min_tillage_used, land_prep_term_lookup["Minimum tillage"], ""),
  ifelse(planting_pits_used, land_prep_term_lookup["Planting pits"], ""),
  ifelse(conventional_tillage_used, land_prep_term_lookup["Conventional"], ""),
  ifelse(tied_ridging_used, land_prep_term_lookup["Tied ridging"], "")
  )
  
  # standardize crop names against the accepted terminag crop vocabulary.
  crop_term_lookup <- c("Maize" = "maize","Sorghum" = "sorghum","Beans" = "common bean",
                        "Chickpeas" = "chickpea","Cow peas" = "cowpea","Pigeon Peas" = "pigeon pea",
                        "Millet" = "millet","Other" = "unknown")
  standardize_crop <- function(x) {
    out <- crop_term_lookup[x]
    ifelse(is.na(out) & !is.na(x), x, out)
  }
  
  spsc_area_ha <- r1$SPSC_AREA_CROP_MAIN_HA


## do not remove values except if they are truly impossible (e.g. negative yield)
## or perhaps a single crazy outlier
  
#  crop_max_yield <- c("maize" = 41500,"common bean" = 9000,"sorghum" = 18000,
#    "chickpea" = 6000,"cowpea" = 5000,"pigeon pea" = 12000)
  
#  cap_yield <- function(crop, yld) {
#    max_allowed <- crop_max_yield[crop]
#    ifelse(!is.na(max_allowed) & !is.na(yld) & yld > max_allowed, NA, yld)
#  }
  
  crop_v <- standardize_crop(r1$SPSC_SELECT_CROP)
  yield_v <- ifelse(!is.na(spsc_area_ha) & spsc_area_ha > 0, r1$SPSC_YIELD_CROP_MAIN / spsc_area_ha, NA)
  yield_v <- cap_yield(crop_v, yield_v) 
  
  d_crops <- cbind(hh_base(), data.frame(
    crop = standardize_crop(r1$SPSC_SELECT_CROP),
    plot_area = spsc_area_ha,
    yield = yield_v,
    crop_amount_sold = r1$SPSC_SOLD_CROP_MAIN,
    crop_price = r1$SPSC_PRICE_CROP_MAIN,
    crop_rotation = standardize_crop(r1$SPSC_CROP_ROTATION),
    intercropped = yn_flag(r1$`SPSC_FARMING_PRACTICES_Intercropping with legumes`),
    intercrops = standardize_crop(r1$SPSC_CROP_INTERCROP),
    cover_crop_used = yn_flag(r1$`SPSC_FARMING_PRACTICES_Cover-cropping`),
    land_prep_method = land_prep_method,
    mulching_used = yn_flag(r1$SPSC_FARMING_PRACTICES_Mulching)
  ))
  d_crops <- d_crops[!is.na(d_crops$crop), ]   # majority dropped here are Kenya/Rwanda, which have no crop data at all
  
  ### d_livestock: 8 livestock slots per household, reshaped long.
  animal_term_lookup <- c("Goat bucks" = "goat","Goat does (female)" = "goat","Heifers/weaners (females)" = "cattle",
    "Mature bulls" = "cattle","Mature Cows" = "cattle","New born calves (female)" = "cattle","New born calves (male)" = "cattle",
    "Young bulls" = "cattle","Pigs" = "pig","Poultry" = "poultry","Sheep" = "sheep")
  
  animal_age_lookup <- c("Goat bucks" = "mature","Goat does (female)" = "mature","Heifers/weaners (females)" = "young",
    "Mature bulls" = "mature","Mature Cows" = "mature","New born calves (female)" = "newborn",
    "New born calves (male)" = "newborn","Young bulls" = "young")
  
  animal_sex_lookup <- c("Goat bucks" = "male","Goat does (female)" = "female","Heifers/weaners (females)" = "female",
    "Mature bulls" = "male","Mature Cows" = "female","New born calves (female)" = "female","New born calves (male)" = "male",
    "Young bulls" = "male")
  
  standardize_animal <- function(x) {out <- animal_term_lookup[x]
    ifelse(is.na(out) & !is.na(x), x, out)
  }
  
  ## never use column numbers, always names
  livestock_slot_starts <- 2331 + (0:7) * 14
  d_livestock <- do.call(rbind, lapply(livestock_slot_starts, function(start) {
    raw_animal <- r1[[start + 1]]
    data.frame(
      hhid = r1$hhid,
      animal = standardize_animal(raw_animal),
      animal_age = animal_age_lookup[raw_animal],   # suggested term
      animal_sex = animal_sex_lookup[raw_animal],   # suggested term
      heads  = r1[[start + 3]]
    )
  }))
  d_livestock <- d_livestock[!is.na(d_livestock$animal) & d_livestock$animal != "", ]
 
  
  carobiner::write_files(path, meta, wide=d_crops, long=d_livestock)
}

