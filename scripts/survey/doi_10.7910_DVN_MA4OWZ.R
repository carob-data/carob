# R script for "carob"
# license: GPL (>=3)

## NOTES
# Only 23 summary variables available in the CSV: PDF questionnaire has more
# Survey on SWC technology adoption and welfare impacts; no yield or crop data.
# Distance variables are in walking minutes, not km.
# Economic variables (productionvalueca2, HHincomeca_w) are in Tanzanian Shillings (TZS).
# Sexhh: 1=male, 0=female. District: 1=Kongwa (Dodoma Region), 0=Kiteto (Manyara Region).
# hh_size likely transformed; Values range 0-3
# crop_value skipped as 101 farms have zero farm size making per ha calculation unreliable.


carob_script <- function(path) {
  
"
Survey on adoption and impact of soil and water conservation (SWC) technologies
in Kongwa and Kiteto districts of Tanzania. Data covers 580 households with
demographic, socioeconomic and farm characteristics including SWC adoption status,
farm size, household income per capita, dietary diversity score and tropical
livestock units.
"

  uri <- "doi:10.7910/DVN/MA4OWZ"
  carob_group <- "survey"
  
  ff <- carobiner::get_data(uri, path, carob_group)
  
  meta <- carobiner::get_metadata(uri, path, carob_group, major=1, minor=1,
      publication = NA,
      carob_contributor = "Stella Muthoni",
      carob_date = "2026-06-24",
      data_type = "survey",
      carob_group = "survey",
      data_organization = "IITA",
      project = "Africa RISING",
      treatment_vars = "none",
      response_vars = "none",
      notes = NA,
      design = NA
   )
  
  f <- ff[basename(ff) == "SWC_data.csv"]
  r <- read.csv(f)
  
  # a few cases in the "1" block
  r$District[is.na(r$District)] <- 1
  
  d <- data.frame(
    hhid               = as.character(r$HHID),
    ##hh_size          = r$lnT_Hh_Size, # probably transformed
    age                = r$Ageh,
    farmer_gender      = ifelse(r$Sexhh == 1, "male", ifelse(r$Sexhh == 0, "female", NA)),
    education          = as.character(r$Educationhh),
    farmland           = r$Farmsize,
    cooperative_member = r$hhmember_group == "Yes",
    market_timeto      = r$dist_districtmarket,
    currency           = "TZS",
    TLU      = r$TLU,
    diet_diversity  = r$HDDS,
    hh_income       = r$HHincomeca_w,
    credit_access      = r$Any_credit == "Yes",
    country            = "Tanzania",
    adm1               = ifelse(r$District == 1, "Dodoma", "Manyara"),
    adm2               = ifelse(r$District == 1, "Kongwa", "Kiteto"),
    longitude          = ifelse(r$District == 1, 36.6104, 36.8266),
    latitude           = ifelse(r$District == 1, -5.9767,  -5.1963),
    geo_uncertainty    = ifelse(r$District == 1, 56168, 84918),
    geo_from_source    = FALSE,
    on_farm            = TRUE,
    is_survey          = TRUE,
    irrigated          = FALSE,
    trial_id           = NA_character_,
    crop               = NA_character_,
    yield_part         = NA_character_,
    yield              = NA_real_,
    yield_moisture     = NA_real_,
    yield_isfresh      = NA,
    planting_date      = NA_character_,
    N_fertilizer       = NA_real_,
    P_fertilizer       = NA_real_,
    K_fertilizer       = NA_real_
  )
  
  carobiner::write_files(meta, d, path = path)
}