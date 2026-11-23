# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Exact planting dates are not given. Planting I was "just after harvesting rice" and Planting II "7 days after 1st planting" (data dictionary); the season ran from 2024-12-05 to 2025-03-13 (metadata). Both planting_date are set to "2024-12" and the planting order is kept in "treatment".
#Coordinates are from the publication of the 2022-2023 trials at the same three sites (doi:10.1016/j.jafr.2024.101603; dataset doi:10.21223/TJFYYD).
# Plant height and number of stems (at 45, 60 and 75 days after sowing) not processed; no unit is given for plant height.

carob_script <- function(path) {
  
  "
Dataset for: Potato production under zero tillage with straw mulch during 2024-2025 season in agronomic trials in the Khulna district, Bangladesh (Soil analysis and agronomic data)

Experimental trials were conducted using a strip-plot design in Fultola, Moshamari, and Hogalbunia, located in the Khulna district of Bangladesh, as part of the GIZ-PZTM project. The trials tested two potato varieties: BARI Alu 72 and BARI Alu 7, utilizing zero tillage with straw mulch (ZTSM) and two planting dates (1 and 2). This dataset includes results related to marketable tuber yield, fresh tuber yield, plant height, the number of stems per plant, and soil analysis. The soil analysis focused on characterization, fertility, salinity, and organic carbon content, with samples collected at the end of the season, from December 2024 to March 2025
"
  
  uri <- "doi:10.21223/P3/IEIKH5"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
      data_organization = "CIP",
      publication = NA,
      project = "GIZ-PZTM",
      design = "strip-plot",
      data_type = "on-farm experiment",
      treatment_vars = "variety;treatment",
      response_vars = "yield;yield_marketable",
      notes = NA,
      carob_contributor = "Maryam Yahya",
      carob_date = "2026-11-23",
      carob_completion = 85,
      carob_effort = 1.5
  )
  
  f <- ff[basename(ff) == "2_Agronomic data 24-25.xlsx"]
  r <- carobiner::read.excel(f)
  
  d <- data.frame(
    trial_id = paste("IEIKH5", r$location, sep = "_"),
    plot_id = as.character(r$plot_number),
    rep = as.integer(r$replication),
    variety = r$variety,
    treatment = r$planting,
    location = r$location,
    adm1 = "Khulna",
    country = "Bangladesh",
    planting_date = "2024-12",
    harvest_date = "2025-03",
    previous_crop = "rice",
    land_prep_method = "none",
    mulch_type = "straw",
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = NA,
    yield = r$FTY * 1000,
    yield_marketable = r$MTY * 1000,
    yield_part = "tubers",
    yield_moisture = 100 - r$DM,
    yield_isfresh = TRUE,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  geo <- data.frame(
    location = c("Fultola", "Moshamari", "Hogalbunia"),
    latitude = c(22.7088, 22.5978, 22.7558),
    longitude = c(89.5262, 89.4722, 89.5021),
    geo_from_source = TRUE 
    
  )
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}