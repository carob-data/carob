# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Soil file not used: 2 sampling depths per plot, and its treatment codes (T3 = BARI Alu 78 + conventional) contradict the yield file (T3 = BARI Alu 78 + zero tillage)
# Moshamari and Hogalburia not found in Khulna district (Google Maps, OpenStreetMap); coordinates left NA

carob_script <- function(path) {
  
  "
Dataset for: Potato production through zero-tillage with straw mulch during 2022-2023 season in agronomic trials in the Khulna district, Bangladesh (Soil analysis and yield data)

As part of the GIZ-PZTM project, agronomic trials were conducted under a strip-plot design in the locations of Fultola, Moshamari and Hogalbunia of Khulna district in Bangladesh. Two varieties of potato (BARI Alu 72 and BARI Alu 78) under zero tillage with straw mulch (ZTSM) and conventional treatments were tested. This dataset corresponds to the results of yield (marketable tuber yield and fresh tuber yield) and soil analysis (characterization, fertility, salinity and carbon organic) of the samples collected at the end season 2022-2023.
"
  
  uri <- "doi:10.21223/TJFYYD"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
    data_organization = "CIP",
    publication = NA,
    project = "GIZ-PZTM",
    design = "strip-plot",
    data_type = "on-farm experiment",
    treatment_vars = "variety;land_prep_method",
    response_vars = "yield;yield_marketable",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-11-03",
    carob_completion = 70,
    carob_effort = 1
  )
  
  f2 <- ff[basename(ff) == "02_Yield data.xlsx"]
  r <- carobiner::read.excel(f2)
  r$location[r$location == "Hogalbunia"] <- "Hogalburia"
  
  d <- data.frame(
    trial_id = paste("TJFYYD", r$location, sep = "_"),
    plot_id = as.character(r$plot_number),
    rep = as.integer(r$replication),
    variety = r$variety,
    land_prep_method = ifelse(r$management == "ZT", "none", "conventional"),
    mulch_type = ifelse(r$management == "ZT", "straw", NA),
    location = r$location,
    adm1 = "Khulna",
    country = "Bangladesh",
    planting_date = "2022",
    harvest_date = "2023",
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = NA,
    yield = r$FTY * 1000,
    yield_marketable = r$MTY * 1000,
    yield_part = "tubers",
    yield_moisture = as.numeric(NA),
    yield_isfresh = TRUE,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  
  geo <- data.frame(
    location = c("Fultola", "Moshamari", "Hogalburia"),
    latitude = c(22.9651, NA, NA),
    longitude = c(89.4778, NA, NA),
    geo_from_source = FALSE,
    geo_source = c("Google Maps", NA, NA)
  )
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}