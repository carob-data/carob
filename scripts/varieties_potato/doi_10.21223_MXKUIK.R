
carob_script <- function(path) {
  
  "
  Dataset for: New Resistant Potato Varieties to Late Blight and High Quality for French Fries generated in Peru

  Ten potato clones from the B3C1 and B3C2 populations and two Peruvian varieties
  (UNICA and Canchan) were evaluated in 13 experiments in farmers' fields in Peru
  during 2019-2020 and 2020-2021, using a randomized complete block design with
  three replications.
  "
  
  uri <- "doi:10.21223/MXKUIK"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(
    uri, path, group,
    major = 1,
    minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "randomized complete block design",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "MARYAM YAHYA",
    carob_date = "2026-08-27",
    carob_completion = 90,
    carob_effort = 1.5
                                  
  )
  
  # Source file
  f1 <- ff[basename(ff) == "01_Data_Adaptation_and_Efficiency_Trials.xlsx"]
  
  # Read source data
  r1 <- carobiner::read.excel(f1, na = c("", "#N/D", "#DIV/0!", "NA"))
  
  # Estimated from Google Maps
  coords <- data.frame(
    Locality = c("Chinchao", "Chugay", "La Paccha", "Majes", "Majes1", "Majes2",
                 "Quilcas", "Santa Rita", "Jauja", "Yanac"),
    latitude = c(-9.63333, -7.78167, -6.51181, -16.36250, -16.36250, -16.36250,
                 -11.93749, -12.00000, -11.77500, -7.80000),
    longitude = c(-76.0667, -77.8683, -78.92879, -72.19111, -72.19111, -72.19111,
                  -75.2593, -75.50000, -75.50000, -77.80000)
  )
  
  # Final data.frame
  d <- data.frame(
    trial_id = paste("MXKUIK", gsub("[ ,]+", "_", as.character(r1$Locality)), as.character(r1$Year), sep = "_"),
    plot_id = as.character(r1$Plot),
    rep = as.integer(r1$Rep),
    variety = r1$Clone,
    location = r1$Locality,
    country = "Peru",
    crop = "potato",
    crop_rotation = NA,
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = as.numeric(r1$TTYA) * 1000,           # Total yield (adjusted, t/ha → kg/ha)
    marketable_yield = as.numeric(r1$MTYA) * 1000, # Marketable yield (adjusted, t/ha → kg/ha)
    yield_moisture = 100 - as.numeric(r1$`DM_Oven_drying_ method`), # Moisture (%) = 100 - dry matter (%)
    yield_isfresh = TRUE,
    planting_date = "2019",
    harvest_date = "2020",
    N_fertilizer = NA_real_,
    P_fertilizer = NA_real_,
    K_fertilizer = NA_real_,
    S_fertilizer = NA_real_,
    fertilizer_type = NA_character_,
    lime = NA_real_,
    # NEW: reducing sugars (%)
    reducing_sugars_ = as.numeric(r1$`Reducing_sugars_%`),
    # NEW: French fry color at harvest (USDA scale 1-5)
    ffr_color_harvest_ = as.numeric(r1$`French_Fry_ color_At_Harvest`),
    # NEW: French fry color after blanching (USDA scale 1-5)
    ffr_color_blanching_ = as.numeric(r1$`French_Fry_ color_Blanching`),
    # NEW: French fry color after 90 days storage (USDA scale 1-5)
    ffr_color_90d_ = as.numeric(r1$`French_Fry_ color_ 90_days_after_harvest`),
    # NEW: average baked flavor score (1-5, 5=Excellent, 3=Good, 1=Bad)
    flavor_baked_ = rowMeans(cbind(
      as.numeric(r1$`Baked Flavor 1`),
      as.numeric(r1$`Baked Flavor 2`),
      as.numeric(r1$`Baked Flavor 3`)
    ), na.rm = TRUE),
    # NEW: average baked texture score (5=Floury, 3=Intermediate, 1=Watery)
    texture_baked_ = rowMeans(cbind(
      as.numeric(r1$Baked_Texture1),
      as.numeric(r1$Baked_Texture2),
      as.numeric(r1$Baked_Texture3)
    ), na.rm = TRUE)
  )
  
  # Convert NaN from rowMeans to NA
  d$flavor_baked_[is.nan(d$flavor_baked_)] <- NA
  d$texture_baked_[is.nan(d$texture_baked_)] <- NA
  
  # Merge coordinates
  d <- merge(d, coords, by.x = "location", by.y = "Locality", all.x = TRUE)
  d$geo_from_source <- FALSE
  
  # Remove rows where all key variables are NA
  d <- d[!is.na(d$yield) | !is.na(d$variety), ]
  
  # Write CAROB files
  carobiner::write_files(path, meta, d)
}