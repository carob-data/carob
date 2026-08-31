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
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1,  minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "randomized complete block design",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "Maryam Yahya",
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
  
  d <- data.frame(
    trial_id = paste0("MXKUIK_", r1$Locality, "_", r1$Year),
    plot_id = as.character(r1$Plot),
    rep = as.integer(r1$Rep),
    variety = r1$Clone,
    location = r1$Locality,
    country = "Peru",
    crop = "potato",
    crop_rotation = NA,
    on_farm = TRUE,
    is_survey = FALSE,
    yield_part = "tubers",
    yield = r1$TTYA * 1000,           
    yield_marketable = r1$MTYA * 1000,
    yield_moisture = 100 - r1$`DM_Oven_drying_ method`, # Moisture (%) = 100 - dry matter (%)
    yield_isfresh = TRUE,
    planting_date = "2019",
    harvest_date = "2020",
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    irrigated = NA,
    # NEW: reducing sugars (%)
    tuber_reducing_sugars = r1$`Reducing_sugars_%`,
    # NEW: French fry color
    fries_color = rowMeans(r1[, c(`French_Fry_ color_At_Harvest`, `French_Fry_ color_ 90_days_after_harvest`)], na.rm=TRUE),
    # NEW: French fry color after blanching (USDA scale 1-5)
    fries_blanching = r1$`French_Fry_ color_Blanching`,
    # NEW: average baked flavor score (1-5, 5=Excellent, 3=Good, 1=Bad)
    tuber_flavor = rowMeans(r1[, grep("^Baked Flavor", names(r1))], na.rm = TRUE),
    # NEW: average baked texture score (5=Floury, 3=Intermediate, 1=Watery)
    tuber_texture = rowMeans(r1[, grep("^Baked_Texture", names(r1))], na.rm=TRUE)
  )
  
  # Convert NaN from rowMeans to NA
  d$tuber_flavor[is.nan(d$tuber_flavor)] <- NA
  d$tuber_texture[is.nan(d$tuber_texture)] <- NA
  
  # Merge coordinates
  d <- merge(d, coords, by.x = "location", by.y = "Locality", all.x = TRUE)
  d$geo_from_source <- FALSE
  
  # Remove rows where all key variables are NA
  #d <- d[!is.na(d$yield), ]
  d$yield_moisture[d$yield_moisture == 100] <- NA
  carobiner::write_files(path, meta, d)
}
