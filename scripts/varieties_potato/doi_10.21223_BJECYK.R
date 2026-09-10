carob_script <- function(path) {
  
  "
Dataset for: Late blight resistant potato varieties for the tropical highlands and mid-elevation

With the objective of selecting potato clones with high resistance to late blight and good quality for frying in French fires, 6 experiments were planted with 8 advanced clones from population B groups B3C1 and B3C2, in contrasting localities in Peru. The clones CIP395123.6, CIP396026.1 and CIP396034.103, were selected for their high tuber yield, good quality for french fries, high content of dry matter, low content of reducing sugars and adapts to the various localities of Peru, these clones will be released as new varieties in 2022 in Peru.
"
  
  uri <- "doi:10.21223/BJECYK"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 3,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "fries_color;tuber_flavor;tuber_texture",
    notes =NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-09",
    carob_completion = 80,
    carob_effort = 3.5
  )
  
  ## Source files
  f1 <- ff[basename(ff) == "01_MAJ21_01.xlsx"]
  f2 <- ff[basename(ff) == "02_french_fries.xlsx"]
  f3 <- ff[basename(ff) == "04_HYO21_03.xlsx"]
  f4 <- ff[basename(ff) == "05_HCHO21_02(LICAME).xlsx"]
  f5 <- ff[basename(ff) == "06_HCHO21_03(YANAC).xlsx"]
  f6 <- ff[basename(ff) == "07_CAJ21_01.xlsx"]
  f7 <- ff[basename(ff) == "08_HCO21_03.xlsx"]
  
  ## Read data
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  r4 <- carobiner::read.excel(f4)
  r5 <- carobiner::read.excel(f5)
  r6 <- carobiner::read.excel(f6)
  r7 <- carobiner::read.excel(f7)
  
  ## Add location
  r1$location <- "Majes"
  r3$location <- "Huancayo"
  r4$location <- "Licame"
  r5$location <- "Yanac"
  r6$location <- "Cajamarca"
  r7$location <- "Huanuco"
  
  ## Combine plot-level data
  r <- carobiner::bindr(r1, r3, r4, r5, r6, r7)
  ## Create lookup for fries color from r2
  fries_lookup <- r2[, c("Locality", "Clone", "French_fries_Mean")]
  names(fries_lookup) <- c("location", "variety", "fries_color")
  
  ## Fix location names
  fries_lookup$location[fries_lookup$location == "Chota"] <- "Cajamarca"
  fries_lookup$location[fries_lookup$location == "Chugay"] <- "Licame"
  ## Coordinates
 geo <- data.frame(
    location = c("Majes", "Huancayo", "Licame", "Yanac", "Cajamarca", "Huanuco"),
    latitude = c(-16.3625, -12.0651, -7.8133, -7.8133, -7.1638, -9.9306),
    longitude = c(-72.1911, -75.2048, -78.0483, -78.0483, -78.5000, -76.2422),
    geo_from_source = FALSE
  )
  
  d <- data.frame(
    trial_id = paste0("BJECYK_", r$location),
    plot_id = as.character(r$Plot),
    rep = as.integer(gsub("R", "", r$Rep)),
    variety = r$Clone,
    location = r$location,
    country = "Peru",
    crop = "potato",
    crop_rotation = NA,
    on_farm = NA,
    is_survey = FALSE,
    irrigated = NA,
    planting_date = "2021",
    harvest_date = "2021",
    yield = NA_real_,
    yield_moisture = NA,
    yield_isfresh= NA,
    yield_part = "tubers",
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    tuber_flavor = rowMeans(cbind(
      as.numeric(r$Evaluator1_Flavor),
      as.numeric(r$Evaluator2_Flavor),
      as.numeric(r$Evaluator3_Flavor)
    ), na.rm = TRUE),
    tuber_texture = rowMeans(cbind(
      as.numeric(r$Evaluator1_Texture),
      as.numeric(r$Evaluator2_Texture),
      as.numeric(r$Evaluator3_Texture)
    ), na.rm = TRUE)
  )
  
  ## Merge fries color
  d <- merge(d, fries_lookup, by = c("location", "variety"), all.x = TRUE)
  
  
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  ## Remove rows with missing data
  d <- d[!is.na(d$variety), ]
  
  carobiner::write_files(path, meta, d)
}