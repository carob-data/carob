
carob_script <- function(path) {
  
  "
  Dataset for: Assessment of french fries quality traits during 2020-2021

  In 2021, in the CIP physiology and post-harvest laboratory in La Molina, quality tests were carried out for frying in eighth potato clones with high levels of resistance to late blight and excellent quality for frying in french fries and two varieties Canchan and Unica, the samples came from 5 localities of Peru, where these clones were planted in adaptation and efficiency experiments for the registration of varieties within the CIP-PODEROSA project, under the tax research law, Law 30309, which encourages the Peruvian government to encourage agricultural research in the country. Tests to determine the dry matter, frying color, reducing sugar content, and frying with scalding were carried out.
  "
  
  uri <- "doi:10.21223/ZTPO9T"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 2,
                                  data_organization = "CIP",
                                  publication = NA,
                                  project = NA,
                                  design = NA,
                                  data_type = "experiment",
                                  treatment_vars = "variety",
                                  response_vars = "color_fries_1_;color_fries_2_;texture_fries_1_;texture_fries_2_",
                                  notes = NA,
                                  carob_contributor = "Maryam Yahya",
                                  carob_date = "2026-09-02",
                                  carob_completion = 85,
                                  carob_effort = 1.5
  )
  
  ## Source file
  f1 <- ff[basename(ff) == "01_Potato French Fries Processing Results 2020-2021.xlsx"]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1, na = c("", "#N/D", "#DIV/0!", "NA", "-"))
  
  ## Coordinates estimated from Google Maps (September 2026)
  coords <- data.frame(
    location = c("Majes", "Huancayo", "Chugay", "Yanac", "Chota", "Huanuco"),
    latitude = c(-16.3625, -12.0651, -7.78167, -7.8000, -6.5636, -9.9306),
    longitude = c(-72.1911, -75.2049, -77.8683, -77.8000, -78.6500, -76.2422)
  )
  
  ## Management variables set to NA (not in source)
  d <- data.frame(
    trial_id = "ZTPO9T",
    plot_id = as.character(r1$Numeration),
    rep = as.integer(r1$Repetition),
    variety = r1$Clone,
    location = r1$Locality,
    country = "Peru",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    N_fertilizer = NA_real_,
    P_fertilizer = NA_real_,
    K_fertilizer = NA_real_,
    planting_date = NA,
    harvest_date = NA,
    yield_part = "tubers",
    yield = NA_real_,
    yield_moisture = NA_real_,
    yield_isfresh = NA,
    # NEW: french fries quality scores (color and texture, 1-5)
    color_fries_1_ = as.integer(r1$`Color sample1`),
    color_fries_2_ = as.integer(r1$`Color sample2`),
    texture_fries_1_ = as.integer(r1$`Texture sample1`),
    texture_fries_2_ = as.integer(r1$`Texture sample2`)
  )
  
  ## Merge coordinates
  d <- merge(d, coords, by = "location", all.x = TRUE)
  d$geo_from_source <- FALSE
  
  ## Remove rows with missing key variables
  d <- d[!is.na(d$variety), ]
  
  ## Write CAROB files
  carobiner::write_files(path, meta, d)
}