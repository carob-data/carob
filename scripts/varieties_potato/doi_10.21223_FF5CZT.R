
carob_script <- function(path) {
  
  "
  Dataset for: Assessment of french fries quality traits during 2019-2020

  In 2020, in the CIP physiology and post-harvest laboratory in La Molina, quality tests were carried out for frying in ten potato clones with high levels of resistance to late blight and excellent quality for frying in french fries and two varieties Canchan and Unica, the samples came from 5 localities of Peru, where these clones were planted in adaptation and efficiency experiments for the registration of varieties within the CIP-PODEROSA project, under the tax research law, Law 30309, which encourages the Peruvian government to encourage agricultural research in the country. Tests to determine the dry matter, frying color, reducing sugar content, and frying with scalding were carried out.
  "
  
  uri <- "doi:10.21223/FF5CZT"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 3,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "fries_color;fries_texture",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-04",
    carob_completion = 85,
    carob_effort = 2.5
  )
  
  ## Source files
  f1 <- ff[basename(ff) == "01_Potato French Fries Processing Results Quilcas 2019-2020.xlsx"]
  f2 <- ff[basename(ff) == "02_Potato French Fries Processing Results Majes 2019-2020.xlsx"]
  f3 <- ff[basename(ff) == "03_Potato French Fries Processing Results Huanuco 2019-2020.xlsx"]
  f4 <- ff[basename(ff) == "04_Potato French Fries Processing Results Huamachuco 2019-2020.xlsx"]
  f5 <- ff[basename(ff) == "05_Potato French Fries Processing Results Chota 2019-2020.xlsx"]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1, na = c("", "#N/D", "#DIV/0!", "NA", "-", "*", "**", "***"))
  r2 <- carobiner::read.excel(f2, na = c("", "#N/D", "#DIV/0!", "NA", "-", "*", "**", "***"))
  r3 <- carobiner::read.excel(f3, na = c("", "#N/D", "#DIV/0!", "NA", "-", "*", "**", "***"))
  r4 <- carobiner::read.excel(f4, na = c("", "#N/D", "#DIV/0!", "NA", "-", "*", "**", "***"))
  r5 <- carobiner::read.excel(f5, na = c("", "#N/D", "#DIV/0!", "NA", "-", "*", "**", "***"))
  
  ## Add location
  r1$location <- "Quilcas"
  r2$location <- "Majes"
  r3$location <- "Huanuco"
  r4$location <- "Huamachuco"
  r5$location <- "Chota"
  
  ## Combine
  r <- carobiner::bindr(r1, r2, r3, r4, r5)
  
  ## Convert to numeric (handling NAs)
  r$Color1 <- as.numeric(r$`Color sample1`)
  r$Color2 <- as.numeric(r$`Color sample2`)
  r$Texture1 <- as.numeric(r$`Texture sample1`)
  r$Texture2 <- as.numeric(r$`Texture sample2`)
  
  ## Coordinates estimated from Google Maps (September 2026)
  geo <- data.frame(
    location = c("Quilcas", "Majes", "Huanuco", "Huamachuco", "Chota"),
    latitude = c(-11.9375, -16.3625, -9.9306, -7.8133, -6.5636),
    longitude = c(-75.2593, -72.1911, -76.2422, -77.7733, -78.6500),
    geo_source = "Google Maps",
    geo_from_source = FALSE
  )
  
  ## Management variables set to NA (not in source)
  d <- data.frame(
    trial_id = paste("FF5CZT", r$location, sep = "_"),
    plot_id = as.character(r$`#`),
    rep = as.integer(r$Repetition),
    variety = r$Clone,
    location = r$location,
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
    # NEW: average french fries quality scores (1-5)
    fries_color = rowMeans(r[, c("Color1", "Color2")], na.rm = TRUE),
    fries_texture = rowMeans(r[, c("Texture1", "Texture2")], na.rm = TRUE)
  )
  
  ## Merge coordinates
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  ## Write CAROB files
  carobiner::write_files(path, meta, d)
}