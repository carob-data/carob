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
    response_vars = "fries_color;fries_texture",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-03",
    carob_completion = 85,
    carob_effort = 1.5
  )
  
  ## Source file
  f1 <- ff[basename(ff) == "01_Potato French Fries Processing Results 2020-2021.xlsx"]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1, na = c("", "#N/D", "#DIV/0!", "NA", "-"))
  
  ## Aggregate over evaluators (average of color and texture)
  ## Each combination of Clone × Repetition × Locality has 3 evaluators
  r1 <- aggregate(r1[, c("Color sample1", "Color sample2", "Texture sample1", "Texture sample2")],
                  r1[, c("Clone", "Repetition", "Locality")],
                  mean, na.rm = TRUE)
  
  ## Rename columns for consistency
  names(r1) <- c("Clone", "Repetition", "Locality", "Color1", "Color2", "Texture1", "Texture2")
  
  ## Coordinates estimated from Google Maps (September 2026)
  coords <- data.frame(
    location = c("Majes", "Huancayo", "Chugay", "Yanac", "Chota", "Huanuco"),
    latitude = c(-16.3625, -12.0651, -7.78167, -7.8000, -6.5636, -9.9306),
    longitude = c(-72.1911, -75.2049, -77.8683, -77.8000, -78.6500, -76.2422)
  )
  
  ## Management variables set to NA (not in source)
  d <- data.frame(
    trial_id = "ZTPO9T",
    plot_id = paste(r1$Locality, r1$Repetition, r1$Clone, sep = "_"),
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
    # NEW: average french fries quality scores (1-5)
    fries_color = rowMeans(r1[, c("Color1", "Color2")], na.rm = TRUE),
    fries_texture = rowMeans(r1[, c("Texture1", "Texture2")], na.rm = TRUE)
  )
  
  ## Convert NaN from rowMeans to NA
  d$fries_color[is.nan(d$fries_color)] <- NA
  d$fries_texture[is.nan(d$fries_texture)] <- NA
  
  ## Merge coordinates
  d <- merge(d, coords, by = "location", all.x = TRUE)
  d$geo_from_source <- FALSE
  
  ## Remove rows with missing key variables
  d <- d[!is.na(d$variety), ]
  
  ## Write CAROB files
  carobiner::write_files(path, meta, d)
}