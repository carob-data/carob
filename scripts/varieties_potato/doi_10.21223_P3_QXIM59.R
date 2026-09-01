carob_script <- function(path) {
  
  "
  Dataset for: Participatory Varietal Selection of 5 late blight resistant clones (B1C5) in the locality of San Juan in La Libertad, Peru

  5 potato clones from the B1C5 population with late blight resistance were evaluated
  in San Juan, La Libertad, Peru during 2010 and 2013. The trial used a
  Randomized Complete Block Design (RCBD) with 3 replicates for Mother trials.
  "
  
  uri <- "doi:10.21223/P3/QXIM59"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 4, minor = 0, data_organization = "CIP", publication = NA, project = NA, design = "randomized complete block design", data_type = "experiment", treatment_vars = "variety", response_vars = "yield", notes = NA, carob_contributor = "MARYAM YAHYA", carob_date = "2026-08-31", carob_completion = 90, carob_effort = 1.5)
  
  ## Source files (processed data only)
  f3 <- ff[grepl("201210.*_processed", ff, ignore.case = TRUE)]
  f5 <- ff[grepl("201311.*_processed", ff, ignore.case = TRUE)]
  
  ## Read source data
  r3 <- carobiner::read.excel(f3, na = c("", "#N/D", "#DIV/0!", "NA"))
  r5 <- carobiner::read.excel(f5, na = c("", "#N/D", "#DIV/0!", "NA"))
  
  ## Add location and year
  r3$location <- "San Juan"
  r5$location <- "San Juan"
  r3$year <- "2010"
  r5$year <- "2013"
  
  ## Combine
  r <- carobiner::bindr(r3, r5)
  
  ## Convert dates to character (YYYY-MM-DD)
  r$planting_date <- as.character(as.Date(r$planting_date))
  r$harvest_date <- as.character(as.Date(r$harvest_date))
  
  ## Final data.frame
  d <- data.frame(
    trial_id = paste("QXIM59", r$year, sep = "_"),
    plot_id = as.character(r$plot),
    rep = as.integer(r$rep),
    variety = r$variety,
    location = r$location,
    country = "Peru",
    crop = "potato",
    crop_rotation = NA,
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = as.logical(r$irrigated),
    yield_part = "tubers",
    yield = as.numeric(r$yield_fresh) * 1000,
    marketable_yield = as.numeric(r$mtyna) * 1000,
    yield_moisture = NA_real_,
    yield_isfresh = TRUE,
    latitude = as.numeric(r$latitude),
    longitude = as.numeric(r$longitude),
    geo_from_source = TRUE,
    planting_date = r$planting_date,
    harvest_date = r$harvest_date,
    N_fertilizer = as.numeric(r$n_fertilizer),
    P_fertilizer = as.numeric(r$p_fertilizer),
    K_fertilizer = as.numeric(r$k_fertilizer),
    fertilizer_type = NA_character_,
    lime = NA_real_,
    soil_texture = r$soil_texture,
    elevation = as.numeric(r$elevation),
    # NEW: field type (Mother trial)
    field_type_ = r$field
  )
  
  ## Clean
  d <- d[!is.na(d$yield) | !is.na(d$variety), ]
  
  ## Write
  carobiner::write_files(path, meta, d)
}