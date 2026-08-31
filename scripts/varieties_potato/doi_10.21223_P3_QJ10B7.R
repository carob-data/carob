
carob_script <- function(path) {
  
  "
  Dataset for: Participatory Varietal Selection of 4 frost tolerant clones in the localities
  of Aeropuerto, Allato, Chacapunco and Occo Centro in Huancavelica, Peru

  4 potato clones with frost tolerance were evaluated in Aeropuerto, Allato, Chacapunco
  and Occo Centro, Huancavelica, Peru during 2012-2013. The trial used a
  Randomized Complete Block Design (RCBD) with 3 replicates for Mother trials.
  "
  
  uri <- "doi:10.21223/P3/QJ10B7"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(
    uri, path, group,
    major = 4,
    minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "randomized complete block design",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "MARYAM YAHYA",
    carob_date = "2026-08-25",
    carob_completion = 90,
    carob_effort = 2.0
  )
  
  ## Source files (processed data only)
  f1 <- ff[grepl("AEROPR.*_processed", ff, ignore.case = TRUE)]
  f2 <- ff[grepl("OCCO.*_processed", ff, ignore.case = TRUE)]
  f3 <- ff[grepl("ALLATO.*_processed", ff, ignore.case = TRUE)]
  f4 <- ff[grepl("CHACAP.*_processed", ff, ignore.case = TRUE)]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1, na = c("", "#N/D", "#DIV/0!", "NA"))
  r2 <- carobiner::read.excel(f2, na = c("", "#N/D", "#DIV/0!", "NA"))
  r3 <- carobiner::read.excel(f3, na = c("", "#N/D", "#DIV/0!", "NA"))
  r4 <- carobiner::read.excel(f4, na = c("", "#N/D", "#DIV/0!", "NA"))
  
  ## Add location identifiers
  r1$location <- "Aeropuerto"
  r2$location <- "Occo Centro"
  r3$location <- "Allato"
  r4$location <- "Chacapunco"
  
  ## Combine the four trials
  r <- carobiner::bindr(r1, r2, r3, r4)
  
  ## Create final standardized data.frame
  d <- data.frame(
    trial_id = paste("QJ10B7", gsub("[ ,]+", "_", r$location), sep = "_"),
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
    yield = as.numeric(r$yield_fresh) * 1000,      # Total fresh yield (t/ha → kg/ha)
    marketable_yield = as.numeric(r$mtyna) * 1000, # Marketable yield (t/ha → kg/ha)
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
    # NEW: field type (Mother trial vs Baby trial)
    field_type_ = as.character(r$field)
  )
  
  ## Remove completely empty rows
  d <- d[!is.na(d$yield) | !is.na(d$variety), ]
  
  ## Write CAROB files
  carobiner::write_files(path, meta, d)
}