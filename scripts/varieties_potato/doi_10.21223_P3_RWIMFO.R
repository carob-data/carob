carob_script <- function(path) {
  "
  Dataset for: Participatory Varietal Selection of 16 late blight resistant clones (B1C5)
  in the localities of Yanamayo and Patacancha in Cusco, Peru

  16 potato clones from the B1C5 population with late blight resistance were evaluated
  in Yanamayo and Patacancha, Cusco, Peru during 2009-2010. The trial used a
  Randomized Complete Block Design (RCBD) with 3 replicates for Mother trials and
  3 replicates for Baby trials.
  "
  
  uri <- "doi:10.21223/P3/RWIMFO"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 4, minor = 0, 
          data_organization = "CIP", 
          publication = NA,
          project = NA, 
          design = "RCBD", 
          data_type = "experiment", 
          treatment_vars = "variety", 
          response_vars = "yield", 
          notes = NA, 
          carob_contributor = "Maryam Yahya", 
          carob_date = "2026-08-25", 
          carob_completion = 90, 
          carob_effort = 1.5
   )
  
  f1 <- ff[grepl("YANAM.*_processed", ff, ignore.case = TRUE)]
  f2 <- ff[grepl("PATACR.*_processed", ff, ignore.case = TRUE)]
  
  r1 <- carobiner::read.excel(f1, na = c("", "#N/D", "#DIV/0!", "NA"))
  r2 <- carobiner::read.excel(f2, na = c("", "#N/D", "#DIV/0!", "NA"))
  
  # Add location identifiers
  r1$location <- "Yanamayo"
  r2$location <- "Patacancha"
  
  # Combine the two trials
  r <- carobiner::bindr(r1, r2)
  
  d <- data.frame(
    trial_id = paste("RWIMFO_",  r$location),
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
    yield = r$yield_fresh * 1000,      # Total fresh yield (t/ha → kg/ha)
    yield_marketable = r$mtyna * 1000, # Marketable yield (t/ha → kg/ha)
    yield_moisture = NA_real_,
    yield_isfresh = TRUE,
    latitude = r$latitude,
    longitude = r$longitude,
    geo_from_source = TRUE,
    planting_date = as.character(as.Date(r$planting_date)),
    harvest_date = as.character(as.Date(r$harvest_date)),  
    N_fertilizer = r$n_fertilizer,
    P_fertilizer = r$p_fertilizer,
    K_fertilizer = r$k_fertilizer,
    soil_texture = r$soil_texture,
    elevation = r$elevation,
    treatment = r$field
  )
  
  carobiner::write_files(path, meta, d)
}
