carob_script <- function(path) {
  
  "
  Dataset for: Participatory Varietal Selection of 5 late blight resistant clones (B1C5) in the locality of San Juan in La Libertad, Peru

  The Mother and Baby (M&B) trial methodology was adapted by CIP for Participatory Variety Selection (PVS) through decentralized evaluation networks and multi-year evaluations in potato growing areas in the Andean region. The M&B trial design encourages active participation of farmers through the application of treatments through systematic evaluations and selections of treatments in their own plots called 'Baby trials' (i.e. farmer managed trials) and in fields with an experimental design called 'Mother trials' (i.e. researcher managed trials). Objective: Analyze characteristics, attributes and preferences that men and women have when selecting a new potato variety at the phase of flowering and harvesting. A M&B trial was performed to evaluate 5 clones of the population B1C5 with late blight resistance at the locality of San Juan Bajo, in Sanchez Carrion province, in La Libertad department, Peru. The trial design was a Randomized Complete Block Design (RCBD) with 3 replicates for Mother trials and 3 replicates for Baby trials during 2012-2014. In this experiment characteristics of plant (size, type of foliage), yield, desirable quality, pest and disease resistance during flowering and harvesting were evaluated. Total number of participants at flowering phase was 36 (men=28 and women=8) and 81 (men=14, women = 67); and the number of participants at harvesting phase was 33 (men=20 and women=13), 22 (men=12, women = 10) in both experiments respectively. Additionally, an organoleptic evaluation was assessed at harvesting to evaluate appearance, taste and texture. The number of panelists was 10 (men=5, women=5) for the organoleptic evaluation.
  "
  
  uri <- "doi:10.21223/P3/QXIM59"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 4, minor = 0, data_organization = "CIP", publication = NA, project = NA, design = "randomized complete block design", data_type = "experiment", treatment_vars = "variety", response_vars = "yield", notes = NA, carob_contributor = "MARYAM YAHYA", carob_date = "2026-09-01", carob_completion = 90, carob_effort = 1.5)
  
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
  
  ## Values in r5 appear to be 10x too high, so divide by 10
  r5$yield_fresh <- r5$yield_fresh / 10
  r5$mtyna <- r5$mtyna / 10
  
  ## Combine
  r <- carobiner::bindr(r3, r5)
  
  ## Convert dates to character (YYYY-MM-DD)
  r$planting_date <- as.character(as.Date(r$planting_date))
  r$harvest_date <- as.character(as.Date(r$harvest_date))
  
  ## Create final standardized data.frame
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
    irrigated = r$irrigated,
    yield_part = "tubers",
    yield = as.numeric(r$yield_fresh) * 1000,      # Total fresh yield (t/ha → kg/ha)
    marketable_yield = as.numeric(r$mtyna) * 1000, # Marketable yield (t/ha → kg/ha)
    yield_moisture = NA_real_,
    yield_isfresh = TRUE,
    latitude = r$latitude,
    longitude = r$longitude,
    geo_from_source = TRUE,
    planting_date = r$planting_date,
    harvest_date = r$harvest_date,
    N_fertilizer = r$n_fertilizer,
    P_fertilizer = r$p_fertilizer,
    K_fertilizer = r$k_fertilizer,
    fertilizer_type = NA_character_,
    lime = NA_real_,
    soil_texture = r$soil_texture,
    elevation = r$elevation,
    # NEW: field type (Mother trial)
    field_type_ = r$field
  )
  
  ## Remove rows where all key variables are NA
  d <- d[!is.na(d$yield) | !is.na(d$variety), ]
  
  ## Write CAROB files
  carobiner::write_files(path, meta, d)
}