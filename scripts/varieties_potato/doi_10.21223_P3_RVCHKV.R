carob_script <- function(path) {
  
  "
  Dataset for: Participatory Varietal Selection of 7 with frost tolerant clones in the localities of San Juan Bajo, La Soledad and Macullida in La Libertad, Peru

  The Mother and Baby (M&B) trial methodology was adapted by CIP for Participatory Variety Selection (PVS) through decentralized evaluation networks and multi-year evaluations in potato growing areas in the Andean region. The M&B trial design encourages active participation of farmers through the application of treatments through systematic evaluations and selections of treatments in their own plots called 'Baby trials' (i.e. farmer managed trials) and in fields with an experimental design called 'Mother trials' (i.e. researcher managed trials). Objective: Analyze characteristics, attributes and preferences that men and women have when selecting a new potato variety at the phase of flowering and harvesting. A M&B trial was performed to evaluate 11 clones with frost tolerance in the locality of San Juan Bajo and 7 clones in the localities of La Soledad and Macullida, in the province of Sanchez Carrion, in La Libertad department, Peru. The trial design was a Randomized Complete Block Design (RCBD) with 3 replicates for Mother trials and 3 replicates for Baby trials during 2012-2013. In this experiment, characteristics of plant (size, type of foliage), yield, desirable quality, pest and disease resistance during flowering and harvesting were evaluated. Total number of participants at flowering phase was 36 (men=28 and women=8) at the locality of San Juan Bajo, 9 (men=5, women = 4) at the locality of La Soledad, and 9 (men=4, women = 5) in the locality of Macullida. Total number of participants at harvesting phase was 33 (men=20 and women=13) at the locality of San Juan Bajo, 9 (men=5, women = 4) at the locality of La Soledad, and 9 (men=4, women = 5) in the locality of Macullida. Additionally, an organoleptic evaluation was assessed at harvesting to evaluate appearance, taste and texture. The number of panelists was 10 (men=5, women=5) for the organoleptic evaluation.
  "
  
  uri <- "doi:10.21223/P3/RVCHKV"
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
    carob_date = "2026-09-04",
    carob_completion = 90,
    carob_effort = 1.5
  )
  
  ## Source files (processed data only)
  f1 <- ff[grepl("SJUAN.*_processed", ff, ignore.case = TRUE)]
  f2 <- ff[grepl("LSOLE.*_processed", ff, ignore.case = TRUE)]
  f3 <- ff[grepl("MACULL.*_processed", ff, ignore.case = TRUE)]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  
  ## yield adjustment seems of for some cases in r1
  r1$yield_fresh <- pmin(r1$yield_fresh, r1$ttyna * 1.1)
  
  ## Add location identifiers
  r1$location <- "San Juan Bajo"
  r2$location <- "La Soledad"
  r3$location <- "Macullida"
  
  ## Combine the three trials
  r <- carobiner::bindr(r1, r2, r3)
  
  d <- data.frame(
    trial_id = paste0("RVCHKV_", r$location),
    plot_id = as.character(r$plot),
    rep = as.integer(r$rep),
    variety = r$variety,
    location = r$location,
    country = "Peru",
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = r$irrigated,
    yield_part = "tubers",
    yield = r$yield_fresh * 1000,
    yield_marketable = r$mtyna * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    latitude = r$latitude,
    longitude = r$longitude,
    geo_from_source = TRUE,
    planting_date = as.character(r$planting_date),
    harvest_date = as.character(r$harvest_date),
    N_fertilizer = r$n_fertilizer,
    P_fertilizer = r$p_fertilizer,
    K_fertilizer = r$k_fertilizer,
    soil_texture = r$soil_texture,
    elevation = r$elevation,
    # (Mother trial vs Baby trial)
    treatment = r$field
  )

  d <- d[!is.na(d$yield), ]
  carobiner::write_files(path, meta, d)
}
