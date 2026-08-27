
carob_script <- function(path) {
  
  "
  Dataset for: Potato NPT Trials Amidst Prolonged Rains in Kenyan Regions - October 2009
  
  The Potato NPT Trials Amidst Prolonged Rains in Kenyan Regions took place
  in October 2009 in various locations in Kenya, including Baraka,
  Kibirichia, Kisima, Limuru and Narok. The study was conducted under a
  randomized complete block design (RCBD) with eight potato clones and
  four replications, aiming to assess the performance of these clones
  under prolonged rainfall.
  "
  
  uri <- "doi:10.21223/FBZ6JS"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(
    uri, path, group,
    major = 2,
    minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "randomized complete block design",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "MARYAM YAHYA",
    carob_date = "2026-08-27",
    carob_completion = 85,
    carob_effort = 0.5
  )
  
  # Source files
  f1 <- ff[basename(ff) == "PTYL200910_BARAKA.xls"]
  f2 <- ff[basename(ff) == "PTYL200910_KIBRCH.xls"]
  f3 <- ff[basename(ff) == "PTYL200910_KISIMA.xls"]
  f4 <- ff[basename(ff) == "PTYL200910_LIMURU.xls"]
  f5 <- ff[basename(ff) == "PTYL200910_NAROK.xls"]
  
  # Read Fieldbook sheets
  r1 <- carobiner::read.excel(f1, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  r2 <- carobiner::read.excel(f2, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  r3 <- carobiner::read.excel(f3, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  r4 <- carobiner::read.excel(f4, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  r5 <- carobiner::read.excel(f5, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  
  # Read Minimal sheets for site information
  min1 <- carobiner::read.excel(f1, sheet = "Minimal")
  min2 <- carobiner::read.excel(f2, sheet = "Minimal")
  min3 <- carobiner::read.excel(f3, sheet = "Minimal")
  min4 <- carobiner::read.excel(f4, sheet = "Minimal")
  min5 <- carobiner::read.excel(f5, sheet = "Minimal")
  
  # Add trial location
  r1$location <- "Baraka"
  r2$location <- "Kibirichia"
  r3$location <- "Kisima"
  r4$location <- "Limuru"
  r5$location <- "Narok"
  
  # Combine the five trials
  r <- carobiner::bindr(r1, r2, r3, r4, r5)
  
  # Extract coordinates from Minimal sheets
  get_minimal_value <- function(x, factor) {
    z <- x[x[["Factor"]] == factor, "Value"]
    if (length(z) == 0) return(NA_character_)
    as.character(z[1])
  }
  
  locations <- c("Baraka", "Kibirichia", "Kisima", "Limuru", "Narok")
  
  min_latitude <- c(
    get_minimal_value(min1, "Latitude"),
    get_minimal_value(min2, "Latitude"),
    get_minimal_value(min3, "Latitude"),
    get_minimal_value(min4, "Latitude"),
    get_minimal_value(min5, "Latitude")
  )
  
  min_longitude <- c(
    get_minimal_value(min1, "Longitude"),
    get_minimal_value(min2, "Longitude"),
    get_minimal_value(min3, "Longitude"),
    get_minimal_value(min4, "Longitude"),
    get_minimal_value(min5, "Longitude")
  )
  
  loc_idx <- match(r$location, locations)
  
  #  final data.frame
  # NOTE: Baraka does not have MTYA, so yield will be NA for Baraka
  d <- data.frame(
    trial_id = paste("FBZ6JS", gsub("[ ,]+", "_", r$location), sep = "_"),
    plot_id = as.character(r$PLOT),
    rep = as.integer(r$REP),
    variety = as.character(r$INSTN),
    location = r$location,
    country = "Kenya",
    crop = "potato",
    crop_rotation = NA,
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = ifelse("MTYA" %in% names(r), as.numeric(r$MTYA) * 1000, NA_real_),
    yield_moisture = NA_real_,
    yield_isfresh = TRUE,
    latitude = as.numeric(min_latitude[loc_idx]),
    longitude = as.numeric(min_longitude[loc_idx]),
    geo_from_source = TRUE,
    planting_date = "2009-10",
    harvest_date = "2010-02",
    N_fertilizer = NA_real_,
    P_fertilizer = NA_real_,
    K_fertilizer = NA_real_,
    S_fertilizer = NA_real_,
    fertilizer_type = NA_character_,
    lime = NA_real_,
    # NEW: percentage of plants emerged (available for most sites, NA for Baraka)
    pct_emergence_ = ifelse("PPE" %in% names(r), as.numeric(r$PPE), NA_real_),
    # NEW: percentage of plants harvested (available for most sites, NA for Baraka)
    pct_harvested_ = ifelse("PPH" %in% names(r), as.numeric(r$PPH), NA_real_),
    # NEW: average marketable tuber weight (g)
    avg_tuber_weight_ = as.numeric(r$ATMW)
  )
  
  # Remove empty rows
  d <- d[!is.na(d$yield) | !is.na(d$variety), ]
  
  # Write CAROB files
  carobiner::write_files(path, meta, d)
}