# R script for "carob"
# license: GPL (>=3)

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
    response_vars = "yield_marketable",
    notes = NA,
    carob_contributor = "MARYAM YAHYA",
    carob_date = "2026-08-24",
    carob_completion = 80,
    carob_effort = 0.5
  )
  
  ## Source files
  f1 <- ff[basename(ff) == "PTYL200910_BARAKA.xls"]
  f2 <- ff[basename(ff) == "PTYL200910_KIBRCH.xls"]
  f3 <- ff[basename(ff) == "PTYL200910_KISIMA.xls"]
  f4 <- ff[basename(ff) == "PTYL200910_LIMURU.xls"]
  f5 <- ff[basename(ff) == "PTYL200910_NAROK.xls"]
  
  ## Read Fieldbook sheets with proper NA handling
  r1 <- carobiner::read.excel(f1, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  r2 <- carobiner::read.excel(f2, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  r3 <- carobiner::read.excel(f3, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  r4 <- carobiner::read.excel(f4, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  r5 <- carobiner::read.excel(f5, sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
  
  ## Read Minimal sheets for site information
  min1 <- carobiner::read.excel(f1, sheet = "Minimal")
  min2 <- carobiner::read.excel(f2, sheet = "Minimal")
  min3 <- carobiner::read.excel(f3, sheet = "Minimal")
  min4 <- carobiner::read.excel(f4, sheet = "Minimal")
  min5 <- carobiner::read.excel(f5, sheet = "Minimal")
  
  ## Add trial location to each Fieldbook
  r1$location <- "Baraka"
  r2$location <- "Kibirichia"
  r3$location <- "Kisima"
  r4$location <- "Limuru"
  r5$location <- "Narok"
  
  ## Combine the five trials
  r <- carobiner::bindr(r1, r2, r3, r4, r5)
  
  ## Extract geographic information from Minimal sheets
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
  
  ## Match location to coordinates
  loc_idx <- match(r$location, locations)
  
  ## Create final standardized data.frame
  d <- data.frame(
    trial_id = as.character(loc_idx),
    plot_id = as.character(r$PLOT),
    rep = as.integer(r$REP),
    variety = as.character(r$INSTN),
    location = r$location,
    country = "Kenya",
    crop = "potato",
    crop_rotation = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield_marketable = as.numeric(r$MTYA) * 1000,  # t/ha to kg/ha
    yield = NA_real_,
    yield_moisture = NA_real_,
    yield_isfresh = NA,
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
    # NEW: number of tubers planted per plot (plot area unknown, so per-plot value)
    tubers_planted_plot_ = as.numeric(r$NTP),
    # NEW: number of plants that emerged per plot
    plants_emerged_plot_ = as.numeric(r$NPE),
    # NEW: percentage of plants emerged
    pct_emergence_ = as.numeric(r$PPE),
    # NEW: number of plants harvested per plot
    plants_harvested_plot_ = as.numeric(r$NPH),
    # NEW: percentage of plants harvested
    pct_harvested_ = as.numeric(r$PPH),
    # NEW: number of marketable tubers per plot
    marketable_tubers_plot_ = as.numeric(r$NMTP),
    # NEW: number of marketable tubers per plant
    marketable_tubers_plant_ = as.numeric(r$NMTPL),
    # NEW: weight of marketable tubers per plot
    marketable_tuber_weight_plot_ = as.numeric(r$MTWP),
    # NEW: weight of marketable tubers per plant
    marketable_tuber_weight_plant_ = as.numeric(r$MTWPL)
  )
  
  ## Remove rows where all key variables are NA
  d <- d[!is.na(d$yield_marketable) | !is.na(d$variety), ]
  
  ## Write CAROB files
  carobiner::write_files(path, meta, d)
}