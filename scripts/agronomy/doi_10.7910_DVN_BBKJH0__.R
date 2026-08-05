# ============================================================
# CAROB SCRIPT
# Dataset: On-farm validation of RiceAdvice in West Africa
# Repository: Harvard Dataverse (doi:10.7910/DVN/BBKJH0)
# License: GPL (>=3)
# ============================================================

carob_script <- function(path) {
  library(tidyverse)
  # ==========================================================
  # Dataset information
  # ==========================================================
  
  uri <- "doi:10.7910/DVN/BBKJH0"
  group <- "agronomy"
  
  # ==========================================================
  # Creating repository folders
  # ==========================================================
  
  ff <- carobiner::get_data(uri, path, group)
  
  # ==========================================================
  # Metadata
  # ==========================================================
  
  meta <- carobiner::get_metadata(
    uri = uri,
    path = path,
    group = group,
    major = 3,
    minor = 0,
    data_organization = "AfricaRice",
    project = "RiceAdvice Lite",
    publication = NA,
    data_type = "on-farm experiment",
    design = "on-farm trial",
    treatment_vars = "treatment;variety;planting_method;N_fertilizer;P_fertilizer;K_fertilizer",
    response_vars = "yield",
    carob_completion = 100,
    carob_effort = 12,
    carob_contributor = "Kora Simperegui",
    carob_date = "2026-08-03",
    notes =
      paste(
        "RiceAdvice Lite dataset containing",
        "field-level agronomic management,",
        "fertilizer application, yield,",
        "nutrient-use efficiency and economic data.",
        "Trial identifiers reconstructed using",
        "country, administrative unit, year,",
        "season and activity type."
      )
  )
  
  # ==========================================================
  # Read raw data
  # ==========================================================
  
  f <- ff[basename(ff) == "data.xls"]
  
  r <- carobiner::read.excel(f)
  
  # ==========================================================
  # Standardize column names
  # ==========================================================
  
  names(r) <- names(r) |>
    trimws() |>
    gsub("%", "pct", x = _) |>
    gsub(" ", "_", x = _)
  
  # ==============================================================
  # Harmonize variable modalities accordind to valid terminologies
  # ==============================================================
  
  r <- r %>%
    mutate(
      # Fertilizer type harmonization
      type_npk_fertilizer_used = case_when(
        type_npk_fertilizer_used %in% c(
          "NPK 15 15 15",
          "NPK 15:15:15",
          "NPK 15-15-15",
          "NPK 20:10:10",
          "NPK 20-10-10",
          "NPK 12:22:22",
          "NPK 12-22-22",
          "NPK 23-10-05"
        ) ~ "NPK",
        type_npk_fertilizer_used %in% c(
          "No NPK applied",
          "No fertilizer applied"
        ) ~ "none",
        TRUE ~ type_npk_fertilizer_used
      ),
      
      # Planting method harmonization
      planting_method = recode(planting_method, "Direct" = "Direct seeding"),
      
      # Season harmonization
      season =
        recode(season,
          "wet season" = "wet",
          "dry season" = "dry"),
      
      # Country name
      country_name = recode(country_name, "Cote d'Ivoire" = "Côte d'Ivoire"),
      
      # Fertilizer type
      HHID = recode(HHID, "n/a" = ""),
      
      # Fertilizer type
      number_weeding = recode(number_weeding, "n/a" = ""),
      
      # Fertilizer type
      longitude = recode(longitude, "n/a" = ""),
      
      # Fertilizer type
      latitude = recode(latitude, "n/a" = ""),
      
      # Fertilizer type
      total_fertilizer_cost_usd_ha =
        recode(total_fertilizer_cost_usd_ha, "n/a" = ""),
      
      # Land preparation intensity
      number_tillage = case_when(
        number_tillage == 0 ~ "None",
        number_tillage == "n/a" ~ "Unknown",
        number_tillage %in% c(1,2,3) ~ "Tillage",
        TRUE ~ as.character(number_tillage)),
      
      # Herbicide use indicator
      land_preparation_clearing = case_when(
        land_preparation_clearing == "herbicide" ~ TRUE,
        land_preparation_clearing %in% c("manual", "n/a") ~ FALSE,
        TRUE ~ NA)
    )
  
  
  # ==========================================================
  # Create CAROB standardized table
  # ==========================================================
  
  d <- data.frame(
    country = carobiner::fix_name(r$country_name, case = "title"),
    crop = "rice",
    location = carobiner::fix_name(r$first_level_administrative_unit, case = "title"),
    site = carobiner::fix_name(r$first_level_administrative_unit, case = "title"),
    adm1 = carobiner::fix_name(r$first_level_administrative_unit, case = "title"),
    season = carobiner::fix_name(r$season, case = "lower"),
    variety = carobiner::fix_name(r$variety_used, case = ""),
    planting_method =carobiner::fix_name(r$planting_method,case = "lower"),
    irrigated = grepl("irrigated", r$production_system, ignore.case = TRUE),
    treatment = carobiner::fix_name(r$experimental_treatment_name, case = ""),
    rep = as.integer(r$replicate),
    stringsAsFactors = FALSE
  )
  
  # ==========================================================
  # Trial identifier
  # ==========================================================
  
  d$trial_id <- as.character(
    interaction(
      r$country_name,
      r$first_level_administrative_unit,
      r$year,
      r$season,
      r$rep,
      drop = TRUE
    )
  )
  
  # ==========================================================
  # Geographic information
  # ==========================================================
  
  d$longitude <- as.numeric(gsub(",", ".", r$longitude))
  d$latitude <- as.numeric(gsub(",", ".", r$latitude))
  d$geo_from_source <- TRUE
  
  # ==========================================================
  # Experimental information
  # ==========================================================
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  
  d$planting_date <- as.Date(as.numeric(r$sowing_date), origin = "1899-12-30")
  d$transplanting_date <- as.Date(as.numeric(r$transplanting_date), origin = "1899-12-30")
  d$harvest_date <- as.Date(as.numeric(r$harvest_date), origin = "1899-12-30")
  
  d$planting_date <- as.character(d$planting_date)
  d$transplanting_date <- as.character(d$transplanting_date)
  d$harvest_date <- as.character(d$harvest_date)
  # ==========================================================
  # Fertilizer information
  # ==========================================================
  
  d$N_fertilizer <- as.numeric(r$n_applied_kg_ha)
  d$P_fertilizer <- as.numeric(r$p_applied_kg_ha)
  d$K_fertilizer <- as.numeric(r$k_applied_kg_ha)
  
  d$fertilizer_type <- r$type_npk_fertilizer_used
  
  d$fertilizer_price <- as.numeric(r$total_fertilizer_cost_usd_ha)
  
  # ==========================================================
  # Management information
  # ==========================================================
  
  d$herbicide_used <- r$land_preparation_clearing
  
  d$land_prep_method <- carobiner::fix_name(r$number_tillage, case = "lower")
  
  d$weeding_times <- as.integer(r$number_weeding)
  
  # ==========================================================
  # Yield information
  # ==========================================================
  
  d$yield <- as.numeric(r$yield_at_14pct_moisture_content_kg_ha)
  d$yield_part <- "grain"
  d$yield_moisture <- 14
  
  # ==========================================================
  # Household identifier
  # ==========================================================
  
  d$hhid <- carobiner::fix_name(r$HHID, case = "")
  
  # ==========================================================
  # Export
  # ==========================================================

  carobiner::write_files(path, meta, d)
}

# ============================================================
# Execute script
# ============================================================

path <- getwd()
carob_script(path)