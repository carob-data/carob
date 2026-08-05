# ============================================================
# CAROB SCRIPT
# Dataset: Current rice management practices in Nigeria, West Africa
# Repository: Harvard Dataverse (doi.org/10.7910/DVN/NDL2T3)
# License: GPL (>=3)
# ============================================================

carob_script <- function(path) {
  require(tidyverse)
  # ==========================================================
  # Dataset information
  # ==========================================================
  
  uri <- "doi.org/10.7910/DVN/NDL2T3"
  
  group <- "survey"
  
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
    major = 2,
    minor = 0,
    data_organization = "AfricaRice",
    project = "Sustainable Farming Program",
    publication = NA,
    data_type = "on-farm experiment",
    design = "on-farm trial",
    treatment_vars = "herbicide_used; weeding_done; insecticide_used; OM_used;
    variety; planting_method;N_fertilizer;P_fertilizer;K_fertilizer",
    response_vars = "yield",
    carob_completion = 100,
    carob_effort = 10, 
    carob_contributor = "Kora Simperegui",
    carob_date = "2026-07-30",
    notes =
      paste(
        "Field-level record of rice management practices currently
        used by farmers in Nigeria, covering the entire production
        cycle from land preparation to harvest. For each plot, it 
        documents land preparation and leveling, number and method of 
        tillage, variety and seed source, planting date and adherence 
        to optimum sowing windows, quantity of seed applied, and crop 
        establishment method."
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
  # Harmonize variable according to valid terminologies
  # ==============================================================
  
  r <- r %>%
    mutate(
      # Transplanting method
      planting_method = case_when(
        Crop_establishement_method == "transplanting" ~ "transplanting",
        Crop_establishement_method == "broadcasting" ~ "broadcasting",
        Crop_establishement_method == "line_seeding" ~ "line sowing",
        Crop_establishement_method == "dibbling" ~ "dibbling",
        TRUE ~ Crop_establishement_method),

      # Season harmonization
      Season =
        recode(Season,
          "Wet season" = "wet",
          "dry season" = "dry"),
  
      # Was weeding done
      weeding_done = case_when(
        Number_of_weeding == 0 ~ "No",
        Number_of_weeding > 0 ~ "Yes",
        TRUE ~ as.character(Number_of_weeding)),
  
      # Land preparation intensity
      number_tillage = case_when(
        Number_of_weeding == 0 ~ "None",
        Number_of_weeding == "n/a" ~ "Unknown",
        Number_of_weeding > 0 ~ "Tillage",
        TRUE ~ as.character(Number_of_weeding)),
      
      # Organic matter application
      Organic_input_use =
        recode(Organic_input_use,
               "no" = "No",
               "yes" = "Yes"),
    )
  
  # ==========================================================
  # Create CAROB standardized table
  # ==========================================================
  
  d <- data.frame(
    country = carobiner::fix_name(r$Country, case = "title"),
    crop = "rice",
    # location = carobiner::fix_name(r$Country, case = "title"),
    # site = carobiner::fix_name(r$first_level_administrative_unit, case = "title"),
    adm1 = carobiner::fix_name(r$State, case = "title"),
    season = carobiner::fix_name(r$Season, case = "lower"),
    variety = carobiner::fix_name(r$Variety_used, case = ""),
    planting_method =carobiner::fix_name(r$planting_method,case = "lower"),
    irrigated = grepl("irrigated", r$Production_system, ignore.case = TRUE),
    stringsAsFactors = FALSE
  )
  
  # ==========================================================
  # Trial identifier
  # ==========================================================
  
  d$trial_id <- as.character(
    interaction(
      r$Country,
      r$State,
      r$Year,
      r$Season,
      drop = TRUE
    )
  )
  
  # Observation date
  d$date <- r$survey_date
  
  # ==========================================================
  # Geographic information
  # ==========================================================
  
  d$longitude <- as.numeric(gsub(",", ".", r$Field_longitude))
  d$latitude <- as.numeric(gsub(",", ".", r$Field_latitude))
  d$geo_from_source <- TRUE
  
  # ==========================================================
  # Experimental information
  # ==========================================================
  
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  d$seed_rate <- as.numeric(r$`Quantity_of_seed_used_(kg/ha)`)
  d$planting_date <- r$planting_date
  d$transplanting_method <- as.character(r$planting_method)
  
  # ==========================================================
  # Fertilizer information
  # ==========================================================
  
  d$N_fertilizer <- as.numeric(r$`Quanity_of_N_applied_(kg/ha)`)
  d$P_fertilizer <- as.numeric(r$`Quantity_of_P2O5_applied_(kg/ha)`)
  d$K_fertilizer <- as.numeric(r$`Quantity_of_K2O_applied_(kg/ha)`)
  
  
  d$N_splits <- as.integer(r$Number_of_inorganic_fertilizer_application)
  
  # ==========================================================
  # Management information
  # ==========================================================
  
  # Transform the character (YES/NO) variables to logical
  r$Herbicide_use <- r$Herbicide_use == "Yes"
  r$weeding_done <- r$weeding_done == "Yes" 
  r$Insecticide_use <- r$Insecticide_use == "Yes" 
  r$Organic_input_use <- r$Organic_input_use == "Yes"
  
  # Use the transformed the variables for the rest of the script
  
  d$land_prep_method <- carobiner::fix_name(r$number_tillage, case = "lower")
  
  d$weeding_times <- as.integer(r$Number_of_weeding)
  
  d$herbicide_used <- r$Herbicide_use
  
  d$weeding_done <- r$weeding_done
  
  d$insecticide_times <- as.integer(r$Number_of_insecticide_application)
  
  d$insecticide_used <- r$Insecticide_use
    
  # Organic input 
  d$OM_used <- r$Organic_input_use
  d$OM_amount <- r$`Quantity_of_organic_input_applied_(kg/ha)`
  
  # ==========================================================
  # Yield information
  # ==========================================================
  
  d$yield <- as.numeric(r$`Paddy_yield_(kg/ha)`)
  d$yield_part <- "grain"
  # d$yield_moisture <- 14
  
  # ==========================================================
  # Household identifier
  # ==========================================================
  
  d$hhid <- carobiner::fix_name(r$barcode_household, case = "")
  d$sex <- as.character(r$Gender)
  d$age <- as.numeric(r$Farmer_age)
  d$education <- as.character(r$Farmer_education)
  d$field_size <- as.numeric(r$`Field_size_(ha)`)
  
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
