# R script for "carob"
# license: GPL (>=3)

## ISSUES
# - The source data do not provide latitude or longitude.
# - The source data do not provide planting dates or harvest dates.
# - The source data do not provide fertilizer or irrigation information.
# - The additional response variables below are retained because they are
#   measured variables in the source dataset and are relevant to the study.
# - Some additional response variable names may not yet be in the Carob
#   controlled vocabulary. They are retained for editor review rather than
#   being removed from the standardized dataset.
# - The source dataset contains locality names such as Majes, Majes1 and
#   Majes2. These values are retained as provided in the source data.

carob_script <- function(path) {
  
  uri <- "doi:10.21223/MXKUIK"
  group <- "agronomy"
  
  ff <- carobiner::get_data(
    uri,
    path,
    group
  )
  
  
  ## =========================================================
  ## Metadata
  ## =========================================================
  
  meta <- carobiner::get_metadata(
    uri,
    path,
    group,
    major = 1,
    minor = 1,
    
    # CIP is the data organization identified for this dataset.
    data_organization = "CIP",
    
    publication =
      "New Resistant Potato Varieties to Late Blight and High Quality for French Fries generated in Peru",
    
    project = NA,
    
    design =
      "Randomized complete block design with three replications",
    
    data_type = "experiment",
    
    treatment_vars = "variety",
    
    response_vars = paste(
      c(
        "yield",
        "nph",
        "mtwp",
        "nomtwp",
        "ttwp",
        "mtyna",
        "ttyna",
        "mtya",
        "ttya",
        "dm_hydrometer",
        "dm_oven",
        "reducing_sugars",
        "ffr_at_harvest",
        "ffr_blanching",
        "ffr_90_days",
        "flavbp1",
        "flavbp2",
        "flavbp3",
        "texbp1",
        "texbp2",
        "texbp3"
      ),
      collapse = ";"
    ),
    
    notes =
      "Potato adaptation and efficiency trials conducted during the 2019-2020 and 2020-2021 agricultural campaigns in farmers' fields in Peru. The experiments used a randomized complete block design with three replications.",
    
    carob_contributor = "MARYAM YAHYA",
    
    carob_date = "2026-08-24",
    
    carob_completion = 100,
    
    carob_effort = 1
  )
  
  
  ## =========================================================
  ## Identify source Excel files
  ## =========================================================
  
  f1 <- ff[
    basename(ff) ==
      "01_Data_Adaptation_and_Efficiency_Trials.xlsx"
  ]
  
  f2 <- ff[
    basename(ff) ==
      "02_Data_dictionary_Adaptation_Efficiency_trials.xlsx"
  ]
  
  
  ## =========================================================
  ## Read source data
  ## =========================================================
  
  r1 <- carobiner::read.excel(f1)
  
  r2 <- carobiner::read.excel(f2)
  
  
  ## =========================================================
  ## Check that required source variables are present
  ## =========================================================
  
  required_r1 <- c(
    "Plot",
    "Clone",
    "Rep",
    "Year",
    "Locality",
    "NPH",
    "MTWP",
    "NoMTWP",
    "TTWP",
    "MTYNA",
    "TTYNA",
    "MTYA",
    "TTYA",
    "DM_Hydrometer_method",
    "DM_Oven_drying_ method",
    "Reducing_sugars_%",
    "French_Fry_ color_At_Harvest",
    "French_Fry_ color_Blanching",
    "French_Fry_ color_ 90_days_after_harvest",
    "Baked Flavor 1",
    "Baked Flavor 2",
    "Baked Flavor 3",
    "Baked_Texture1",
    "Baked_Texture2",
    "Baked_Texture3"
  )
  
  stopifnot(all(required_r1 %in% names(r1)))
  
  
  required_r2 <- c(
    "Factor_Variables",
    "Abbreviations",
    "Other_name",
    "Crop_Ontology",
    "Category"
  )
  
  stopifnot(all(required_r2 %in% names(r2)))
  
  
  ## =========================================================
  ## Standardize data
  ## =========================================================
  
  d <- data.frame(
    
    ## ---------------------------------------------------------
    ## Trial identification
    ##
    ## A trial is defined by locality and agricultural campaign.
    ## Replications and varieties remain observations within trials.
    ## ---------------------------------------------------------
    
    trial_id = paste(
      "MXKUIK",
      gsub("[ ,]+", "_", as.character(r1$Locality)),
      as.character(r1$Year),
      sep = "_"
    ),
    
    plot_id = as.character(r1$Plot),
    
    
    ## ---------------------------------------------------------
    ## Basic experiment information
    ## ---------------------------------------------------------
    
    crop = "potato",
    
    variety = as.character(r1$Clone),
    
    rep = as.integer(r1$Rep),
    
    country = "Peru",
    
    location = as.character(r1$Locality),
    
    
    ## ---------------------------------------------------------
    ## Standardized yield
    ##
    ## MTYA is the adjusted marketable tuber yield.
    ## Source unit is t/ha.
    ## Carob yield is expressed in kg/ha.
    ## ---------------------------------------------------------
    
    yield = as.numeric(r1$MTYA) * 1000,
    
    yield_part = "tubers",
    
    # The source does not provide a yield moisture value.
    yield_moisture = NA_real_,
    
    # MTYA is reported as tuber yield and is treated as fresh yield.
    yield_isfresh = TRUE,
    
    
    ## ---------------------------------------------------------
    ## Additional yield variables from the source dataset
    ## ---------------------------------------------------------
    
    mtyna = as.numeric(r1$MTYNA),
    
    ttyna = as.numeric(r1$TTYNA),
    
    mtya = as.numeric(r1$MTYA),
    
    ttya = as.numeric(r1$TTYA),
    
    
    ## ---------------------------------------------------------
    ## Plot-level tuber measurements
    ## ---------------------------------------------------------
    
    nph = as.numeric(r1$NPH),
    
    mtwp = as.numeric(r1$MTWP),
    
    nomtwp = as.numeric(r1$NoMTWP),
    
    ttwp = as.numeric(r1$TTWP),
    
    
    ## ---------------------------------------------------------
    ## Dry matter
    ## ---------------------------------------------------------
    
    dm_hydrometer = as.numeric(
      r1$DM_Hydrometer_method
    ),
    
    dm_oven = as.numeric(
      r1$`DM_Oven_drying_ method`
    ),
    
    
    ## ---------------------------------------------------------
    ## Reducing sugars
    ## ---------------------------------------------------------
    
    reducing_sugars = as.numeric(
      r1$`Reducing_sugars_%`
    ),
    
    
    ## ---------------------------------------------------------
    ## French-fry colour
    ## ---------------------------------------------------------
    
    ffr_at_harvest = as.numeric(
      r1$`French_Fry_ color_At_Harvest`
    ),
    
    ffr_blanching = as.numeric(
      r1$`French_Fry_ color_Blanching`
    ),
    
    ffr_90_days = as.numeric(
      r1$`French_Fry_ color_ 90_days_after_harvest`
    ),
    
    
    ## ---------------------------------------------------------
    ## Baked potato flavour
    ## ---------------------------------------------------------
    
    flavbp1 = as.numeric(
      r1$`Baked Flavor 1`
    ),
    
    flavbp2 = as.numeric(
      r1$`Baked Flavor 2`
    ),
    
    flavbp3 = as.numeric(
      r1$`Baked Flavor 3`
    ),
    
    
    ## ---------------------------------------------------------
    ## Baked potato texture
    ## ---------------------------------------------------------
    
    texbp1 = as.numeric(
      r1$Baked_Texture1
    ),
    
    texbp2 = as.numeric(
      r1$Baked_Texture2
    ),
    
    texbp3 = as.numeric(
      r1$Baked_Texture3
    ),
    
    
    ## ---------------------------------------------------------
    ## Experiment characteristics
    ## ---------------------------------------------------------
    
    on_farm = TRUE,
    
    is_survey = FALSE,
    
    irrigated = NA,
    
    crop_rotation = NA,
    
    planting_date = NA,
    
    harvest_date = NA,
    
    
    ## ---------------------------------------------------------
    ## Geographic information
    ##
    ## Coordinates are not provided in the supplied source files.
    ## They are therefore not estimated or guessed here.
    ## ---------------------------------------------------------
    
    longitude = NA_real_,
    
    latitude = NA_real_,
    
    geo_from_source = FALSE,
    
    
    ## ---------------------------------------------------------
    ## Fertilizer information
    ##
    ## Fertilizer rates and fertilizer type are not provided in
    ## the supplied source files.
    ## ---------------------------------------------------------
    
    P_fertilizer = NA_real_,
    
    K_fertilizer = NA_real_,
    
    N_fertilizer = NA_real_,
    
    S_fertilizer = NA_real_,
    
    lime = NA_real_,
    
    
    ## ---------------------------------------------------------
    ## Legume-specific variables
    ##
    ## These are not applicable to the potato experiment, but the
    ## standard template includes them.
    ## ---------------------------------------------------------
    
    inoculated = NA,
    
    inoculant = NA,
    
    stringsAsFactors = FALSE
  )
  
  
  ## =========================================================
  ## Write Carob files
  ## =========================================================
  
  carobiner::write_files(
    path,
    meta,
    d
  )
}