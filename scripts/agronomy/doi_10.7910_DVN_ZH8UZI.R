# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
  
  "Baseline household and community survey conducted under the Africa RISING 
project in northern Ghana (Northern Region, Upper West and Upper East) in 2013,
covering the 2012 cropping season. Data includes crop production across multiple 
plots per household, land use, livestock, labor and household demographics. 
Published by Wageningen University and Research (WUR). Maize is the dominant 
crop in the Northern Region, which also includes groundnut, rice, soybean, 
cowpea, yam and cassava in a mixed farming system."
  
  uri <- "doi:10.7910/DVN/ZH8UZI"
  group <- "agronomy"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
                                  publication = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-06-09",
                                  data_type = "survey",
                                  data_organization = "WUR",
                                  project = "Africa RISING",
                                  treatment_vars = "none",
                                  response_vars = "yield",
                                  notes = "Only NorthernRegion file contains crop data; 
                                  Upper West and Upper East contain only demographic and livestock data. 
                                  Data is at subplot level (D2 section): each row is one household-subplot combination with up to 6 subplots per household. 
                                  Subplot area used for yield calculation (not total plot area). 
                                  Harvest converted to kg: bags multiplied by 50 (standard 50kg bag, per codebook p.11); 
                                  kg values used directly; bare numbers set to NA. Fertilizer from free-text field: NPK 15-15-15 (confirmed in data); 
                                  SA = 21% N; UREA = 46% N; unmarked numbers assumed bags. 
                                  Crop residue management (D4) not included as subplot numbering does not align with D2. 
                                  Variety spelling standardized: obatanpa, akomasa, afife, chekopag, laribako, gbena, topsii.",
                                  design = NA,
                                  completion = 80
  )
  
  #Load only Northern Region file that contains crop data
  f <- ff[basename(ff) == "NorthernRegion.tab"]
  nr <- read.delim(f)
  
  # convert harvest text to kg; standard 50kg bag confirmed in survey codebook
  parse_harvest <- function(x) {
    x <- trimws(toupper(x))
    num <- as.numeric(gsub("[^0-9.]", "", x))
    is_bag <- grepl("BAG", x)
    is_kg  <- grepl("KG", x)
    # default NA for unknown units (bare numbers without unit)
    result <- rep(NA_real_, length(x))
    result[is_bag] <- num[is_bag] * 50
    result[is_kg]  <- num[is_kg]
    result
  }
  
  # extract fertilizer quantities and convert to N, P, K kg/ha
  # NPK formulation 15-15-15 confirmed from "NPK 15 15 15" entry in data
  # SA (sulphate of ammonia) = 21% N; UREA = 46% N
  # bag weight assumed 50kg where no unit specified
  parse_fertilizer <- function(fert, area_ha) {
    x <- trimws(toupper(fert))
    
    npk_kg <- rep(NA_real_, length(x))
    
    # NPK with KG explicit e.g. "NPK150KG", "NPK 150KG"
    has_npk_kg <- grepl("NPK\\s*[0-9]+\\s*KG", x)
    idx <- which(has_npk_kg)
    matched <- regmatches(x[idx], regexpr("NPK\\s*[0-9]+\\s*KG", x[idx]))
    npk_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched))
    
    # NPK with BAGS explicit e.g. "NPK2BAGS", "NPK 2 BAGS"
    has_npk_bag <- grepl("NPK\\s*[0-9]+\\s*BAG", x)
    idx <- which(has_npk_bag)
    matched <- regmatches(x[idx], regexpr("NPK\\s*[0-9]+\\s*BAG", x[idx]))
    npk_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched)) * 50
    
    # NPK with number only e.g. "NPK1 SA1" — assumed bags
    has_npk_num <- grepl("NPK\\s*[0-9]+\\s*SA", x) & !has_npk_kg & !has_npk_bag
    idx <- which(has_npk_num)
    matched <- regmatches(x[idx], regexpr("NPK\\s*[0-9]+", x[idx]))
    npk_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched)) * 50
    
    sa_kg <- rep(NA_real_, length(x))
    
    # SA with KG explicit
    has_sa_kg <- grepl("SA\\s*[0-9]+\\s*KG", x)
    idx <- which(has_sa_kg)
    matched <- regmatches(x[idx], regexpr("SA\\s*[0-9]+\\s*KG", x[idx]))
    sa_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched))
    
    # SA with BAGS explicit
    has_sa_bag <- grepl("SA\\s*[0-9]+\\s*BAG", x)
    idx <- which(has_sa_bag)
    matched <- regmatches(x[idx], regexpr("SA\\s*[0-9]+\\s*BAG", x[idx]))
    sa_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched)) * 50
    
    # SA with number only — assumed bags
    has_sa_num <- grepl("SA\\s*[0-9]+", x) & !has_sa_kg & !has_sa_bag
    idx <- which(has_sa_num)
    matched <- regmatches(x[idx], regexpr("SA\\s*[0-9]+", x[idx]))
    sa_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched)) * 50
    
    urea_kg <- rep(NA_real_, length(x))
    
    # UREA with KG explicit
    has_urea_kg <- grepl("UREA\\s*[0-9]+\\s*KG", x)
    idx <- which(has_urea_kg)
    if (length(idx) > 0) {
      matched <- regmatches(x[idx], regexpr("UREA\\s*[0-9]+\\s*KG", x[idx]))
      urea_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched))
    }
    
    # calculate N P K only where at least one quantity is known
    has_any  <- !is.na(npk_kg) | !is.na(sa_kg) | !is.na(urea_kg)
    npk_kg0  <- ifelse(is.na(npk_kg),  0, npk_kg)
    sa_kg0   <- ifelse(is.na(sa_kg),   0, sa_kg)
    urea_kg0 <- ifelse(is.na(urea_kg), 0, urea_kg)
    
    N <- P <- K <- rep(NA_real_, length(x))
    N[has_any] <- (npk_kg0[has_any] * 0.15 + sa_kg0[has_any] * 0.21 + urea_kg0[has_any] * 0.46) / area_ha[has_any]
    P[has_any] <- (npk_kg0[has_any] * 0.15) / area_ha[has_any]
    K[has_any] <- (npk_kg0[has_any] * 0.15) / area_ha[has_any]
    
    data.frame(N_fertilizer = N, P_fertilizer = P, K_fertilizer = K)
  }
  
  # standardize crop names to terminag vocabulary
  clean_crop <- function(x) {
    x <- trimws(tolower(x))
    x[x == "corn"]     <- "maize"
    x[x == "g nut"]    <- "groundnut"
    x[x == "g nuts"]   <- "groundnut"
    x[x == "gnut"]     <- "groundnut"
    x[x == "soyabean"] <- "soybean"
    x[x == "soya"]     <- "soybean"
    x[x == "b beans"]  <- "cowpea"
    x[x == "beans"]    <- "cowpea"
    x[x == "g pepper"] <- "pepper"
    x[x == "casava"]   <- "cassava"
    x[x == "okro"]     <- "okra"
    x[x == "watermel"] <- "watermelon"
    x
  }
  
  # standardize variety names
  # spelling variations confirmed by crop context
  clean_variety <- function(x) {
    x <- trimws(tolower(x))
    x[x %in% c("obaatapa", "obataapa", "obatampa", "0batampa")] <- "obatanpa"
    x[x == "okomasa"]                          <- "akomasa"
    x[x %in% c("afefe", "afifi")]              <- "afife"
    x[x %in% c("tops", "topsi", "tox")]        <- "topsii"
    x[x %in% c("chekopa", "chocopag")]         <- "chekopag"
    x[x %in% c("larbako", "labako")]           <- "laribako"
    x[x == "gbeno"]                            <- "gbena"
    # variety entered as crop name - set to NA
    x[x == "maize"] <- NA
    x[x == ""]      <- NA
    x
  }
  
  # standardize intercrop names to terminag vocabulary
  clean_intercrop <- function(x) {
    x <- trimws(tolower(x))
    x[x %in% c("g nut", "g nuts", "gnut")] <- "groundnut"
    x[x %in% c("soyabean", "soya")]         <- "soybean"
    x[x %in% c("b beans", "beans")]         <- "cowpea"
    x[grepl("maize", x)]  <- "maize"
    x[x == "own"]         <- NA
    x[grepl("%", x)]      <- NA
    x[x == ""] <- NA
    x
  }
  
  # reshape from wide to long format
  # each household has up to 6 subplots recorded as separate column groups (D2 section)
  # subplot numbering uses numbers (1,2,3) for separate plots and letters (1a,1b) for subplots
  plot_list <- lapply(1:6, function(i) {
    crop       <- nr[[paste0("MAIN_CROP.", i)]]
    harvest    <- parse_harvest(nr[[paste0("TOTAL_HARVEST.", i)]])
    # subplot area in local units (acres), converted to hectares
    area_ha    <- nr[[paste0("SIZE_OF_SUB_PLOT.", i)]] * 0.405
    variety    <- nr[[paste0("INDICATE_VARIETY.", i)]]
    intercrop  <- nr[[paste0("MAJOR_INTERCROP.", i)]]
    fert_text  <- nr[[paste0("QUANTITY_AND_TYPE.", i)]]
    subplot_id <- nr[[paste0("SUB_PLOT.", i)]]
    
    fert <- parse_fertilizer(fert_text, area_ha)
    
    # only keep rows where crop is recorded
    keep <- !is.na(crop) & trimws(crop) != ""
    
    data.frame(
      trial_id = trimws(paste0(nr$QUESTIONNAIRE_ID[keep], "-", subplot_id[keep])),
      crop            = clean_crop(crop[keep]),
      variety         = clean_variety(variety[keep]),
      intercrops      = clean_intercrop(intercrop[keep]),
      yield           = harvest[keep] / area_ha[keep],
      yield_part      = "grain",
      yield_moisture = NA_real_,
      yield_isfresh   = FALSE,
      N_fertilizer    = fert$N_fertilizer[keep],
      P_fertilizer    = fert$P_fertilizer[keep],
      K_fertilizer    = fert$K_fertilizer[keep],
      farmer_gender   = nr$A2[keep],
      country         = "Ghana",
      adm1            = "Northern",
      location        = "Northern Region",
      # coordinates are approximate for Northern Region centroid
      # GPS fields in survey were not captured digitally
      longitude       = -1.0,
      latitude        = 9.5,
      geo_from_source = FALSE,
      planting_date   = "2012",
      harvest_date    = "2012",
      on_farm         = TRUE,
      is_survey       = TRUE,
      irrigated       = FALSE
    )
  })
  
  d <- do.call(rbind, plot_list)
  
  # assign correct yield_part for non-grain crops
  d$yield_part[d$crop %in% c("yam", "cassava")] <- "tubers"
  d$yield_part[d$crop == "groundnut"]            <- "pod"
  
  carobiner::write_files(meta, d, path = path)
}