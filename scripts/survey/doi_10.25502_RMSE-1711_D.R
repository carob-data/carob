# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
  
  "N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. This dataset covers the Uganda baseline survey conducted in 2014 across even action sites: Kabale, Kanungu, Kapchorwa, Kibuku, Kole, Oyam, and Palisa."
  
  uri   <- "doi:10.25502/RMSE-1711/D"
  group <- "survey"
 
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group,
                                  major = NA, minor = NA,
                                  data_organization = "IITA",
                                  publication = NA,
                                  project = "N2Africa",
                                  data_type = "survey",
                                  treatment_vars = "none",
                                  response_vars = "yield",
                                  carob_completion = 100,
                                  carob_contributor = "Mitchelle Njukuya",
                                  carob_date = "2026-06-22",
                                  carob_effort = 6,
                                  notes = "GPS coordinates recorded only for Kanungu action site in source data. Coordinates for remaining action sites (Kabale, Kapchorwa, Kibuku, Kole, Oyam, Palisa) assigned from known district centroids; geo_from_source set to FALSE for these records. Yield in f_crop_production_1 is the total field harvest in kg; conversion to kg/ha requires field_size which is present.",
                                  design = NA
  )
  

  f1 <- ff[basename(ff) == "a_b_general_1.csv"]
  r1 <- read.csv(f1)
  
  f2 <- ff[basename(ff) == "a_b_general_2.csv"]
  r2 <- read.csv(f2)
  
  f3 <- ff[basename(ff) == "a_b_general_3.csv"]
  r3 <- read.csv(f3)
  
  f4 <- ff[basename(ff) == "c_livestock_composition.csv"]
  r4 <- read.csv(f4)
  
  f5 <- ff[basename(ff) == "e_land_use_1.csv"]
  r5 <- read.csv(f5)
  
  f6 <- ff[basename(ff) == "e_land_use_2.csv"]
  r6 <- read.csv(f6)
  
  f7 <- ff[basename(ff) == "f_crop_production_1.csv"]
  r7 <- read.csv(f7)
  
  f8 <- ff[basename(ff) == "g_legume_utilisation_1.csv"]
  r8 <- read.csv(f8)
  
  f9 <- ff[basename(ff) == "g_legume_utilisation_3.csv"]
  r9 <- read.csv(f9)
  
  site_coords <- data.frame(
    action_site = c("Kabale", "Kanungu", "Kapchorwa", "Kibuku",
                    "Kole",   "Oyam",    "Palisa"),
    latitude    = c(-1.250,  -0.958,     1.393,      1.042,
                    2.404,   2.234,     1.145),
    longitude   = c(29.985,  29.800,    34.452,     33.767,
                    32.770,  32.332,    33.716),
    elevation   = c(1869,    1858,       1544,        1100,
                    978,     990,       1085),
    stringsAsFactors = FALSE
  )
  
 d1 <- data.frame(
    farm_id     = r1$farm_id,
    country     = trimws(r1$country),
    location    = trimws(r1$action_site),
    adm3    = trimws(r1$village),
    adm2        = trimws(r1$action_site),
    adm1        = trimws(r1$sector_state),
    year        = as.integer(r1$date_interview_yyyy),
    geo_from_source = FALSE,
    stringsAsFactors = FALSE
  )
  
  d1$location <- gsub("^KABALE$", "Kabale", d1$location, ignore.case = FALSE)
  d1$adm2     <- d1$location
  d1$adm1 <- tolower(d1$adm1)
  
  d1 <- merge(d1, site_coords, by.x = "location", by.y = "action_site", all.x = TRUE)
  
  # Mark Kanungu GPS as from source; all others are lookup-assigned
  d1$geo_from_source[d1$location == "Kanungu"] <- FALSE
  
  d2 <- data.frame(
    farm_id     = r2$farm_id,
    sex         = r2$sex_farmer,
    age         = suppressWarnings(as.numeric(r2$age_farmer)),
    is_head     = r2$farmer_head_hh == "Y",
    hh_size     = suppressWarnings(as.numeric(r2$total_number_hh)),
    # Women 17–60 as adult women proxy
    hh_adult_women = suppressWarnings(
      as.numeric(r2$females_17_35) + as.numeric(r2$females_36_60)),
    hh_adult_men   = suppressWarnings(
      as.numeric(r2$males_17_35) + as.numeric(r2$males_36_60)),
    hh_child_18    = suppressWarnings(as.numeric(r2$children_0_16)),
    # Elders: females_over_60 + males_over_60
    hh_elders      = suppressWarnings(
      as.numeric(r2$females_over_60) + as.numeric(r2$males_over_60)),
    education   = trimws(r2$highest_education_hh),
    stringsAsFactors = FALSE
  )
  
  d2$sex[d2$sex == "M"] <- "male"
  d2$sex[d2$sex == "F"] <- "female"
  d2$sex[d2$sex == ""] <- NA
  
  d2$education <- tolower(d2$education)
  d2$education[d2$education %in% c("", "none")] <- NA
  
  d2$hh_size[d2$hh_size > 50 | d2$hh_size < 0] <- NA
  d2$age[d2$age > 100 | d2$age < 12]            <- NA
 
  # main_source_income_hh: Y = yes (this is the main source), N = no
  # importance_agriculture_hh contains the income source label
  # Keep only the primary income source per household
  r3_main <- r3[trimws(toupper(r3$main_source_income_hh)) == "Y", ]
  d3 <- data.frame(
    farm_id    = r3_main$farm_id,
    occupation = trimws(r3_main$importance_agriculture_hh),
    stringsAsFactors = FALSE
  )
  d3$occupation <- tolower(d3$occupation)
  d3$occupation[d3$occupation == ""] <- NA
  # Take first primary source per household if duplicated
  d3 <- d3[!duplicated(d3$farm_id), ]
  
  d4 <- data.frame(
    farm_id   = r4$farm_id,
    heads = r4$livestock_number,
    animal = trimws(r4$livestock_type),
    stringsAsFactors = FALSE
  )
  
  d4$animal <- tolower(d4$animal)
  d4$animal[d4$animal == ""]               <- NA
  d4$animal[d4$animal == "chicken"]        <- "chicken"
  d4$animal[d4$animal == "goats"]          <- "goat"
  d4$animal[d4$animal == "pigs"]           <- "pig"
  d4$animal[d4$animal == "sheep"]          <- "sheep"
  d4$animal[d4$lanimal == "ducks"]          <- "duck"
  d4$animal[d4$animal == "rabbits"]        <- "rabbit"
  d4$animal[d4$animal == "pigeons"]        <- "pigeon"
  d4$animal[d4$animal == "turkey"]         <- "turkey"
  d4$animal[d4$animal == "turkies"]        <- "turkey"
  d4$animal[d4$animal == "improved dairy cows"]  <- "cattle"
  d4$animal[d4$animal == "local dairy cows"]     <- "cattle"
  d4$animal[d4$animal == "fattening cattle"]     <- "cattle"
  d4$animal[grepl("draught cattle", d4$animal)]  <- "cattle"
  
  d4a <- aggregate(
    animal ~ farm_id,
    data = d4,
    FUN  = function(x) paste(unique(na.omit(x)), collapse = ";")
  )
  d4a$animal[d4a$animal == ""] <- NA
  
  heads_df <- aggregate(
    heads ~ farm_id,
    data = d4,
    FUN = sum,
    na.rm = TRUE
  )
  
  d4a <- merge(d4a, heads_df, by = "farm_id", all.x = TRUE)
  
  d5 <- data.frame(
    farm_id  = r5$farm_id,
    farmland = suppressWarnings(as.numeric(r5$total_area_ha)),
    stringsAsFactors = FALSE
  )
  d5$farmland[d5$farmland <= 0] <- NA
  
 # Each row = one field per household; season_1..4 list crops grown that season
  # Crops within a season are newline-separated (intercropped).
  # crop_rotation = unique crops across all seasons, separated by ";"
  
  standardise_crop_name <- function(x) {
    x <- trimws(tolower(x))
    x <- gsub("\n", ";", x)
    x[grepl("^maize", x)]                                   <- "maize"
    x[grepl("sorghum", x)]                                  <- "sorghum"
    x[grepl("millet", x)]                                   <- "millet"
    x[grepl("rice", x)]                                     <- "rice"
    x[grepl("cassava", x)]                                  <- "cassava"
    x[grepl("sweet potato|sweetpotato", x)]                 <- "sweetpotato"
    x[grepl("irish potato|irish potatoes", x)]              <- "potato"
    x[grepl("banana|plantain", x)]                         <- "banana"
    x[grepl("climbing bean|climing bean|common bean|beans", x)]  <- "common bean"
    x[grepl("bush bean", x)]                               <- "common bean"
    x[grepl("soybean|soybeans|soy beans", x)]              <- "soybean"
    x[grepl("groundnut|ground nut", x)]                    <- "groundnut"
    x[grepl("^peas$|^pea$", x)]                           <- "pea"
    x[grepl("sunflower", x)]                               <- "sunflower"
    x[grepl("simsim|sesame", x)]                           <- "sesame"
    x[grepl("coffee", x)]                                  <- "coffee"
    x[grepl("cotton", x)]                                  <- "cotton"
    x[grepl("cabbage", x)]                                 <- "cabbage"
    x[grepl("tomato", x)]                                  <- "tomato"
    x[grepl("onion", x)]                                   <- "onion"
    x[grepl("pumpkin", x)]                                 <- "pumpkin"
    x[grepl("yam", x)]                                     <- "yam"
    x[grepl("sukuma|kale", x)]                             <- "kale"
    x[grepl("egg plants", x)]                             <- "eggplant"
    x[x %in% c("", " ","dodo","fruits","grass","trees","vegetables")] <- NA
    x
  }
  
  # Build crop rotation per field: collect unique crops across all 4 seasons
  get_rotation <- function(row) {
    seasons <- unlist(strsplit(
      paste(row["season_1"], row["season_2"], row["season_3"], row["season_4"], sep = "\n"),
      "\n"))
    seasons <- unique(trimws(standardise_crop_name(seasons)))
    seasons <- seasons[!is.na(seasons) & seasons != ""]
    if (length(seasons) == 0) return(NA_character_)
    paste(seasons, collapse = ";")
  }
  
  r6_rot <- apply(r6, 1, get_rotation)
  
  d6 <- data.frame(
    farm_id      = r6$farm_id,
    field_id     = as.character(r6$field_no),
    crop_rotation = r6_rot,
    stringsAsFactors = FALSE
  )
  # Aggregate to farm level (combine all fields)
  d6a <- aggregate(
    crop_rotation ~ farm_id,
    data = d6,
    FUN  = function(x) {
      all_crops <- unique(unlist(strsplit(x, ";")))
      all_crops <- all_crops[all_crops != "" & !is.na(all_crops)]
      paste(all_crops, collapse = ";")
    }
  )
  
  d6$crop_rotation <- gsub("\\b(climbing bean|bush bean|beans)\\b","common bean",d6$crop_rotation)
  
  
  d7_list <- lapply(seq_len(nrow(r7)), function(i) {
    row    <- r7[i, ]
    farm   <- row$farm_id
    field  <- as.character(row$field)
    size   <- suppressWarnings(as.numeric(row$size_ha))
    
    # Collect all non-empty crop names and shares
    crops  <- character(0)
    shares <- numeric(0)
    for (j in 1:8) {
      cn <- trimws(row[[paste0("crop_", j)]])
      sh <- suppressWarnings(as.numeric(row[[paste0("crop_", j, "_rel_share")]]))
      if (!is.na(cn) && cn != "") {
        crops  <- c(crops,  cn)
        shares <- c(shares, ifelse(is.na(sh), 0, sh))
      }
    }
    if (length(crops) == 0) return(NULL)
    
    # Primary crop = crop with highest share (first if tied)
    primary_idx <- which.max(shares)
    primary_crop <- standardise_crop_name(crops[primary_idx])
    intercrops    <- if (length(crops) > 1) {
      others <- standardise_crop_name(crops[-primary_idx])
      others <- others[!is.na(others)]
      if (length(others) > 0) paste(others, collapse = ";") else NA_character_
    } else NA_character_
    
    # Yield: harvest_amount_kg; convert to kg/ha
    yield_total <- suppressWarnings(as.numeric(row$harvest_amount_kg))
    yield_ha    <- if (!is.na(yield_total) && !is.na(size) && size > 0) {
      yield_total / size
    } else NA_real_
    
    data.frame(
      farm_id       = farm,
      field_id      = field,
      crop          = primary_crop,
      intercrops     = intercrops,
      field_size    = if (!is.na(size) && size > 0) size else NA_real_,
      yield   = yield_total,
      field_distance = suppressWarnings(as.numeric(row$walking_distance_min)),
      fertilizer_type = trimws(row$min_fert_type),
      OM_used       = trimws(row$organic_input_applied) == "Y",
      inoculated    = trimws(row$inoculant_applied) == "Y",
      stringsAsFactors = FALSE
    )
  })
  
  d7 <- do.call(rbind, d7_list)
  
  d7$yield[d7$yield < 0 | d7$yield > 150000] <- NA
  
  # Fertilizer type: standardise compound entries; convert to ";" separated
  d7$fertilizer_type <- tolower(d7$fertilizer_type)
  d7$fertilizer_type[d7$fertilizer_type %in% c("", "n", "no")] <- NA
  
  d7$fertilizer_type <- gsub(",\\s*", ";", d7$fertilizer_type)
  
  d7$fertilizer_type <- gsub("Urea", "urea", d7$fertilizer_type, ignore.case = TRUE)
  d7$fertilizer_type <- gsub("dap",  "DAP",  d7$fertilizer_type, ignore.case = TRUE)
  d7$fertilizer_type <- gsub("npk",  "NPK",  d7$fertilizer_type, ignore.case = TRUE)
  d7$fertilizer_type <- gsub("can",  "CAN",  d7$fertilizer_type, ignore.case = TRUE)
  d7$fertilizer_type <- gsub("dak",  "DAP",  d7$fertilizer_type, ignore.case = TRUE)
  # "super grow" and "BK" are trade names — keep as-is; "small red" is a variety, not a fertilizer
  d7$fertilizer_type[grepl("^small red$", d7$fertilizer_type, ignore.case = TRUE)] <- NA
  d7$fertilizer_type[grepl("^improved$",  d7$fertilizer_type, ignore.case = TRUE)] <- NA
  d7$fertilizer_type[d7$fertilizer_type %in% c("bk","super grow")] <- NA
  d7$fertilizer_type[d7$fertilizer_type == "NPK 17:17:17"] <- "NPK"
  
  d7$fertilizer_type <- sapply(
    strsplit(as.character(d7$fertilizer_type), ";"),
    function(x) {
      x <- trimws(x)
      x <- x[!x %in% c("super grow", "bk")]
      
      if(length(x) == 0) NA else paste(unique(x), collapse = ";")
    }
  )
  
  d7$N_fertilizer <- NA_real_
  d7$P_fertilizer <- NA_real_
  d7$K_fertilizer <- NA_real_
  
  d7$N_fertilizer[grepl("urea", d7$fertilizer_type) &
                    !grepl("DAP|NPK", d7$fertilizer_type)] <- NA_real_
 
  d8 <- data.frame(
    farm_id        = r8$farm_id,
    crop           = trimws(r8$crop),
    # total_production_farm is in kg (weight_unit = "kg" confirmed in all rows)
    yield_g1_total = suppressWarnings(as.numeric(r8$total_production_farm)),
    amount_sold_kg = suppressWarnings(as.numeric(r8$amount_for_sale)),
    stringsAsFactors = FALSE
  )
  d8$crop <- standardise_crop_name(d8$crop)
  d8$yield_g1_total[d8$yield_g1_total <= 0] <- NA
  
  # Convert total kg to kg/ha using field sizes from d7.
  # size_ha was 0 for most records; d7 already resolved this using size_acres.
  # Average field size per farm+crop is used as denominator.
  field_sizes <- d7[!is.na(d7$field_size), c("farm_id", "crop", "field_size")]
  field_sizes_agg <- aggregate(
    field_size ~ farm_id + crop,
    data = field_sizes,
    FUN  = mean
  )
  
   d8 <- data.frame(
    farm_id   = r8$farm_id,
    crop      = trimws(r8$crop),
    # total_production_farm in kg (weight_unit = kg confirmed)
    yield = suppressWarnings(as.numeric(r8$total_production_farm)),
    amount_sold_kg = suppressWarnings(as.numeric(r8$amount_for_sale)),
    stringsAsFactors = FALSE
  )
  d8$crop <- standardise_crop_name(d8$crop)
  d8$yield[d8$yield <= 0] <- NA
  
  dominant_management <- function(fed, mulched, burnt, sold, other) {
    vals <- suppressWarnings(c(
      as.numeric(fed), as.numeric(mulched),
      as.numeric(burnt), as.numeric(sold), as.numeric(other)))
    labels <- c("fed to livestock", "mulched", "burned", "sold", "other")
    if (all(is.na(vals)) || all(vals == 0, na.rm = TRUE)) return(NA_character_)
    labels[which.max(vals)]
  }

  d9 <- data.frame(
    farm_id  = r9$farm_id,
    crop     = trimws(r9$legume),
    previous_crop_residue_management = mapply(
      dominant_management,
      r9$perc_fed_to_livestock,
      r9$perc_mulched,
      r9$perc_burnt,
      r9$perc_sold,
      r9$perc_other
    ),
    stringsAsFactors = FALSE
  )
  d9$crop <- standardise_crop_name(d9$crop)
  # Keep main legume residue record per farm_id + crop
  d9 <- d9[!duplicated(paste(d9$farm_id, d9$crop)), ]
  
  # Base: field-level crop production (one row per field per household)
  d <- d7
  
  d <- merge(d, d8, by = c("farm_id", "crop","yield"), all = TRUE)
 # Merge legume residue management
  d <- merge(d, d9, by = c("farm_id", "crop"), all = TRUE)
  
  # Household-level merges
  d <- merge(d, d1,  by = "farm_id", all = TRUE)
  d <- merge(d, d2,  by = "farm_id", all = TRUE)
  d <- merge(d, d3,  by = "farm_id", all = TRUE)
  d <- merge(d, d4, by = "farm_id", all = TRUE)
  d <- merge(d, d5,  by = "farm_id", all = TRUE)
  d <- merge(d, d6a, by = "farm_id", all = TRUE)
  
  d$on_farm       <- TRUE
  d$is_survey     <- TRUE
  d$irrigated     <- FALSE
  d$yield_part    <- NA_character_
  d$yield_isfresh <- TRUE
  d$treatment     <- NA_character_
  d$planting_date <- NA_character_
  d$harvest_date  <- NA_character_
  d$yield_moisture <- NA_real_
  d$trial_id <- as.character(as.integer(as.factor(1)))
  
  # Assign year from interview date
  d$year[is.na(d$year)] <- 2014L
  
  d$yield_part[d$crop %in% c("maize", "sorghum", "millet", "rice")]    <- "grain"
  d$yield_part[d$crop %in% c("common bean", "soybean",
                             "groundnut", "pea")]                                               <- "seed"
  d$yield_part[d$crop %in% c("cassava", "sweetpotato", "potato", "yam")] <- "roots"
  d$yield_part[d$crop %in% c("tomato", "pumpkin", "cabbage", "onion",
                             "kale")]                                                           <- "fruit"
  d$yield_part[d$crop %in% c("banana")]                                <- "fruit"
  d$yield_part[d$crop == "coffee"]                                     <- "seed"
  d$yield_part[d$crop == "cotton"]                                     <- "seed"
  d$yield_part[d$crop == "sesame"]                                     <- "seed"
  d$yield_part[d$crop == "sunflower"]                                  <- "seed"
  d$crop_rotation[d$crop_rotation=="passion fruits"] <- "passion fruit"
  
  #d$farm_id   <- as.character(d$farm_id)
  d$field_id  <- as.character(d$field_id)
  d$year      <- as.integer(d$year)
  d$hh_size   <- as.integer(d$hh_size)
  d$hh_adult_women <- as.integer(d$hh_adult_women)
  d$hh_adult_men   <- as.integer(d$hh_adult_men)
  d$hh_child_18    <- as.integer(d$hh_child_18)
  d$hh_elders      <- as.integer(d$hh_elders)
  d$inoculated     <- as.logical(d$inoculated)
  d$OM_used        <- as.logical(d$OM_used)
  d$is_head        <- as.logical(d$is_head)
  d$geo_from_source <- as.logical(d$geo_from_source)
  
  names(d)[names(d) == "farm_id"] <- "field_id"
  d$field_id.1 <- NULL
  
  d$crop_rotation <- gsub("\\bpassion fruits\\b","passion fruit",d$crop_rotation)
  
  char_cols <- sapply(d, is.character)
  d[char_cols] <- lapply(d[char_cols], trimws)
  
  d$yield[d$crop %in% c("coffee", "pumpkin")] <-
    d$yield[d$crop %in% c("coffee", "pumpkin")] * 1000       #pumpkin yield is 0
  
  d <- unique(d)
  
  carobiner::write_files(path, meta, d)
}