

"
GPS coordinates recorded only for Kanungu action site in source data. Coordinates for remaining action sites (Kabale, Kapchorwa, Kibuku, Kole, Oyam, Palisa) assigned from known district centroids; geo_from_source set to FALSE for these records. Yield in f_crop_production_1 is the total field harvest in kg; conversion to kg/ha requires field_size which is present.

I have requested for the location data from the corresponding author and will update the script soon as they send
"

carob_script <- function(path) {
  
"
N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. This dataset covers the Uganda baseline survey conducted in 2014 across even action sites: Kabale, Kanungu, Kapchorwa, Kibuku, Kole, Oyam, and Palisa.
"
  
  uri   <- "doi:10.25502/wcbr-ck09/d"
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
      carob_date = "2026-06-26",
      carob_effort = 6,
      notes = NA,
      design = NA
  )
  

  f1 <- ff[basename(ff) == "a1_a3_demographic.csv"]
  r1 <- read.csv(f1)
  
  f2 <- ff[basename(ff) == "a4_demographic.csv"]
  r2 <- read.csv(f2)
  
  f3 <- ff[basename(ff) == "c_labour.csv"]
  r3 <- read.csv(f3)
  
  f4 <- ff[basename(ff) == "d_livestock_ownership.csv"]
  r4 <- read.csv(f4)
  
  f5 <- ff[basename(ff) == "e_land_use.csv"]
  r5 <- read.csv(f5)
  
  f6 <- ff[basename(ff) == "e3_land_use_rotation.csv"]
  r6 <- read.csv(f6)
  
  f7 <- ff[basename(ff) == "f_crop_production.csv"]
  r7 <- read.csv(f7)
  
  f8 <- ff[basename(ff) == "g1_legume_utilisation.csv"]
  r8 <- read.csv(f8)
  
  f9 <- ff[basename(ff) == "g2_legume_utilisation.csv"]
  r9 <- read.csv(f9)
  
  standardise_crop <- function(x) {
    x <- trimws(tolower(x))
    x[grepl("^maize$|^corn$|^mais$", x)]                          <- "maize"
    x[grepl("^cassava$|^manioc$", x)]                             <- "cassava"
    x[grepl("^yam$|^igname$|^kratchi|^katara|^lotossou", x)]      <- "yam"
    x[grepl("^cowpea$", x)]           <- "cowpea"
    x[grepl("^soybean$|^soya$", x)]                               <- "soybean"
    x[grepl("^groundnut$|^arachide$", x)]                         <- "groundnut"
    x[grepl("^rice$", x)]             <- "rice"
    x[grepl("cajanus|pigeon pea", x)]                             <- "pigeon pea"
    x[grepl("mucuna|Mucuna pluriens", x)]                         <- "velvet bean"
    x[grepl("^sweet pot|^sweet pat", x)]                          <- "sweetpotato"
    x[grepl("^taro$|^cocoyam$", x)]                               <- "taro"
    x[grepl("^banana$|^plantain$", x)]                            <- "banana"
    x[grepl("^tomato$|^tomate$|^tomoto$", x)]                     <- "tomato"
    x[grepl("^pepper$|^pipper$", x)]                              <- "pepper"
    x[grepl("^eggplant$|^garden egg$|garden eggs", x)]           <- "eggplant"
    x[grepl("^okra$|^gombo$|^okro$|^occro$", x)]                 <- "okra"
    x[grepl("^cabbage$", x)]          <- "cabbage"
    x[grepl("^lettuce$", x)]          <- "lettuce"
    x[grepl("^green bean$|^kidney bean$", x)]                     <- "common bean"
    x[grepl("^cocoa$", x)]            <- "cocoa"
    x[grepl("^coffee$|^cofee$", x)]                               <- "coffee"
    x[grepl("^palm oil$|^oil palm$", x)]                          <- "oilpalm"
    x[grepl("^rubber$", x)]           <- "rubber"
    x[grepl("^sugarcane$|^sugar cane", x)]                        <- "sugarcane"
    x[grepl("eucalyptus|teck|techtona|techtonas", x)]             <- "eucalyptus"
    x[grepl("^orange", x)]            <- "orange"
    x[grepl("^avocado$", x)]          <- "avocado"
    x[grepl("^sorghum$", x)]          <- "sorghum"
    x[grepl("^fallow$", x)]           <- NA
    x[x %in% c("", "na", "NA")]       <- NA
	x[x %in% c("pepper and tomato", "tomato and pepper")] <- "tomato;chili pepper"
    x
  }
  
  d1 <- data.frame(
    field_id  = r1$farm_id,
    sex      = trimws(tolower(r1$gender_household_head)),
    hh_size  = suppressWarnings(
      as.numeric(r1$household_number_of_adults) +
        as.numeric(r1$household_number_of_children)),
    hh_adult_men   = suppressWarnings(NA_integer_),
    hh_adult_women = suppressWarnings(NA_integer_),
    hh_child_18    = suppressWarnings(as.integer(r1$household_number_of_children)),
    stringsAsFactors = FALSE
  )
  
  d1$sex[d1$sex == "m"] <- "male"
  d1$sex[d1$sex == "f"] <- "female"
  d1$sex[d1$sex == ""] <- NA
  d1$hh_size[d1$hh_size < 0 | d1$hh_size > 50] <- NA
  
  hd <- r2[r2$relation_to_household_member == "household head", ]
  # Take first record per farm_id if there are duplicates
  hd <- hd[!duplicated(hd$farm_id), ]
  
  d2 <- data.frame(
    field_id   = hd$farm_id,
    age       = suppressWarnings(as.numeric(hd$age)),
    education = trimws(tolower(hd$highest_education_level)),
    stringsAsFactors = FALSE
  )
  d2$age[d2$age < 12 | d2$age > 100] <- NA
  d2$education[d2$education == ""] <- NA
  
  d3 <- data.frame(
    field_id           = r3$farm_id,
    farm_labour_hired = trimws(r3$labour_hired) == "y",
    stringsAsFactors  = FALSE
  )
  
  d4 <- data.frame(
    field_id   = r4$farm_id,
    animal = trimws(tolower(r4$livestock)),
    heads = r4$number_owned,
    stringsAsFactors = FALSE
  )
  
  d4$animal[d4$animal == ""]             <- NA
  d4$animal[grepl("^goats?$", d4$animal)]          <- "goat"
  d4$animal[d4$animal == "pigs"]                   <- "pig"
  d4$animal[d4$animal == "sheep"]                  <- "sheep"
  d4$animal[grepl("^ducks?$", d4$animal)]          <- "duck"
  d4$animal[d4$animal == "chicken"]                <- "chicken"
  d4$animal[d4$animal == "rabbits"]                <- "rabbit"
  d4$animal[grepl("^dogs?$", d4$animal)]           <- "dog"
  d4$animal[grepl("guinea fowls?", d4$animal)]     <- "guinea fowl"
  d4$animal[d4$animal == "guinea pigs"]            <- "guinea pig"
  d4$animal[d4$animal == "pigeon"]                 <- "pigeon"
  d4$animal[d4$animal == "oxen"]                   <- "cattle"
  d4$animal[grepl("agouti|rat\\(zool\\)", d4$animal)] <- NA
  d4$animal[grepl("^cats?$", d4$animal)]           <- NA
  
  d5 <- data.frame(
    field_id      = r5$farm_id,
    land_fallow  = r5$is_land_left_fallow_during_cropping_season == "y",
    stringsAsFactors = FALSE
  )
  
  r6_long <- do.call(rbind, lapply(seq_len(nrow(r6)), function(i) {
    row    <- r6[i, ]
    crops  <- c(row$principle_crop, row$second_crop, row$third_crop)
    crops  <- unique(trimws(crops[crops != "" & !is.na(crops)]))
    crops  <- standardise_crop(crops)
    crops  <- crops[!is.na(crops)]
    if (length(crops) == 0) return(NULL)
    data.frame(field_id = row$farm_id, crop = crops, stringsAsFactors = FALSE)
  }))
  
  if (!is.null(r6_long)) {
    d6 <- aggregate(
      crop ~ field_id,
      data = r6_long,
      FUN  = function(x) paste(unique(x), collapse = ";")
    )
    names(d6)[2] <- "crop_rotation"
  } else {
    d6 <- data.frame(farm_id = character(0), crop_rotation = character(0))
  }
  
  standardise_crop_name <- function(x) {
    x <- trimws(tolower(x))
    x <- gsub("\n", ";", x)
    x[grepl("^maize", x)]       <- "maize"
    x[grepl("sorghum", x)]      <- "sorghum"
    x[grepl("millet", x)]       <- "millet"
    x[grepl("rice", x)]         <- "rice"
    x[grepl("cassava", x)]      <- "cassava"
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
    x[grepl("coffee", x)]      <- "coffee"
    x[grepl("cotton", x)]      <- "cotton"
    x[grepl("cabbage", x)]                                 <- "cabbage"
    x[grepl("tomato", x)]      <- "tomato"
    x[grepl("onion", x)]       <- "onion"
    x[grepl("pumpkin", x)]                                 <- "pumpkin"
    x[grepl("yam", x)]         <- "yam"
    x[grepl("sukuma|kale", x)]                             <- "kale"
    x[grepl("egg plants", x)]                             <- "eggplant"
    x[grepl("fruits|trees", x)]                           <- "fruits"
    x[grepl("vegetables", x)]                             <- "vegetables"
    x[x %in% c("", " ","dodo","grass")] <- NA
    x
  }
  
  d7 <- data.frame(
    field_id         = r7$farm_id,
    crop            = standardise_crop(r7$crop),
    field_size      = suppressWarnings(as.numeric(r7$area_ha)),
    OM_used         = trimws(r7$animal_manure_applied) == "y" |
      trimws(r7$other_organic_input) == "y",
    # Combine other_organic_input_type and animal_manure_type into one OM_type
    # field separated by ";" where both are present
    OM_type = {
      a   <- trimws(r7$other_organic_input_type)
      b   <- trimws(r7$animal_manure_type)
      out <- ifelse(a != "" & b != "", paste(a, b, sep = ";"),
                    ifelse(a != "", a, b))
      ifelse(out == "", NA_character_, out)
    },
    fertilizer_type = trimws(r7$mineral_fert_type),
    stringsAsFactors = FALSE
  )
  
  # field_size: area_ha is populated for all rows (no need for acre fallback here)
  d7$field_size[d7$field_size <= 0] <- NA
  
  # Standardise OM type
  d7$OM_type <- tolower(d7$OM_type)
  d7$OM_type[grepl("crop residue|harvest residue", d7$OM_type)] <- "crop residue"
  d7$OM_type[grepl("domestic waste|household waste|waste manager", d7$OM_type)] <- "domestic waste"
  d7$OM_type[grepl("cajanus|fertilizing plant", d7$OM_type)] <- "green manure"
  d7$OM_type[grepl("goa|sheep|pig|oxen",d7$OM_type)] <- "farmyard manure"
  d7$OM_type[grepl("chicken|poultry|duck|guinea|poultry",d7$OM_type)] <- "poultry manure"
  
  # Where crop residue AND domestic waste co-occur, keep as compound
  d7$OM_type[grepl("crop residue.*domestic|domestic.*crop residue", d7$OM_type)] <-
    "crop residue;domestic waste"
  
  # Standardise fertilizer type
  d7$fertilizer_type <- tolower(trimws(d7$fertilizer_type))
  d7$fertilizer_type[d7$fertilizer_type %in% c("", "n", "s")] <- NA
  # All variants are NPK 15-15-15 with or without urea
  d7$fertilizer_type[grepl("npk.*urea|urea.*npk", d7$fertilizer_type)] <- "NPK;urea"
  d7$fertilizer_type[grepl("^npk", d7$fertilizer_type) &
                       !grepl("urea", d7$fertilizer_type)]             <- "NPK"
  d7$fertilizer_type[grepl("^urea$", d7$fertilizer_type)]              <- "urea"
  
  
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
  
  legume_crops <- c("cowpea", "soybean", "groundnut", "bean", "pigeonpea",
                    "mucuna", "pea")
  farm_legume_crop <- d7[d7$crop %in% legume_crops & !is.na(d7$crop),
                         c("field_id", "crop", "field_size")]
  # Use mean field size across legume plots per farm
  farm_legume_size <- aggregate(
    field_size ~ field_id,
    data = farm_legume_crop[!is.na(farm_legume_crop$field_size), ],
    FUN  = mean
  )
  # Dominant legume crop per farm (first if multiple)
  farm_legume_name <- farm_legume_crop[!duplicated(farm_legume_crop$farm_id),
           c("field_id", "crop")]
  
  d8 <- data.frame(
    field_id        = r8$farm_id,
    yield = suppressWarnings(as.numeric(r8$total_prod_most_recent_season_kg)),
    amount_sold_kg = suppressWarnings(as.numeric(r8$amount_used_for_sale)),
    stringsAsFactors = FALSE
  )
  
  # Merge crop name and field size
  d8 <- merge(d8, farm_legume_name, by = "field_id", all.x = TRUE)
  d8 <- merge(d8, farm_legume_size, by = "field_id", all.x = TRUE)
  
  d9 <- data.frame(
    field_id  = r9$farm_id,
    previous_crop_residue_management = trimws(r9$haulms_usage),
    stringsAsFactors = FALSE
  )
  d9$previous_crop_residue_management[
    d9$previous_crop_residue_management == ""] <- NA
  
  # Standardise haulm usage to CAROB conventions
  d9$previous_crop_residue_management <- tolower(
    d9$previous_crop_residue_management)
  d9$previous_crop_residue_management[
    grepl("incorporated", d9$previous_crop_residue_management)] <- "incorporated"
  d9$previous_crop_residue_management[
    grepl("^burnt$|^burned", d9$previous_crop_residue_management)] <- "burned"
  d9$previous_crop_residue_management[
    grepl("burned in field|cutting.*burn|burn.*cutting",
          d9$previous_crop_residue_management)] <- "burned"
  d9$previous_crop_residue_management[
    grepl("composted", d9$previous_crop_residue_management)] <- "incorporated as compost"
  d9$previous_crop_residue_management[
    grepl("feed for own livestock", d9$previous_crop_residue_management)] <- "fed to livestock"
  d9$previous_crop_residue_management[
    grepl("left in the field", d9$previous_crop_residue_management)] <- "left on field"
  d9$previous_crop_residue_management[
    grepl("give away|gift|donation", d9$previous_crop_residue_management)] <- "given away"
  # Remaining categories (firewood, cutting, wood heater, soup) = removed
  d9$previous_crop_residue_management[
    grepl("firewood|cutting|wood heater|slip|soup",
          d9$previous_crop_residue_management)] <- "removed (other use)"
  
  #unique_farms <- unique(c(r1$farm_id))
 
  d <- d7
  
  # Merge legume yield into base (match on farm_id + crop)
  d <- merge(d, d8[, c("field_id", "crop", "yield", "amount_sold_kg")],
             by = c("field_id", "crop"), all = TRUE)
  
  # Merge residue management — household level (no crop key in g2)
  d <- merge(d, d9, by = "field_id", all = TRUE)
  
  # Household-level merges
  d <- merge(d, d1,  by = "field_id", all = TRUE)
  d <- merge(d, d2,  by = "field_id", all = TRUE)
  d <- merge(d, d3,  by = "field_id", all = TRUE)
  d <- merge(d, d4, by = "field_id", all = TRUE)
  d <- merge(d, d5,  by = "field_id", all = TRUE)
  d <- merge(d, d6,  by = "field_id", all = TRUE)
  #d <- merge(d, loc, by = "field_id", all = TRUE)
  
  d$on_farm        <- TRUE
  d$is_survey      <- TRUE
  d$irrigated      <- FALSE
  d$yield_part     <- NA_character_
  d$yield_isfresh  <- TRUE
  d$treatment      <- NA_character_
  d$planting_date  <- NA_character_
  d$harvest_date   <- NA_character_
  d$yield_moisture <- NA_real_
  d$year           <- 2012L
  d$N_fertilizer   <- NA_real_
  d$P_fertilizer   <- NA_real_
  d$K_fertilizer   <- NA_real_
  d$country <- "Togo"
  d$longitude <- NA
  d$latitude  <- NA
  d$geo_from_source <- FALSE
  d$trial_id       <- as.character(as.integer(as.factor(1)))
  
  d$yield_part[d$crop %in% c("maize", "rice", "sorghum")]                     <- "grain"
  d$yield_part[d$crop %in% c("cowpea", "soybean", "groundnut", "bean",
                             "pigeon pea")]                                 <- "seed"
  d$yield_part[d$crop %in% c("cassava", "sweetpotato", "taro", "yam")]        <- "roots"
  d$yield_part[d$crop %in% c("tomato", "pepper", "eggplant", "okra",
                             "cabbage", "lettuce")]                        <- "fruit"
  d$yield_part[d$crop %in% c("banana", "avocado", "orange")]                  <- "fruit"
  d$yield_part[d$crop %in% c("cocoa", "coffee")]                              <- "seed"
  d$yield_part[d$crop == "oilpalm"]               <- "fruit"
  d$yield_part[d$crop %in% c("mucuna")]           <- "seed"
  
  d$field_id         <- as.character(d$field_id)
  d$hh_size         <- as.integer(d$hh_size)
  d$hh_child_18     <- as.integer(d$hh_child_18)
  d$farm_labour_hired <- as.logical(d$farm_labour_hired)
  d$geo_from_source <- as.logical(d$geo_from_source)
  d$OM_used         <- as.logical(d$OM_used)
  
  # Fix crop rotation values
  d$crop_rotation <- gsub("\\boilpalm\\b", "oil palm", d$crop_rotation)
  
  d$crop_rotation <- gsub("\\bgoundnut\\b", "groundnut", d$crop_rotation)
  d$crop_rotation <- gsub("\\bgroudnut\\b", "groundnut", d$crop_rotation)
  d$crop_rotation <- gsub("\\bgroungnut\\b", "groundnut", d$crop_rotation)
  
  d$crop_rotation <- gsub("\\bplantain banana\\b", "banana", d$crop_rotation)
  d$crop_rotation <- gsub("\\bplantian\\b", "banana", d$crop_rotation)
  
  d$crop_rotation <- gsub("\\bgboma\\b", "eggplant", d$crop_rotation)
  d$crop_rotation <- gsub("\\baubergine\\b", "eggplant", d$crop_rotation)
  d$crop_rotation <- gsub("\\bgarding eggs\\b", "eggplant", d$crop_rotation)
  
  d$crop_rotation <- gsub("\\bcassia\\b", "cinnamon", d$crop_rotation)
  
  d$crop_rotation <- gsub("\\bpeanut\\b", "rhizoma peanut", d$crop_rotation)
  
  #fix animal names
  d$animal[d$animal %in% c("cat","dog")] <- NA
  
  #fix OM_type
  d$OM_type[d$OM_typ %in% c("crop residue","crops residues","domestic waste","domestic wates and crops residues",
                            "domestid wates and crops residues","domesyic wastes and crops residues")] <- "farmyard manure"
  #fix crop
  d$crop[d$crop == "cocoa and orange"] <- "orange"
  d$crop[d$crop == "oilpalm"] <- "oil palm"
  d$crop[d$crop %in% c("yam(kratchi,katara )","yam: kratchi,katara and lotossou")] <- "yam"
  
  char_cols <- sapply(d, is.character)
  d[char_cols] <- lapply(d[char_cols], trimws)
  
  d <- unique(d)
  
  carobiner::write_files(path, meta, d)
}