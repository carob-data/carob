# R script for "carob"
# license: GPL (>=3)

#production.csv file (standardized as dd) describes the farmer's *other* fields/crop rotations (field survey) (outside the N2Africa trial plot) and is intentionally NOT merged into d -- see note near `dd`

carob_script <- function(path) {

  "N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. This dataset covers an N2Africa diagnostic trial (control/PK/NPK/SYMPAL fertilizer treatments on common bean) in northern Tanzania (Lushoto and Moshi Rural districts), 2014 season II, together with household demographics, livestock composition, and farm production context."

  uri   <- "doi:10.25502/f46k-yh57"
  group <- "survey"

  ff <- carobiner::get_data(uri, path, group)

  meta <- carobiner::get_metadata(uri, path, group,
                                  major = NA, minor = NA,
                                  data_organization = "IITA",
                                  publication = NA,
                                  project = "N2Africa",
                                  data_type = "survey",
                                  treatment_vars = "fertilizer_used;fertilizer_amount",
                                  response_vars = "yield",
                                  carob_completion = 100,
                                  carob_contributor = "Mitchelle Njukuya",
                                  carob_date = "2026-07-14",
                                  carob_effort = 3,
                                  notes = NA,
                                  design = NA
  )

  f1 <- ff[basename(ff) == "general.csv"]
  r1 <- read.csv(f1, check.names = FALSE, stringsAsFactors = FALSE)

  f2 <- ff[basename(ff) == "land_crops_and_livestock.csv"]
  r2 <- read.csv(f2, check.names = FALSE, stringsAsFactors = FALSE)

  f3 <- ff[basename(ff) == "experiment.csv"]
  r3 <- read.csv(f3, check.names = FALSE, stringsAsFactors = FALSE)

  f4 <- ff[basename(ff) == "production.csv"]
  r4 <- read.csv(f4, check.names = FALSE, stringsAsFactors = FALSE)

  yn  <- function(x) tolower(trimws(as.character(x))) %in% "y"
  chr <- function(x) { x <- trimws(as.character(x)); x[x %in% c("", "NA")] <- NA; x }

  ## build an ISO (yyyy-mm-dd) date string from separate dd/mm/yyyy
  ## components. mm may arrive as a month name (e.g. "December") or a number.
  build_date <- function(dd, mm, yyyy) {
    dd   <- trimws(as.character(dd))
    mm   <- trimws(as.character(mm))
    yyyy <- trimws(as.character(yyyy))
    mm_num <- (as.numeric(mm))
    mm_num[is.na(mm_num)] <- match(tolower(mm[is.na(mm_num)]), tolower(month.name))
    dd_num   <- (as.numeric(dd))
    yyyy_num <- (as.numeric(yyyy))
    ifelse(is.na(dd_num) | is.na(mm_num) | is.na(yyyy_num), NA_character_,
           sprintf("%04d-%02d-%02d", yyyy_num, mm_num, dd_num))
  }

  standardise_crop <- function(x) {
    x <- trimws(tolower(x))
    x[grepl("^bean$|^beans$|^bush bean$|^bush beans$|^climbing bean$|^climbing beans$", x)] <- "common bean"
    x[grepl("^cabbage$", x)]          <- "cabbage"
    x[grepl("^carrot$|^carrots$", x)] <- "carrot"
    x[grepl("^cassava$", x)]          <- "cassava"
    x[grepl("^groundnut$|^groundnuts$", x)] <- "groundnut"
    x[grepl("^vegetable$|^vegetables$", x)] <- "vegetable"
    x[grepl("^fruit$|^fruits$", x)]   <- "fruit"
    x[grepl("^pigeonpea$|^pigeon pea$", x)] <- "pigeon pea"
    x[grepl("^potato$|^potatoes$", x)] <- "potato"
    x[grepl("^sunflower$", x)]        <- "sunflower"
    x[grepl("^maize$", x)]            <- "maize"
    x[x %in% c("", "na")] <- NA
    x
  }

  ## experiment_id: fix inconsistent capitalisation/spelling. "Bean
  ## diagnostic" is a spelling variant of "diagnostic trial" (same trial);
  ## the numbered "diagnostic plots N" labels are kept distinct.
  standardise_experiment_id <- function(x) {
    x <- tolower(trimws(x))
    x[x == "bean diagnostic"] <- "diagnostic trial"
    x[x == ""] <- NA
    x
  }

  ## locate a crop x plot harvest column even when the raw spelling is
  ## inconsistent between plots (e.g. "grain_weight_kg_shelled_grain..."
  ## vs "grain_weight_shelled_grain...").
  find_col <- function(df, prefix_regex, crop, plot) {
    suffix_regex <- paste0("crop_", crop, "_plot_", plot, "($|\\$)")
    hits <- grep(prefix_regex, names(df), value = TRUE)
    hits <- grep(suffix_regex, hits, value = TRUE)
    if (length(hits) == 0) return(NA_character_)
    hits[1]
  }

  d1 <- data.frame(
    field_id = r1$farm_id,
    country  = "Tanzania",
    adm1     = tolower(trimws(r1$district)),
    adm2     = tolower(trimws(r1$sector_ward)),
    adm3     = tolower(trimws(r1$village)),
    stringsAsFactors = FALSE
  )

  ## GPS: prefer field-level coordinates, fall back to household-level
  ## coordinates when the field-level value is missing
  lat_field <- (as.numeric(r1$gps_latitude_field))
  lon_field <- (as.numeric(r1$gps_longitude_field))
  elv_field <- (as.numeric(r1[["gps_altitude_field$m"]]))
  lat_hh    <- (as.numeric(r1[["gps_latitude_hh$decimal_degrees"]]))
  lon_hh    <- (as.numeric(r1[["gps_longitude_hh$decimal_degrees"]]))
  elv_hh    <- (as.numeric(r1[["gps_altitude_hh$m"]]))
  d1$latitude  <- -abs(ifelse(!is.na(lat_field), lat_field, lat_hh))
  d1$longitude <- ifelse(!is.na(lon_field), lon_field, lon_hh)
  d1$elevation <- ifelse(!is.na(elv_field), elv_field, elv_hh)
  d1$geo_from_source <- TRUE

  ## three distinct dates kept separate rather than collapsed into one
  ## "date" field: household-survey date, field-survey date
  d1$date_hh    <- build_date(r1[["date_hhsurvey_dd$days"]], r1[["date_hhsurvey_mm$months"]],
                               r1[["date_hhsurvey_yyyy$years"]])
  d1$date_field <- build_date(r1$date_field_survey_dd, r1$date_field_survey_mm,
                               r1$date_field_survey_yyyy)
  hd1 <- build_date(r1$date_harvest_dd_technician_1, r1$date_harvest_mm_technician_1,
                     r1$date_harvest_yyyy_technician_1)
  hd2 <- build_date(r1$date_harvest_dd_technician_2, r1$date_harvest_mm_technician_2,
                     r1$date_harvest_yyyy_technician_2)
  hd3 <- build_date(r1$date_harvest_dd_technician_3, r1$date_harvest_mm_technician_3,
                     r1$date_harvest_yyyy_technician_3)
  d1$harvest_date <- ifelse(!is.na(hd1), hd1, ifelse(!is.na(hd2), hd2, hd3))

  d2 <- data.frame(field_id = r1$farm_id, stringsAsFactors = FALSE)

  sex <- tolower(trimws(r1$gender_of_farmer))
  sex[sex == "m"] <- "male"
  sex[sex == "f"] <- "female"
  sex[!(sex %in% c("male", "female"))] <- NA
  d2$sex     <- sex
  d2$age     <- (as.numeric(r1[["age_of_farmer$years"]]))
  d2$is_head <- trimws(r1$farmer_hh_head) == "Y"

  d2$hh_adult_women <- (as.numeric(r1$no_females_17_35_yrs)) +
                        (as.numeric(r1$no_females_36_60_yrs))
  d2$hh_adult_men   <- (as.numeric(r1$no_males_17_35_yrs)) +
                        (as.numeric(r1$no_males_36_60_yrs))
  d2$hh_child_18    <- (as.numeric(r1$no_females_0_16_yrs)) +
                        (as.numeric(r1$no_males_0_16_yrs))
  d2$hh_elders      <- (as.numeric(r1$no_females_over_60_yrs)) +
                        (as.numeric(r1$no_males_over_60_yrs))
  d2$hh_size <- rowSums(cbind(d2$hh_adult_women, d2$hh_adult_men,
                              d2$hh_child_18, d2$hh_elders), na.rm = TRUE)

  edu_levels <- c("primary", "secondary", "post_secondary", "university", "other")
  edu_cols <- c("education_hh_head_yrs_primary", "education_hh_head_yrs_secondary",
                "education_hh_head_yrs_post_secundary", "education_hh_head_yrs_university",
                "education_hh_head_yrs_other")
  edu_mat <- sapply(edu_cols, function(cn) (as.numeric(r1[[cn]])))
  edu_mat[is.na(edu_mat)] <- 0
  d2$education <- apply(edu_mat, 1, function(r) {
    hit <- which(r > 0)
    if (length(hit) == 0) return(NA_character_)
    edu_levels[max(hit)]
  })

  livestock_cols <- list(cattle = "no_cattle", sheep = "no_sheep", goat = "no_goats",
                          pig = "no_pigs", poultry = "no_poutry")
  d3_list <- lapply(names(livestock_cols), function(a) {
    data.frame(field_id = r2$farm_id, animal = a,
               heads = (as.numeric(r2[[livestock_cols[[a]]]])),
               stringsAsFactors = FALSE)
  })
  other1 <- data.frame(field_id = r2$farm_id, animal = tolower(trimws(r2$spec_livestock_other1)),
                        heads = (as.numeric(r2$no_livestock_other1)),
                        stringsAsFactors = FALSE)
  other2 <- data.frame(field_id = r2$farm_id, animal = tolower(trimws(r2$spec_livestock_other2)),
                        heads = (as.numeric(r2$no_livestock_other2)),
                        stringsAsFactors = FALSE)
  d3 <- do.call(rbind, c(d3_list, list(other1, other2)))
  d3$animal[d3$animal == ""]      <- NA
  d3$animal[d3$animal == "ducks"] <- "duck"
  d3$animal[d3$animal == "dogs"]  <- NA  # dogs are not livestock -- excluded
  d3 <- d3[!is.na(d3$animal) & !is.na(d3$heads), ]

  ## d4: household main crops (land_crops_and_livestock.csv) -- these are
  ## the farmer's OTHER crops, separate from the trial's bush bean plots.
  ## Kept as additional observation rows in d (not merged as columns)
  crop_cols <- list(
    list(name = "main_crop_1", yield = "yield_main_crop_1", yield_unit = "yield_main_crop_1_unit",
         area = "area_main_crop_1", area_unit = "area_main_crop_1_unit$ha"),
    list(name = "main_crop_2", yield = "yield_main_crop_2", yield_unit = "yield_main_crop_2_unit$kg_per_ha",
         area = "area_main_crop_2", area_unit = "area_main_crop_2_unit$ha"),
    list(name = "main_crop_3", yield = "yield_main_crop_3", yield_unit = "yield_main_crop_3_unit$kg_per_ha",
         area = "area_main_crop_3", area_unit = "area_main_crop_3_unit$ha")
  )
  d4_list <- lapply(crop_cols, function(cc) {
    yield_kg  <- (as.numeric(r2[[cc$yield]]))
    area_raw  <- (as.numeric(r2[[cc$area]]))
    area_unit <- tolower(trimws(r2[[cc$area_unit]]))
    ## convert area to ha, then yield to kg/ha, so this lines up with the
    ## trial-plot yield unit computed below.
    area_ha <- ifelse(grepl("^acres?$", area_unit), area_raw * 0.404686, area_raw)
    yield_kg_ha <- ifelse(!is.na(yield_kg) & !is.na(area_ha) & area_ha > 0,
                           yield_kg / area_ha, NA_real_)
    data.frame(
      field_id  = r2$farm_id,
      crop      = standardise_crop(r2[[cc$name]]),
      yield     = yield_kg_ha,
      treatment = NA_character_,
      is_survey = TRUE,
      on_farm   = TRUE,
      stringsAsFactors = FALSE
    )
  })
  d4 <- do.call(rbind, d4_list)
  d4 <- d4[!is.na(d4$crop), ]

  ## d5: trial plots (experiment.csv) -- one row per plot per household.
  ## Every household in this trial grew only "bush bean" as crop_1;
  ## crop_2 is never used anywhere in the raw data (all crop_2 harvest /
  ## crop_2_y_n columns are entirely blank) -- confirmed by inspection.
  ## So this is treated as a single-crop trial, worked plot-by-plot
  ## (crop_1 only) rather than crop x plot.
  plot_index <- 1:8
  d5_list <- lapply(plot_index, function(p) {
    col_width <- find_col(r3, "^width_of_harvested_plot", 1, p)
    col_depth <- find_col(r3, "^depth_of_harvested_plot", 1, p)
    col_rows  <- find_col(r3, "^number_of_rows_in_plot", 1, p)
    col_grain <- find_col(r3, "^grain_weight", 1, p)
    col_bio   <- find_col(r3, "^above_ground_biomass", 1, p)
    
    plot_width   <- (as.numeric(r3[[col_width]]))
    plot_depth   <- (as.numeric(r3[[col_depth]]))
    grain_kg     <- (as.numeric(r3[[col_grain]]))
    fwy_total    <- (as.numeric(r3[[col_bio]]))
    plot_area_m2 <- plot_width * plot_depth
    yield_kg_ha  <- ifelse(!is.na(grain_kg) & !is.na(plot_area_m2) & plot_area_m2 > 0,
                            grain_kg / plot_area_m2 * 10000, NA_real_)

    treatment_label <- tolower(chr(r3[[paste0("name_treatment_", p)]]))

    data.frame(
      field_id      = r3$farm_id,
      experiment_id = standardise_experiment_id(r3$experiment_id),
      plot_id       = as.character(p),
      crop          = "common bean",
      treatment     = treatment_label,
      fertilizer_used = treatment_label,
      treatment_description = chr(r3[[paste0("description_treatment_", p)]]),
      germination   = chr(r3[[paste0("germination_crop_1_treatment_", p)]]),

      ## relative_performance_plot_* (labour/weeds/pests_diseases/yield/why)
      ## is entirely blank for every household in this dataset 
      weed_severity = NA_character_,
      pest_severity = NA_character_,
      plot_labour   = NA_real_,

      plot_width    = plot_width,
      #number_of_rows_in_plot = (as.numeric(r3[[col_rows]])),
      yield         = yield_kg_ha,
      fwy_total     = fwy_total,

      ## N/P/K left NA -- raw data give kg of product (fertilizer_amount)
      ## and product names (NPK/PK/SYMPAL) but not nutrient composition, so
      ## exact N/P/K rates cannot be computed without fabricating a
      ## composition.
      N_fertilizer = NA_real_,
      P_fertilizer = NA_real_,
      K_fertilizer = NA_real_,
      
      ## fertilizer_amount = total kg of mineral fertilizer product(s)
      ## applied to the plot, summed across up to 3 products
      ## (fert_1/2/3_kg in the raw data).
      fertilizer_amount = rowSums(cbind(
        (as.numeric(r3[[paste0("fert_1_kg_plot_plot_", p, "$kg_per_plot")]])),
        (as.numeric(r3[[paste0("fert_2_kg_plot_plot_", p, "$kg_per_plot")]])),
        (as.numeric(r3[[paste0("fert_3_kg_plot_plot_", p, "$kg_per_plot")]]))
      ), na.rm = TRUE),
      
      inoculant_y_n = yn(r3[[paste0("inoculant_y_n_plot_", p)]]),
      OM_amount     = (as.numeric(r3[[paste0("manure_kg_plot_plot_", p, "$kg_per_plot")]])),
      crop_1_y_n    = yn(r3[[paste0("crop_1_y_n_plot_", p)]]),
      plant_density = (as.numeric(r3[[paste0("density_1_plot_", p)]])),

      is_survey = FALSE,
      on_farm   = TRUE,
      stringsAsFactors = FALSE
    )
  })
  d5 <- do.call(rbind, d5_list)
  keep <- d5$crop_1_y_n | (!is.na(d5$treatment) & d5$treatment != "")
  d5 <- d5[keep, ]
  d5$crop_1_y_n <- NULL
  
  d5$fertilizer_used <- ifelse(d5$fertilizer_used=="control",FALSE,TRUE)
  
  ## dd: production.csv, standardised but kept SEPARATE from d.
  ## Why: production.csv describes the farmer's OTHER fields (up to 4),
  ## their crop rotations, soil-fertility perceptions, and management --
  ## none of which are the N2Africa trial plot itself. There is no natural
  ## one-to-one key linking a production-field row to a trial-plot row in
  ## d (both are multi-row per household), so merging them would produce
  ## a spurious cross-join (every field/crop-rank combination duplicated
  ## against every trial-plot row) rather than meaningful linked data.
  
  field_index <- data.frame(field_number = 1:4)
  field_cols <- list(
    area_field                        = paste0("area_field_", 1:4),
    walking_distance_minutes          = paste0("walking_distance_field_", 1:4, "$minutes"),
    other_crops                       = paste0("other_crops_field_", 1:4),
    amount_mineral_fertilizer_applied = paste0("amount_of_mineral_fertilizer_applied_field_", 1:4),
    type_of_mineral_fertilizer        = paste0("type_of_mineral_fertilizer_field_", 1:4),
    organic_inputs_applied            = paste0("organic_inputs_applied_field_", 1:4),
    rhizobium_inoculant_applied       = paste0("rhizobium_inoculant_applied_field_", 1:4),
    perceived_fertility_of_the_field  = paste0("perceived_fertility_of_the_field_field_", 1:4)
  )
  dd_field_list <- lapply(seq_along(field_index$field_number), function(i) {
    fn <- field_index$field_number[i]
    data.frame(
      field_id      = r4$farm_id,
      field_number  = fn,
      area_field    = (as.numeric(r4[[field_cols$area_field[i]]])),
      walking_distance_minutes = (as.numeric(r4[[field_cols$walking_distance_minutes[i]]])),
      other_crops   = standardise_crop(r4[[field_cols$other_crops[i]]]),
      amount_mineral_fertilizer_applied = (as.numeric(r4[[field_cols$amount_mineral_fertilizer_applied[i]]])),
      type_of_mineral_fertilizer  = tolower(trimws(r4[[field_cols$type_of_mineral_fertilizer[i]]])),
      organic_inputs_applied      = tolower(trimws(r4[[field_cols$organic_inputs_applied[i]]])) == "y",
      rhizobium_inoculant_applied = tolower(trimws(r4[[field_cols$rhizobium_inoculant_applied[i]]])) == "y",
      perceived_fertility_of_the_field = tolower(trimws(r4[[field_cols$perceived_fertility_of_the_field[i]]])),
      stringsAsFactors = FALSE
    )
  })
  dd_field <- do.call(rbind, dd_field_list)

  crop_field_index <- expand.grid(crop_rank = 1:4, field_number = 1:4)
  rank_prefix <- c("1st", "2nd", "3d", "4th")[crop_field_index$crop_rank]
  dd_cropfield_list <- lapply(seq_len(nrow(crop_field_index)), function(i) {
    cr <- crop_field_index$crop_rank[i]
    fn <- crop_field_index$field_number[i]
    crop_col    <- paste0(rank_prefix[i], "_most_important_crop_field_", fn)
    area_col    <- paste0("area_perc_field_crop_", cr, "_field_", fn, "$percentage")
    variety_col <- paste0("variety_crop_", cr, "_field_", fn)
    data.frame(
      field_id     = r4$farm_id,
      field_number = fn,
      crop_rank    = cr,
      crop         = standardise_crop(r4[[crop_col]]),
      area_perc    = (as.numeric(r4[[area_col]])),
      variety      = trimws(r4[[variety_col]]),
      stringsAsFactors = FALSE
    )
  })
  dd_cropfield <- do.call(rbind, dd_cropfield_list)
  dd_cropfield <- dd_cropfield[!is.na(dd_cropfield$crop), ]

  dd <- merge(dd_field, dd_cropfield, by = c("field_id", "field_number"), all = TRUE)
  dd <- unique(dd)

  ## assemble d
  common_cols <- union(names(d4), names(d5))
  for (cn in setdiff(common_cols, names(d4))) d4[[cn]] <- NA
  for (cn in setdiff(common_cols, names(d5))) d5[[cn]] <- NA
  d_obs <- rbind(d4[, common_cols], d5[, common_cols])

  d <- merge(d_obs, d1, by = "field_id", all.x = TRUE)
  d <- merge(d, d2, by = "field_id", all.x = TRUE)
  d <- merge(d, d3, by = "field_id", all.x = TRUE)  

  d$trial_id      <- as.character(as.integer(as.factor(1)))
  d$irrigated     <- FALSE
  d$yield_part    <- ifelse(d$crop == "common bean", "seed",
                      ifelse(d$crop %in% c("cassava", "potato", "sweetpotato"), "roots",
                      ifelse(d$crop == "maize", "grain", NA_character_)))
  d$yield_isfresh <- NA
  d$planting_date <- NA_character_
  d$yield_moisture <- NA_real_

  d$field_id       <- as.character(d$field_id)
  d$hh_size        <- as.integer(d$hh_size)
  d$hh_adult_women <- as.integer(d$hh_adult_women)
  d$hh_adult_men   <- as.integer(d$hh_adult_men)
  d$hh_child_18    <- as.integer(d$hh_child_18)
  d$hh_elders      <- as.integer(d$hh_elders)
  d$is_head        <- as.logical(d$is_head)
  d$geo_from_source <- as.logical(d$geo_from_source)
  d$is_survey      <- as.logical(d$is_survey)
  d$on_farm        <- as.logical(d$on_farm)
  d$inoculated  <- as.logical(d$inoculant_y_n)
  d$fertilizer_type <- toupper(d$treatment)
  d$treatment <- as.character(d$treatment_description)
  d$fertilizer_type[d$fertilizer_type=="CONTROL"] <- "none"
  
  d$treatment_description <- d$inoculant_y_n <- NULL
  
  char_cols <- sapply(d, is.character)
  d[char_cols] <- lapply(d[char_cols], trimws)

  d <- unique(d)

  carobiner::write_files(path, meta, d)
}
