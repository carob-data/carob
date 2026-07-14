# R script for "carob"
# license: GPL (>=3)

## NOTES
# This appears to be an ODK-collected dataset originally designed to study
# light interception before and after pruning. This script focuses on bean
# yield, which has multiple plots per farm_id and two common bean varieties
# (local and improved) grown on the same farm, but there is no way to
# identify which plot belongs to which variety. For light interception, only
# the before-pruning state is used (pruning occurred after harvest, so
# cannot be linked to yield).


## ISSUES
# UGKAN-CBBISC -04 plot 2 has n_rows=2 (vs 12 elsewhere), giving an
# implausible 24,000 kg/ha yield; UGKAN-CBBISC -01 plot 3 has n_rows=22.
# Both kept as reported.
#
# farm_id "Ug" is truncated but has a documented complete crop failure
# (drought); yield set to 0, not NA.
#
# date_of_final_harvest placeholder "1-Jan-99" converted to NA.
#
# No crop-name column exists; crop inferred as "common bean" from the
# dataset title, "CBBISC" farm_id code, and sole_climbing_beans_in_
# experiment_this_farm (variety type: local/improved).
#
# disease: "halo blight" (common bean) has no matching terminag term.
#
# yield_shaded, yield_unshaded (kg/ha) are suggested new terminag terms:
# yield from the most/least-shaded harvest subsamples within each plot.
#
# light_interception (%) is a suggested new terminag term: 1 - below_canopy_
# reading/outside_canopy reference reading, averaged across all transect
# locations (no-pruning state only).
#
# longitude/latitude are district-level estimates (geocoded from adm2:
# Kapchorwa/Kanungu) - sector_ward values are too inconsistent (multiple
# place names per cell, mixed capitalization/punctuation) to geocode.


carob_script <- function(path) {
  
  "
N2Africa agronomy trials harvest - Uganda, 2016, II

On-farm trial data from 23 climbing bean farms in Kapchorwa and Kanungu
districts, Uganda (2016), collected as part of N2Africa. Two related
measurements are included: (1) canopy light interception, measured with a
light sensor across four transects per farm, once before and once after
pruning; and (2) plot-level grain harvest weights, for up to 6 plots per farm, comparing local and
improved bean varieties grown side-by-side (the specific plot each variety
was grown on is not identified in the data). A smaller shaded/unshaded
harvest subsample was also taken within some plots, to relate light
interception to yield loss from shading. Only 5 of the 23 farms have
complete harvest weights; one farm's crop failed entirely due to drought,
and free-text notes for several others describe drought, pest (pod borer),
hail, and disease (halo blight) damage."
  
  uri <- "doi:10.25502/9kj5-mb56"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "IITA;ICRAF;WUR",
                                  publication = NA,
                                  project = "N2Africa",
                                  design = "on-farm plot-level harvest trial, up to 6 plots per farm, comparing local vs improved climbing bean varieties",
                                  data_type = "on-farm experiment",
                                  treatment_vars = "variety_type",
                                  response_vars = "yield;yield_shaded;yield_unshaded;plant_density;light_interception",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-07-14",
                                  carob_completion = 50,
                                  carob_effort = 4
  )
  
  f1 <- ff[basename(ff) == "data_table.csv"]
  f2 <- ff[basename(ff) == "variable_definitions.csv"]
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  
  ### light_interception: below_canopy_reading vs. the transect's outside_canopy
  ### reference reading, averaged across all no-pruning transect locations
  below_cols <- grep("^below_canopy_reading_location_.*_no_pruning_mmol_m2_s$", names(r1), value = TRUE)
  transect_num <- as.integer(sub(".*_transect_(\\d+)_no_pruning.*", "\\1", below_cols))
  outside_cols <- paste0("measurement_outside_canopy_transect_", transect_num, "_no_pruning_mmol_m2_s")
  
  below_mat <- as.matrix(r1[, below_cols])
  outside_mat <- as.matrix(r1[, outside_cols])
  interception_mat <- 1 - (below_mat / outside_mat)
  
  r1$light_interception <- rowMeans(interception_mat, na.rm = TRUE) * 100
  r1$light_interception[is.nan(r1$light_interception)] <- NA
  
  ### Parse free-text harvest observations into standard fields (farm-level)
  obs_text <- r1$observations_during_after_harvest
  obs_text[obs_text == "#NAME?"] <- NA
  
  r1$drought_stress <- ifelse(grepl("drought", obs_text, ignore.case = TRUE), "severe", NA)
  r1$disease <- ifelse(grepl("blight", obs_text, ignore.case = TRUE), "halo blight", NA)
  r1$pest_species <- ifelse(grepl("pod borer", obs_text, ignore.case = TRUE), "pod borer", NA)
  r1$season_constraint <- ifelse(grepl("hilstorm|hailstorm", obs_text, ignore.case = TRUE), "hail", NA)
  
  ### variety type, inferred from sole_climbing_beans_in_experiment_this_farm
  r1$variety_type <- ifelse(
    grepl("local", r1$sole_climbing_beans_in_experiment_this_farm) & grepl("improved", r1$sole_climbing_beans_in_experiment_this_farm), "local;improved",
    ifelse(grepl("improved", r1$sole_climbing_beans_in_experiment_this_farm), "improved",
    ifelse(grepl("local", r1$sole_climbing_beans_in_experiment_this_farm), "local", NA))
  )
  
  ### Reshape plot-level harvest data - one row per farm x plot
  plot_data <- do.call(rbind, lapply(1:6, function(i) {
    ## plot 3's row-count column is missing its number in the raw data
    n_rows_col <- if (i == 3) "number_of_rows_crop_1_plot__nr" else paste0("number_of_rows_crop_1_plot_", i, "_nr")
    
    ## least/most shaded harvest subsample - only plots 1-4 have these columns
    least_shaded_kg   <- if (i <= 4) r1[[paste0("grain_weight_net_harvest_area_least_shaded_plot", i, "_kg")]] else NA
    least_shaded_len  <- if (i <= 4) r1[[paste0("row_length_net_harvest_area_least_shaded_plot", i, "_m")]] else NA
    least_shaded_rows <- if (i <= 4) r1[[paste0("number_of_rows_net_harvest_area_least_shaded_plot", i, "_nr")]] else NA
    
    most_shaded_kg   <- if (i <= 4) r1[[paste0("grain_weight_net_harvest_area_most_shaded_plot", i)]] else NA
    most_shaded_len  <- if (i <= 4) r1[[paste0("row_length_net_harvest_area_most_shaded_plot", i)]] else NA
    most_shaded_rows <- if (i <= 4) r1[[paste0("number_of_rows_net_harvest_area_most_shaded_plot", i)]] else NA
    
    data.frame(
      farm_id = r1$farm_id,
      plot_id = as.character(i),
      grain_weight_kg = r1[[paste0("grain_weight_crop_1_plot_", i, "_kg")]],
      width_m = r1[[paste0("width_of_harvested_plot_crop_1_plot_", i, "_m")]],
      n_rows = r1[[n_rows_col]],
      row_spacing_cm = r1[[paste0("row_spacing_crop_1_plot_", i, "_cm")]],
      plant_spacing_cm = r1[[paste0("plant_spacing_crop_1_plot_", i, "_cm")]],
      no_plants_hole = r1[[paste0("no_plants_hole_crop_1_plot_", i, "_nr")]],
      least_shaded_kg = least_shaded_kg,
      least_shaded_len = least_shaded_len,
      least_shaded_rows = least_shaded_rows,
      most_shaded_kg = most_shaded_kg,
      most_shaded_len = most_shaded_len,
      most_shaded_rows = most_shaded_rows
    )
  }))
  
  ## harvested plot area (m2) and yield (kg/ha) - see ISSUES for the formula and its source
  plot_data$area_m2 <- plot_data$width_m * (plot_data$n_rows * plot_data$row_spacing_cm / 100)
  plot_data$yield <- (plot_data$grain_weight_kg / plot_data$area_m2) * 10000
  plot_data$plant_density <- (plot_data$no_plants_hole / (plot_data$row_spacing_cm/100 * plot_data$plant_spacing_cm/100)) * 10000
  
  ## suggested new terms: yield from the shaded-subsample harvest zones
  plot_data$area_unshaded_m2 <- plot_data$least_shaded_len * (plot_data$least_shaded_rows * plot_data$row_spacing_cm / 100)
  plot_data$yield_unshaded <- (plot_data$least_shaded_kg / plot_data$area_unshaded_m2) * 10000
  
  plot_data$area_shaded_m2 <- plot_data$most_shaded_len * (plot_data$most_shaded_rows * plot_data$row_spacing_cm / 100)
  plot_data$yield_shaded <- (plot_data$most_shaded_kg / plot_data$area_shaded_m2) * 10000
  
  ## documented total crop failure (drought) - a real, informative zero, not NA
  plot_data$yield[plot_data$farm_id == "Ug" & plot_data$plot_id == "1"] <- 0
  
  ### Merge farm-level info into the plot-level table
  d <- merge(plot_data,
             data.frame(
               farm_id = r1$farm_id,
               country = "Uganda",
               adm2 = carobiner::fix_name(r1$lga_district_woreda, "title"),
               adm3 = carobiner::fix_name(r1$sector_ward, "title"),
               harvest_date = as.character(as.Date(r1$date_of_final_harvest_whole_n2a_field_date, format = "%d-%b-%y")),
               drought_stress = r1$drought_stress,
               disease = r1$disease,
               pest_species = r1$pest_species,
               season_constraint = r1$season_constraint,
               variety_type = r1$variety_type,
               light_interception = r1$light_interception
             ),
             by = "farm_id", all.x = TRUE)
  
  ## any harvest date parsing to year 1999 is implausible for a 2016 trial -
  ## treat all as the same placeholder/entry-error pattern, not just "1-Jan-99"
  ## (also seen as e.g. "1999-08-05", "1999-08-03")
  d$harvest_date[grepl("^1999-", d$harvest_date)] <- NA
  
  ### District-level coordinates (sub-county names too inconsistent to geocode
  ### reliably). Values obtained via:
  ###   carobiner::geocode(country = "Uganda", location = "Kapchorwa")$put
  ###   carobiner::geocode(country = "Uganda", location = "Kanungu")$put.
  district_coords <- data.frame(
    lga_district_woreda = c("Kapchorwa", "Kanungu"),
    longitude = c(34.3962, 29.7143),
    latitude = c(1.3311, -0.7064)
  )
  
  d <- merge(d, district_coords, by.x = "adm2", by.y = "lga_district_woreda", all.x = TRUE)
  d$geo_from_source <- FALSE
  
  d$trial_id <- as.character(as.integer(as.factor(d$farm_id)))
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- NA
  d$crop <- "common bean"
  d$yield_part <- "seed"
  d$yield_moisture <- NA
  d$yield_isfresh <- NA
  d$row_spacing <- d$row_spacing_cm
  d$plot_area <- d$area_m2
  
  ## no fertilizer or planting-date data exist anywhere in this dataset
  d$planting_date <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$K_fertilizer <- NA
  d$fertilizer_used <- FALSE
  
  ## clean up intermediate/helper columns not part of the standard
  d$row_spacing_cm <- NULL
  d$width_m <- NULL
  d$n_rows <- NULL
  d$plant_spacing_cm <- NULL
  d$no_plants_hole <- NULL
  d$area_m2 <- NULL
  d$grain_weight_kg <- NULL
  d$least_shaded_kg <- NULL
  d$least_shaded_len <- NULL
  d$least_shaded_rows <- NULL
  d$most_shaded_kg <- NULL
  d$most_shaded_len <- NULL
  d$most_shaded_rows <- NULL
  d$area_unshaded_m2 <- NULL
  d$area_shaded_m2 <- NULL
  d$farm_id <- NULL
  
  carobiner::write_files(path, meta, d)
}