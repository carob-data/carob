# R script for "carob"

carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project."
  
  uri <- "doi:10.25502/ec86-2t29"
  group <- "agronomy"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
    data_organization = "IITA;WUR",
    publication= NA,
    project="N2Africa",
    data_type= "experiment",
    treatment_vars = "variety; fertilizer_date; weeding_dates; weeding_dates; insecticide_dates",
    response_vars = "flowering_date; maturity_date; yield",
    carob_contributor= "Mitchelle Njukuya",
    carob_date="2024-03-19",
    notes="Need to clarify the intercropping with banana situation" 
  )
  
  f <- ff[basename(ff) == "data_table.csv"]
  r <- read.csv(f)
  
  rr <- reshape(r, direction="long",
                varying=c("width_of_harvested_plot_crop_1_plot_1.m", "grain_weight_crop_1_plot_1.kg",
                          "width_of_harvested_plot_crop_1_plot_2.m", "grain_weight_crop_1_plot_2.kg",
                          "width_of_harvested_plot_crop_1_plot_3.m", "grain_weight_crop_1_plot_3.kg",
                          "width_of_harvested_plot_crop_1_plot_4.m", "grain_weight_crop_1_plot_4.kg",
                          "width_of_harvested_plot_crop_1_plot_5.m", "grain_weight_crop_1_plot_5.kg", 
                          "width_of_harvested_plot_crop_1_plot_6.m", "grain_weight_crop_1_plot_6.kg"), 
                timevar="treatment",
                times= c("treatment1", "treatment2", "treatment3", "treatment4", "treatment5", "treatment6"),
                v.names= c("width", "yield"), idvar= "treatment")
  
  d <- data.frame(trial_id=rr$farm_id,
                  record_id=1:nrow(rr),
                  country=rr$country, 
                  location=rr$lga_district_woreda, 
                  site=rr$sector_ward, 
                  longitude=rr$gps_field_device_longitude.decimal_degrees, 
                  latitude=rr$gps_field_device_latitude.decimal_degrees,
                  elevation=rr$gps_field_device_altitude.m,
                  geo_from_source = TRUE,
                  crop="common bean",
                  irrigated=FALSE,
                  fertilizer_type=rr$type_mineral_fertilizer_experiment, 
                  OM_type=rr$type_organic_fertilizer_experiment,
                  weed_species=rr$type_of_weeds, 
                  weed_severity=rr$severity_weeds, 
                  disease_severity=rr$severity_disease,
                  soil_quality=rr$relative_fertility_n2africa_field,
                  previous_crop=rr$crop_1_previous_season,
                  drought_stress=rr$severity_drought,
                  pest_severity=rr$severity_pests,
                  pest_species=gsub(", ", "; ", rr$type_of_pest),
                  diseases=gsub(", ", "; ", rr$type_of_disease),
                  width=rr$width,
                  yield=rr$yield)
  
  d$country <- carobiner::fix_name(d$country, "title")
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  
  d$treatment <- gsub("^.*\\.", "", row.names(rr))
  
  d$variety <- NA
  d$variety[grepl(paste0(c("1", "2", "5"), collapse = "|"), d$treatment)] <- "Kabweseri"
  d$variety[grepl(paste0(c("3", "4", "6"), collapse = "|"), d$treatment)] <- "Nabe12c"
  
  d$planting_date <- as.character(as.Date(rr$date_of_planting_whole_n2africa_field.date, "%d-%b-%y"))
  d$fertilizer_date <- as.character(as.Date(rr$date_of_planting_whole_n2africa_field.date, "%d-%b-%y"))
  d$weeding_dates <- paste(as.Date(rr$date_of_1st_weeding_whole_n2africa_field_.date, "%d-%b-%y"),
                           as.Date(rr$date_of_2nd_weeding_whole_n2africa_field.date, "%d-%b-%y"), sep = ";")
  d$weeding_dates <- gsub("NA;", "",  d$weeding_dates) 
  d$insecticide_dates <- as.character(as.Date(rr$date_of_insecticide_application_1.date, "%d-%b-%y"))
  d$flowering_date <- as.character(as.Date(rr$X50pct_flowering_whole_n2africa_field.date, "%d-%b-%y"))
  d$maturity_date <- as.character(as.Date(rr$X50pct_maturity_whole_n2africa_field.date, "%d-%b-%y"))
  d$harvest_date <- as.character(as.Date(rr$date_of_final_harvest_whole_n2a_field.date, "%d-%b-%y"))
  d$harvest_date <- ifelse(as.Date(d$harvest_date) < as.Date(d$planting_date), NA, d$harvest_date)
  
  # EGB:
  # # According to N2A protocols and dataset specifications
  d$N_fertilizer <- 0
  d$P_fertilizer <- 10 * 0.1923 
  d$K_fertilizer <- 0
  
  d$OM_type <- carobiner::replace_values(d$OM_type, "farmyard_manure", "farmyard manure")
  ## for all? ... Yes
  d$OM_used <- TRUE
  
  # EGB:
  # # Assuming plot length of 2 m. Problem is the difference between most and least shaded. Not sure how to go about it...
  d$yield <- (rr$yield/(2*rr$width))*10000 
  d$yield_part <- "seed"
  
  # EGB:
  # # Add location from Google Maps
  d$longitude[grepl("Nyamabale", d$site)] <- 29.85
  d$latitude[grepl("Nyamabale", d$site)] <- -1.1
  d$geo_from_source[grepl("Nyamabale", d$site)] <- FALSE
  
  # Remove rows where treatments 5 and 6 where not carried
  dd <- d[!(d$trial_id %in% c("UGAKAN_CBBI_28", "UGAKAN_CBBI_27") & d$treatment %in% c("treatment5", "treatment6")), 
          colnames(d)[!(colnames(d) %in% c("width"))]]
  
  d$yield[which(d$yield > 25000)] <- NA
  
  carobiner::write_files(meta, dd, path=path)
}
