# R script for "carob"

## ISSUES
# partially standardized the production.csv (d4) - data processing complications as the data spans across fields and plots and I am still finding a way to correctly standardise and merge with the rest of the data - any assistance would be helpful



carob_script <- function(path) {
  
  "N2Africa demo - Northern Uganda, 2014, Season IIN2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.
The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"
  
  uri <- "doi:10.25502/4ymd-n614"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "IITA;ICRAF; WUR",
                                  publication = NA,
                                  project = NA,
                                  carob_date = "2026-06-17",
                                  design = NA,
                                  carob_group = "agronomy",
                                  data_type = "on-farm experiment",
                                  treatment_vars = "variety",
                                  response_vars = "yield", 
                                  carob_contributor = "Mitchelle Njukuya",
                                  carob_completion = 80,	
                                  carob_effort = 4,	
                                  notes = NA
  )
  
  
  f1 <- ff[basename(ff) == "general.csv"]
  f2 <- ff[basename(ff) == "experiment.csv"]
  f3 <- ff[basename(ff) == "land_crops_and_livestock.csv"]
  f4 <- ff[basename(ff) == "production.csv"]
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r3 <- read.csv(f3)
  r4 <- read.csv(f4)
  
  #general data
  d1 <- data.frame(
    country = r1[["country"]],
    adm2 = r1[["district"]],
    adm3 = r1[["sector_ward"]],
    location = r1[["village"]],
    latitude = r1[["gps_latitude_hh.decimal_degrees"]],
    longitude = r1[["gps_longitude_hh.decimal_degrees"]],
    elevation = r1[["gps_altitude_hh.m"]],
    sex = r1[["gender_of_farmer"]],
    age = r1[["age_of_farmer.years"]],
    season = r1[["previous_fieldbook_season"]],
    field_id = r1[["farm_id"]],
    is_head = r1[["farmer_hh_head"]],
    hh_child_18 = r1[["no_females_0_16_yrs"]] + r1[["no_males_0_16_yrs"]],
    hh_adult_women = r1[["no_females_17_35_yrs"]] + r1[["no_females_36_60_yrs"]],
    hh_elder_women = r1[["no_females_over_60_yrs"]],
    hh_adult_men = r1[["no_males_17_35_yrs"]] + r1[["no_males_36_60_yrs"]],
    hh_elder_men = r1[["no_males_over_60_yrs"]],
    hh_elders = r1[["no_females_over_60_yrs"]] + r1[["no_males_over_60_yrs"]]
    #harvest_date = r1[[]]
  )
  
  month <- trimws(r1$date_hhsurvey_mm.months)
  month[month == ""] <- NA
  
  month_num <- match(month, month.name)
  
  d1$year <- as.Date(
    sprintf("%04d-%02d-%02d",r1$date_hhsurvey_yyyy.years,
      month_num,r1$date_hhsurvey_dd.days)
  )
  
  month1 <- trimws(r1$date_harvest_mm_technician_1)
  month1[month1 == ""] <- NA
  
  month1_num <- match(month1, month.name)
  
  d1$harvest_date <- as.Date(
    sprintf("%04d-%02d-%02d",r1$date_harvest_yyyy_technician_1,
      month1_num,r1$date_harvest_dd_technician_1)
  )
  
  d1$education <- rep(NA_character_, nrow(r1))
  
  d1$education[r1$education_hh_head_yrs_other > 0] <- "other"
  d1$education[r1$education_hh_head_yrs_primary > 0] <- "primary"
  d1$education[r1$education_hh_head_yrs_secondary > 0] <- "secondary"
  d1$education[r1$education_hh_head_yrs_post_secundary > 0] <- "post_secondary"
  d1$education[r1$education_hh_head_yrs_university > 0] <- "university"
  
  #experiment data
  all_na_vars <- names(r2)[sapply(r2, function(x) all(is.na(x)))]
  r2 <- r2[, !sapply(r2, function(x) all(is.na(x)))]
  
  d2 <- NULL
  for(p in 1:8){tmp <- data.frame(field_id = r2$farm_id,plot = p,
      treatment = r2[[paste0("description_treatment_", p)]],
      variety =r2$experimental_treatments_variety_crop_1,
      fert_1_kg =r2[[paste0("fert_1_kg_plot_plot_",p,".kg_per_plot" )]],
      
      fert_2_kg =r2[[paste0("fert_2_kg_plot_plot_",p,".kg_per_plot" )]],
      
      if(p == 1){plot_width <- r2[["width_of_harvested_plot_crop_1_plot_1"]]
      } else { plot_width <- r2[[paste0("width_of_harvested_plot_crop_1_plot_",p,".m")]]},
      
      plot_length = r2[[paste0("depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_",p,".m")]],
      
      dmy_residue = r2[[paste0("above_ground_biomass_weight_husks_stover_res_crop_1_plot_",p,".kg")]],
      
      stringsAsFactors = FALSE)
    
    d2 <- rbind(d2, tmp)
    
  }
  
  d2$fertilizer_type <- apply( r2[c("experimental_treatments_fertilizer_1","experimental_treatments_fertilizer_2", "experimental_treatments_fertilizer_3" )],
    1,function(x){x <- x[!is.na(x)]x <- x[x != ""] paste(x, collapse = "; ")
    })
  
  fert_type <- apply(r2[c("experimental_treatments_fertilizer_1","experimental_treatments_fertilizer_2","experimental_treatments_fertilizer_3")],
    1,function(x){x <- x[!is.na(x)]x <- x[x != ""]paste(x, collapse = "; ")
    })
  
  d2$fertilizer_type <- rep(fert_type, times = 8)
  
  d2$OM_type <- rep(r2$experimental_treatments_type_of_manure,each = 8)
  
  d2$fertilizer_type[grepl("no fertilizer",d2$treatment,ignore.case = TRUE)] <- NA
  
  #livestock
  d3 <- NULL
  
  for(i in 1:3){tmp <- data.frame(field_id = r3$farm_id,field_size = ifelse(r3$farm_size_unit == "acre",r3$farm_size * 0.404686,r3$farm_size),
      crop = r3[[paste0("main_crop_", i)]],
      yield = r3[[paste0("yield_main_crop_", i)]] / ifelse(r3[[paste0("area_main_crop_", i)]] > 0,r3[[paste0("area_main_crop_", i)]],NA) *2.47105,
      stringsAsFactors = FALSE)
    d3 <- rbind(d3, tmp)
   }
  
  d3 <- d3[!is.na(d3$crop) & d3$crop != "", ]
  
  #production
  
  d4 <- NULL
  
  for(p in 1:4){tmp <- data.frame(field_id = r4$farm_id,field = p,
      
      # area is recorded in acres -> convert to m2
      plot_area = r4[[paste0("area_field_", p)]] * 4046.85642,
      walking_distance = r4[[paste0( "walking_distance_field_", p,".minutes")]],
      
      fertilizer_amount =r4[[paste0("amount_of_mineral_fertilizer_applied_field_",p )]],
      
      fertilizer_type = r4[[paste0("type_of_mineral_fertilizer_field_",p)]],
      
      OM_type =r4[[paste0("organic_inputs_applied_field_",p)]],
      
      inoculated = r4[[paste0("rhizobium_inoculant_applied_field_",p)]],
      
      stringsAsFactors = FALSE)
    
    d4 <- rbind(d4, tmp)
    
  }
  
  d4$inoculated <- ifelse(tolower(d4$inoculated) == "yes",TRUE,FALSE)
  d4$land_ownedby <- rep(r4$ownership_of_n2africa_field, times = 4)
  
  d4$planting_date <- rep(as.Date(paste(2013,r4$date_of_planting_mm,r4$date_of_planting_dd,sep = "-"),format = "%Y-%m-%d"),times = 4)
  
  d4$fertilizer_date <- rep(as.Date(paste(2013,r4$date_of_mineral_fertiliser_application_mm,r4$date_of_mineral_fertiliser_application_dd,sep = "-"),format = "%Y-%m-%d"),times = 4)
  
  d4$flowering_date <- rep(as.Date(paste(2013,r4$`50pct_flowering_mm`,r4$`50pct_flowering_dd`,sep = "-"),format = "%Y-%m-%d"),times = 4)
  
  d4$maturity_date <- rep(as.Date(paste(2013,r4$`50pct_maturity_mm`,r4$`50pct_maturity_dd`,sep = "-"),format = "%Y-%m-%d"),times = 4)
  
  d4$harvest_date <- rep(as.Date(paste(2013,r4$date_of_final_harvest_mm, r4$date_of_final_harvest_dd,sep = "-"),format = "%Y-%m-%d"),times = 4)
  
  d4$weeding_date <- as.Date(paste(2013,r4$date_of_1st_weeding_mm, r4$date_of_1st_weeding_dd,sep = "-"),format = "%Y-%m-%d")d4$weeding_times <- 4
  
  #merge
  d <- merge(d2,d1,by = "field_id",all.x = TRUE)
  d <- merge(d,d3,by = "field_id",all.x = TRUE )
  #d <- merge(d,d4,by = "field_id",all.x = TRUE)
  
  d$trial_id <- as.character(as.integer(as.factor(1)))
  d$on_farm <- TRUE
  d$is_survey <- TRUE 
  d$irrigated <-FALSE
  d$geo_from_source <- TRUE
  
  d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)    #composition and quantities of liquid fertilizers not specified
  d$fertilizer_type <- gsub("GYPSUM","gypsum",d$fertilizer_type)
  d$fertilizer_type <- gsub("SERENUT 6; ", "",d$fertilizer_type )
  
  d$crop <- trimws(tolower(d$crop))
  
  d$crop[d$crop %in% c(
    "ground nuts",
    "grount nuts",
    "g per nuts"
  )] <- "groundnut"
  
  d$crop[d$crop == "cassava"] <- "cassava"
  d$crop[d$crop == "maize"] <- "maize"
  d$crop[d$crop == "millet"] <- "millet"
  d$crop[d$crop == "sunflower"] <- "sunflower"
  d$crop[d$crop == "soya"] <- "soybean"
  d$crop[d$crop == "rice"] <- "rice"
  d$crop[d$crop == "citrus"] <- "citrus"
  d$crop[d$crop == "hibiscus"] <- "roselle"
  
  d$yield_part <- NA_character_
  
  d$yield_part[d$crop == "maize"] <- "grain"
  d$yield_part[d$crop == "groundnut"] <- "pod"
  d$yield_part[d$crop == "soybean"] <- "grain"
  d$yield_part[d$crop == "sunflower"] <- "seed"
  d$yield_part[d$crop == "millet"] <- "grain"
  d$yield_part[d$crop == "rice"] <- "grain"
  d$yield_part[d$crop == "cassava"] <- "roots"
  d$yield_part[d$crop == "citrus"] <- "fruit"
  d$yield_part[d$crop == "roselle"] <- "aboveground biomass"
  d$yield_moisture <- as.logical(FALSE)
  d$yield_isfresh <- NA
  
  d$plot<-d$is_head<-d$fert_1_kg<-d$fert_2_kg<-d$if..p....1... <- NULL
  
  d$season <- NULL
  d$planting_date <- as.character(NA)
  d$harvest_date <- as.character.Date(d$harvest_date)
  d$yield_moisture <- as.logical(d$yield_moisture)
  d$fertilizer_type[d$fertilizer_type=="TSP; gypsum"] <- "TSP;gypsum"
  d$treatment <- trimws(d$treatment)
  d$treatment[d$treatment == ""] <- NA
  d$country <- trimws(d$country)
  d$country[d$country %in% c ("UGANDA","")] <- "Uganda"
  d$adm2 <- trimws(tolower(d$adm2))
  d$adm3 <- trimws(tolower(d$adm3))
  d$location <- trimws(tolower(d$location))
  d$longitude[d$location=="nakoma"] <- "33.7"
  d$latitude[d$location=="nakoma"] <- "0.53"
  d$elevation[d$location=="nakoma"] <- "1091"
  d$longitude[d$location=="agurur"] <- "33.8"
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  d$elevation <- as.numeric(d$elevation)
  
  d <- unique(d)
  
  carobiner::write_files(path, meta, d)
}

