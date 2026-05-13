# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1.errors left have to do with how the raw data was collected
carob_script <- function(path) {
  
  "In order to assess the impact of the Land Restoration Program, understanding what land restoration options work, where and for whom, there is need to identify the context-specific variables that may influence the performance of the restoration options as well as their uptake. In addition to monitoring the performance of the restoration option being implemented, a registration of the farmers involved in the project was conducted. A standard household survey was used, assessing both the socio-economic and biophysical characteristics of the households. The farmers were from four district of Ethiopia: Boset, Gursum, Samre and Tsaeda Emba. The present dataset includes socio-economical data about 173 households, including general information about the farms. Specific data about agricultural operations, crops, trees and the experimental plots developed inside the project, are part of a separated dataset. NOTE: The coordinates were removed from the dataset in May 2021, in order to comply with GDPR standards. The location details are available on request: please contact the author and explain the purpose of your research."
  
  uri <- "doi:10.25502/dqaw-f631"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
    data_organization = "IITA;ICRAF;UZIM;WUR",
    publication = NA,
    project = NA,
    data_type = "survey",
    treatment_vars = "none",
    response_vars = "none", 
    completion = 90,
    carob_contributor = "Blessing Dzuda",
    carob_date = "2026-05-12",
    notes = "none", 
    design = NA
  )
  
  f <- ff[basename(ff) == "a_general.csv"]
  f1<- ff[basename(ff) == "a_inputs.csv"]
  f2<- ff[basename(ff) == "d_cropping_calendar.csv"]
  f3<- ff[basename(ff) == "e_harvest.csv"]
  r <- read.csv(f)
  r1<- read.csv(f1)
  r2<- read.csv(f2)
  r3<- read.csv(f3)
  
  d <- data.frame(
    hhid=as.character(r$id),
    field_id=r$farm_id,
    country=r$country,
    adm2=r$district,
    adm3=r$sector_ward,
    adm4=trimws(r$vilage),
    farmer_age=as.numeric(r$age),
    field_size=r$farm_size_ha,
    farmer_education=trimws(r$highest_education_household),
    crop="legume",
    yield_part="seed")
  
    d$on_farm <- TRUE
    d$is_survey <- TRUE
    d$irrigated <- FALSE
    d$geo_from_source <- FALSE
    d$yield_moisture <- as.numeric(NA)
  
  #fixing crop names
  crop_map <- c(
    groundnut = "groundnut",
    beans = "common bean",
    soybean = "soybean",
    cowpea = "cowpea"
  )
  
  crop_cols <- names(crop_map)
  d$crop_rotation <- apply(r[, crop_cols], 1, function(x) {
    crops <- crop_map[x == "Y"]
  
    if(length(crops) == 0) {
      NA
    } else {
      paste(crops, collapse = ";")
    }
    
  })
  
  d$adm2 <- trimws(tolower(d$adm2))
  d$adm2[d$adm2 == "goronzi"] <- "goromonzi"
  d$adm2[d$adm2 == "hwedza"] <- "wedza"
  d$adm2 <- tools::toTitleCase(d$adm2)

  #fixing coordinates
  loc<- data.frame(
    adm2 = c("Goromonzi", "Makoni", 
               "Wedza", "Chegutu", "Mudzi", "Mbire", "Guruve", "Murehwa", 
               "Mabika"),
  longitude = c(31.3724, 32.1372, 31.6844, 30.3976, 32.5494, 30.2237, 30.629, 31.8388, 29.9024),
  latitude = c(-17.8183, -18.3849, -18.7677, -18.1868, -17.0517, -16.062, -16.3432, -17.8044, -20.7331))
  
  #merging coordinates
  d <- merge(d, loc, by = "adm2", all.x = TRUE)
  
  #adding fertilizer inputs
  r1$input <- trimws(tolower(r1$input))
  r1$input[r1$input %in% c("compoond d", "compound d", "compund d","compond d","comp d(purchased)","conpound d","comp d")] <- "compound d"
  r1$input[r1$input %in% c("ssp, gypsum", "ssp")] <- "ssp"
  r1$input[r1$input %in% c("an","a.n","a.n(purchased)","ammonium","top dressing")] <- "an"
  r1$input[r1$input %in% c("compound l")] <- "compound l"
  r1$input[r1$input %in% c("urea")] <- "urea"
  r1$input[r1$input %in% c("compound c")] <- "compound c"
  
  fert_inputs <- c("an", "ssp", "compound d","compound l","urea","compound c")
  r1 <- r1[r1$input %in% fert_inputs, ]
  
  r1$N_fertilizer <- NA
  r1$P_fertilizer <- NA
  r1$K_fertilizer <- NA
  names(r1)[names(r1) == "farm_id"] <- "field_id"
  
  #fixing fert values
  r1$quantity <- as.character(r1$quantity)
  r1$quantity <- tolower(trimws(r1$quantity))
  r1$quantity <- gsub("o", "0", r1$quantity)
  r1$quantity <- gsub("[^0-9.]", "", r1$quantity)
  r1$quantity <- as.numeric(r1$quantity)
  
  #Cleaning values
  #AN
  r1$N_fertilizer[r1$input == "an"] <-
    r1$quantity[r1$input == "an"] * 0.345
  #Urea
  r1$N_fertilizer[r1$input == "urea"] <-
    r1$quantity[r1$input == "urea"] * 0.46
  #SSP
  r1$P_fertilizer[r1$input == "ssp"] <-
    (r1$quantity[r1$input == "ssp"] * 0.18) / 2.29
  #Comp.D(NPK)
  r1$N_fertilizer[r1$input == "compound d"] <-
    r1$quantity[r1$input == "compound d"] * 0.07
  
  r1$P_fertilizer[r1$input == "compound d"] <-
    (r1$quantity[r1$input == "compound d"] * 0.14) / 2.29
  
  r1$K_fertilizer[r1$input == "compound d"] <-
    (r1$quantity[r1$input == "compound d"] * 0.07) / 1.2051
  #Comp.L(NPK)
  r1$N_fertilizer[r1$input == "compound l"] <-
    r1$quantity[r1$input == "compound l"] * 0.05
  
  r1$P_fertilizer[r1$input == "compound l"] <-
    (r1$quantity[r1$input == "compound l"] * 0.18) / 2.29
  
  r1$K_fertilizer[r1$input == "compound l"] <-
    (r1$quantity[r1$input == "compound l"] * 0.10) / 1.2051
  #Comp.C
  r1$N_fertilizer[r1$input == "compound c"] <-
    r1$quantity[r1$input == "compound c"] * 0.06
  
  r1$P_fertilizer[r1$input == "compound c"] <-
    (r1$quantity[r1$input == "compound c"] * 0.15) / 2.29
  
  r1$K_fertilizer[r1$input == "compound c"] <-
    (r1$quantity[r1$input == "compound c"] * 0.12) / 1.2051
  
  
  fert_sum <- aggregate(
    cbind(
      N_fertilizer,
      P_fertilizer,
      K_fertilizer
    ) ~ field_id,
    data = r1,
    sum,
    na.rm = TRUE
  )
  #merging fertilizer values
  d <- merge(
    d,
    fert_sum,
    by = "field_id",
    all.x = TRUE
  )
  
  #fixing planting date
  # rename columns
  names(r2)[names(r2) == "farm_id"] <- "field_id"
  names(r2)[names(r2) == "date_planting_yyyy"] <- "planting_date"
  
  # clean activity names
  r2$activity <- tolower(trimws(r2$activity))
  
  # keep only planting records
  r2 <- r2[r2$activity == "date of planting", ]
  
  # convert planting year to numeric
  r2$planting_date <- as.numeric(r2$planting_date)
  
  # replace 0 with NA
  r2$planting_date[r2$planting_date == 0] <- NA
  
  # keep only needed columns
  plant_year <- unique(
    r2[, c("field_id", "planting_date")]
  )
  
  # merge into main dataset
  d <- merge(
    d,
    plant_year,
    by = "field_id",
    all.x = TRUE
  )
  
  d$planting_date <- as.character(d$planting_date)
  d$trial_id <- paste(d$country, d$hhid, sep = "-")

  #standardizing yield
  names(r3)[names(r3) == "farm_id"] <- "field_id"
  
  r3$yield <-
    (r3$weight_kg / r3$area_harvested_m2) * 10000
  
  #aggregation
  yield_sum <- aggregate(
    yield ~ field_id,
    data = r3,
    mean,
    na.rm = TRUE
  )
  
  d <- merge(
    d,
    yield_sum,
    by = "field_id",
    all.x = TRUE
  ) 
  
  carobiner::write_files(path, meta, d)
}