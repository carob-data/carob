# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. Could not find the crop "Lekua"
#2 Added a new crop yellow rocket (Barbarea vulgaris)

carob_script <- function(path) {
  

  "In order to assess the impact of the Land Restoration Program, understanding what land restoration options work, where and for whom, there is need to identify the context-specific variables that may influence the performance of the restoration options as well as their uptake. In addition to monitoring the performance of the restoration option being implemented, a registration of the farmers involved in the project was conducted. A standard household survey was used, assessing both the socio-economic and biophysical characteristics of the households. The farmers were from four district of Ethiopia: Boset, Gursum, Samre and Tsaeda Emba. The present dataset includes socio-economical data about 173 households, including general information about the farms. Specific data about agricultural operations, crops, trees and the experimental plots developed inside the project, are part of a separated dataset. NOTE: The coordinates were removed from the dataset in May 2021, in order to comply with GDPR standards. The location details are available on request: please contact the author and explain the purpose of your research."
  
  uri <- "hdl:20.500.11766.1/FK2/LU99IW"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=11, minor=0,
    data_organization = "ICRAF;World Vision International;University of Copenhagen",
    publication = NA,
    project = NA,
    data_type = "survey",
    treatment_vars = "none",
    response_vars = "none", 
    completion = 90,
    carob_contributor = "Blessing Dzuda",
    carob_date = "2026-05-13",
    notes = NA, 
    design = NA
  )
  
  f <- ff[basename(ff) == "FP_Data173.csv"]
  f2 <- ff[basename(ff) == "DataDictionary_ElementDescription.csv"]
  r <- read.csv(f, sep = ";", stringsAsFactors = FALSE)
  vars<- read.csv(f2, sep = ";", stringsAsFactors = FALSE)
  
  d <- data.frame(
    hhid=r$HH_ID,
    country=r$Country,
    adm2=r$Zone,
    adm3=r$Woreda,
    farmer_age=r$Farmer_age,
    field_size=r$Farm_Size,
    cropland_used=r$Cultivated_Area,
    farmer_education=r$HH_Head_Edu,
    crop_rotation=tolower(r$Rotation_Type))
  
  d$adm3 <- trimws(d$adm3)
  d$trial_id <- d$trial_id <- paste(d$hhid, d$adm3, sep = "_")
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  d$yield_moisture <- as.numeric(NA)
  d$geo_from_source <- FALSE
  d$yield_isfresh <- NA
  d$planting_date <- as.character(NA)
  
  #processing fertilizer data
  d$N_fertilizer <- 0
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  
  #Fert,column1
  #NPK
  i <- !is.na(r$Type_Fert1) & r$Type_Fert1 == "NPK"
  d$N_fertilizer[i] <- d$N_fertilizer[i] + (0.15 * r$Amount_Fert1[i])
  d$P_fertilizer[i] <- d$P_fertilizer[i] + ((0.15 * r$Amount_Fert1[i]) / 2.29)
  d$K_fertilizer[i] <- d$K_fertilizer[i] + ((0.15 * r$Amount_Fert1[i]) / 1.2051)
  
  #NPS
  i <- !is.na(r$Type_Fert1) & r$Type_Fert1 == "NPS"
  d$N_fertilizer[i] <- d$N_fertilizer[i] + (0.19 * r$Amount_Fert1[i])
  d$P_fertilizer[i] <- d$P_fertilizer[i] + ((0.38 * r$Amount_Fert1[i]) / 2.29)
  
  #UREA
  i <- !is.na(r$Type_Fert1) & r$Type_Fert1 == "UREA"
  d$N_fertilizer[i] <- d$N_fertilizer[i] + (0.46 * r$Amount_Fert1[i])
  
  #DAP
  i <- !is.na(r$Type_Fert1) & r$Type_Fert1 == "DAP"
  d$N_fertilizer[i] <- d$N_fertilizer[i] + (0.18 * r$Amount_Fert1[i])
  d$P_fertilizer[i] <- d$P_fertilizer[i] + ((0.46 * r$Amount_Fert1[i]) / 2.29)
  
  #Fert,column2
  #NPS
  i <- !is.na(r$Type_Fert2) & r$Type_Fert2 == "NPS"
  d$N_fertilizer[i] <- d$N_fertilizer[i] + (0.19 * r$Amount_Fert2[i])
  d$P_fertilizer[i] <- d$P_fertilizer[i] + ((0.38 * r$Amount_Fert2[i]) / 2.29)
  
  #UREA
  i <- !is.na(r$Type_Fert2) & r$Type_Fert2 == "UREA"
  d$N_fertilizer[i] <- d$N_fertilizer[i] + (0.46 * r$Amount_Fert2[i])
  
  
#processing crop,variety and yield
  crop <- data.frame(
    hhid = rep(r$HH_ID, 6),
    
    crop = c(r$Type_Crop1,
             r$Type_Crop2,
             r$Type_Crop3,
             r$Type_Crop4,
             r$Type_Crop5,
             r$Type_Crop6),
    
    variety = c(r$Variety_Crop1,
                r$Variety_Crop2,
                r$Variety_Crop3,
                r$Variety_Crop4,
                r$Variety_Crop5,
                r$Variety_Crop6),
    
    weight = c(r$Yield_Crop1,
              r$Yield_Crop2,
              r$Yield_Crop3,
              r$Yield_Crop4,
              r$Yield_Crop5,
              r$Yield_Crop6),
    
    area = c(r$Area_Crop1,
             r$Area_Crop2,
             r$Area_Crop3,
             r$Area_Crop4,
             r$Area_Crop5,
             r$Area_Crop6)
  )
  
  crop <- crop[!is.na(crop$crop) & crop$crop != "", ]
  crop$crop <- tolower(trimws(crop$crop))
  crop$harvested_weight <- as.numeric(crop$weight)
  crop$area <- as.numeric(crop$area)
  
  crop$yield <- crop$harvested_weight / crop$area
  
  
  #merging
  d <- merge(crop, d, by = "hhid")
  
  d <- d[ , !names(d) %in% c("weight", "area", "harvested_weight")]
  
  #renaming crops
  crop_map <- c(
  "groundnuts" = "groundnut",
  "cereals" = "cereal",
  "barbare" = "yellow rocket",
  "demhay" = "barley",
  "mix of wheat and barley" = "cereal",
  "mix of maize and wheat" = "cereal",
  "birshin" = "lentil",
  "lentils" = "lentil",
  "adengur" = "cowpea",
  "field peas" = "cowpea",
  "cowpeas" = "cowpea",
  "peas" = "pea",
  "beans" = "faba bean",
  "barely" = "barley",
  "vegetables" = "vegetable",
  "vegatables" = "vegetable",
  "solanacea spp" = "vegetable",
  "rotated with legumes" = "legume",
  "legumes" = "legume",
  "sorghum mix with legumes" = "legume",
  "sorghum mix with legume" = "legume",
  "mix of wheat and barely" = "cereal",
  "barley (legumes cultivated  like  chickpea)" = "barley")
  
  idx <- d$crop %in% names(crop_map)
  d$crop[idx] <- crop_map[d$crop[idx]]
  
  #yield_part
  yield_map <- c(
    "wheat" = "grain",
    "coffee" = "grain",
    "barley" = "grain",
    "sorghum" = "grain",
    "maize" = "grain",
    "cereal" = "grain",
    "teff" = "grain",
    "lentil" = "seed",
    "yellow rocket" = "seed",
    "groundnut" = "seed",
    "cowpea" = "seed",
    "pea" = "seed",
    "chickpea" = "seed",
    "faba bean" = "seed",
    "pepper" = "fruit",
    "tomato" = "fruit",
    "onion" = "roots",
    "potato" = "roots",
    "carrot" = "roots")
  
  d$yield_part <- yield_map[d$crop]
  d$crop_rotation <- gsub("-",";", d$crop_rotation)
  
  #Cleaning rotation values
  clean_rotation <- function(x) {
    
    if(is.na(x)) return(NA)
    
    # split by ; or |
    parts <- unlist(strsplit(x, ";|\\|"))
    
    # clean spaces
    parts <- trimws(parts)
    
    # standardizing using crop_map
    idx <- parts %in% names(crop_map)
    parts[idx] <- crop_map[parts[idx]]
    
    # removing duplicates
    parts <- unique(parts)
    
    # recombining
    paste(parts, collapse = ";")
  }
  
  d$crop_rotation <- sapply(d$crop_rotation, clean_rotation)
  
  #adding lon,lat
  d$adm3 <- gsub("Tsaeda Emba","Tsaedaemba", d$adm3)
  loc <- data.frame(
    adm3 = c("Gursum", "Boset", "Samre","Tsaedaemba"),
    longitude = c(42.4008, 39.4554, 39.2091, 39.6667),
    latitude = c(9.353, 8.5748, 13.1882,14.2500))
  
  d <- merge(d,loc, by="adm3")
  d$hhid <- as.character(d$hhid)
  d$farmer_age <- as.numeric(d$farmer_age)
  d <- d[!duplicated(d), ]
  
  carobiner::write_files(path, meta, d)
}
