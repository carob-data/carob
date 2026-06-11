# R script for "carob"
# license: GPL (>=3)

## ISSUES
#coordinates were extracted from google maps

carob_script <- function(path) {
  
"Published data on soil tillage systems for cassava were systematized in an electronic spreadsheet. The tillage systems were implemented in different regions, climates, soils, land uses, cover crops, and crop management. This spreadsheet allows you to filter data by topics of interest, facilitating access, understanding, and use of information from the studies included in this systematization. (2023-06-26)"
 
  uri <- "doi:10.7910/DVN/IDHBYF"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
   data_organization = "Embrapa",
   publication = NA,
   project = NA,
   data_type = "survey",
   treatment_vars = "none",
   response_vars = "none", 
   completion = 100,
   carob_contributor = "Blessing Dzuda",
   carob_date = "2025-05-27",
   notes = NA,
   design = NA
  )
  
  f <- ff[basename(ff) == "Data_2023_06_27.xlsx"]
  r <- carobiner::read.excel(f, sheet="Data")
  
  d <- data.frame(
    country = r$Country,
    adm1=r$State,
    crop="cassava",
    land_prep_method=r$Tillage,
    cover_crop=r$`Cover crop`,
    variety=r$Cultivar,
    yield=as.numeric(r$`Yield (t/ha)`)*1000,
    soil_depth=r$`Depth  (cm)`,
    soil_sand=as.numeric(r$`Sand (g/kg)`)*0.1,
    soil_silt=as.numeric(r$`Silt (g/kg)`)*0.1,
    soil_clay=as.numeric(r$`Clay (g/kg)`)*0.1) 
  
  d$cover_crop_used <- TRUE
  
  tillage_lookup <- c(
    "CT" = "conventional",
    "RT" = "reduced tillage",
    "MT" = "minimum tillage",
    "NT" = "none",
    "CP+FB" = "reduced tillage",
    "CP+RFM" = "reduced tillage;tied ridges",
    "DP+FB" = "conventional",
    "DP+RFM" = "conventional;tied ridges",
    "Farmer's practice" = "conventional"
  )
  
    d$land_prep_method <- tillage_lookup[d$land_prep_method]
    
    cover_crop_lookup <- c(
      "None" = "none",
      "Jackbean (Canavalia ensiforinis) + Vigna sinensis" = "jack bean;cowpea",
      "Black oat (Avena strigosa)" = "black oats",
      "Oat (Avena sativa)" = "oats",
      "Black oat (Avena strigosa) + forage turnip (Raphanus sativus)" = "black oats;radish",
      "Sorghum (Sorghum bicolor)" = "sorghum",
      "Sunn hemp (Crotalaria juncea)" = "sunn hemp",
      "Sorghum + sunn hemp" = "sorghum;sunn hemp",
      "Urochloa decumbens" = "brachiaria",
      "Urochloa ruziziensis" = "brachiaria")
    
    d$cover_crop <- cover_crop_lookup[d$cover_crop]
    
    lat_lon <- data.frame(
      adm1=c("Bahia","Paraná","Tamil Nadu","Mato Grosso do Sul","São Paulo"),
      longitude=c(-12.5983,-24.6817,11.0686,-20.1798,-23.5531),
      latitude=c(-41.0698,-52.0502,78.3955,-5.5038,-46.6637))
    
    d <- merge(d,lat_lon,by="adm1",all.x=TRUE)
    
    d$yield_part <- "tubers"
    d$yield_isfresh <- NA
    d$trial_id <- paste(d$adm1, d$crop, sep = "_")
    d$soil_depth <- as.numeric(gsub(".*-", "", d$soil_depth))
    d$on_farm <- FALSE
    d$is_survey <- TRUE
    d$irrigated <- FALSE
    d$geo_from_source <- FALSE
    d$planting_date <- as.character(NA)
    d$yield_moisture <- as.numeric(NA)
    d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
      
    d <- unique(d)
    
  carobiner::write_files(path, meta, d)
}
