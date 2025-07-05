# R script for "carob"


carob_script <- function(path) {
  
"The database contains data about water use in barley (Hordeum vulgare) and maize (Zea Mays) production in the Mexican Bajío. Water use and grain yield were evaluated under conventional tillage and under conservation agriculture in permanent raised beds, both under furrow irrigation and under drip irrigation. The data in this study were collected between 2016 and 2021 in CIMMYTs’ Bajío innovation hub in a research platform (a dedicated field experiment) and in innovation modules (side by side comparisons in farmers’ fields). For the platform, the data base contains data of three maize (summer) and three barley (winter) cycles, including water use, grain yield and data on key field operations. For the farmers’ fields, the data base contains the yield and water use data from 23 modules, as well as fuel use and soil health data from a subset of modules. (2021-06-29)"
  
  uri <- "hdl:11529/10548578"
  group <- "agronomy"
  
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
    data_organization = "CIMMYT;UGTO",
    publication=NA,
    project=NA,
    data_type= "on-farm experiment",
	response_vars = "yield",
	treatment_vars = "land_prep_method", #;irrigation_method
    carob_contributor= "Blessing Dzuda",
    carob_date="2024-05-10"
  )
  
  f <- ff[basename(ff) == "DAT-PUB-Irrigation_CA_CT-Guanajuato-v2.xlsx"]
  r <- carobiner::read.excel(f, sheet = "Data field experiment")
    
  d <- data.frame(
    adm1=r$State,
    latitude=r$Latitude,
    longitude=r$Longitude,
	geo_from_source=TRUE,
    elevation=r$Altitude,
    treatment=r$Name_tr,
    crop=r$Crop,
    land_prep_method=r$Till,
    previous_crop_residue_perc=r$Res_perc,
    rain=r$Precip,
    irrigation_number=r$Irrigation_number,
    irrigation_amount=r$Total_Irrigation,
    variety=r$Variety,
    N_fertilizer=r$Fert_N,
    P_fertilizer=r$Fert_P,
    K_fertilizer=r$Fert_K,
    plant_height=r$Height,
    planting_date=r$Sowing_date,
    emergence_date=r$Emergence_date,
    flowering_days=r$Flower_days,
    maturity_days=r$Mat_days,
    harvest_date=r$Harvest_date,
    dmy_storage=r$Yield_dry,
    yield=r$Yield_moist,
    crop_price=r$Price
  )
  
  d$flowering_days[d$flowering_days > 1000] <- NA
  d$maturity_days[d$maturity_days > 1000] <- NA
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- TRUE 
  d$country <- "Mexico"
  d$yield_part <- "grain"
  d$trial_id <- paste0(d$adm1,"_",d$rep)
  d$crop <- gsub("Barley","barley",d$crop)
  d$crop <- gsub("Maize","maize",d$crop)
  d$land_prep_method <- gsub("CT","conventional",d$land_prep_method)
  d$land_prep_method <- gsub("PBW","permanent beds",d$land_prep_method)
  d$yield[1:19] <- d$yield[1:19]*1000
  
  d$planting_date <- as.character(as.Date(d$planting_date))
  d$harvest_date <- as.character(as.Date(d$harvest_date))
  d$emergence_date  <- as.character(as.Date(d$emergence_date))

  d[d=="."] <- NA
  d$plant_height <- as.numeric(d$plant_height)
  d$flowering_days <- as.integer(d$flowering_days)
  d$maturity_days <- as.integer(d$maturity_days)
  d$dmy_storage <- as.numeric(d$dmy_storage)
  d$crop_price <- as.numeric(d$crop_price)/1000
  d$irrigation_number<- as.integer(d$irrigation_number)
  
  carobiner::write_files(path, meta, d)
}

