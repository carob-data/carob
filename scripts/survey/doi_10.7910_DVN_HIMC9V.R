# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
  
  "Land and water management practices like that of contour bunding, drip irrigation and shallow wells have been implemented by farmers over time in southern Mali. Farmers use these practices to improve yield performance and increase the household income. The impact of land and water management practices was evaluated on different crops (sorghum, millet, maize, groundnut and cotton) grown in two agro-ecologies of southern Mali. The data collected complete the data on environmental domain (field experiment from 2015 to 2017). That data were collected from the four established technology parks in Bougouni and Koutiala."
  
  uri <- "doi:10.7910/DVN/HIMC9V"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
    data_organization = "ICRISAT",
    publication = NA,
    project = NA,
    data_type = "survey",
    treatment_vars = "none",
    response_vars = "none", 
    carob_completion = 100,
    carob_contributor = "Blessing Dzuda",
    carob_date = "2026-05-25",
    carob_effort = NA,
    notes = NA,
    design = NA
  )
  
  f <- ff[basename(ff) == "ImpactofLandWaterMgmt_data.xlsx"]
  r <- carobiner::read.excel(f)
  
  d <- data.frame(
    id = r$Number,
    country = "Mali",
    adm2 = r$District,
    adm3 = r$village,
    education = as.character(r$Education),
    field_size = r$Farmsize)
    
    
  crop <- data.frame(
    id = r$Number,
    country = "Mali",
    adm2 = r$District,
    adm3 = r$village,
    sorghum = r$Sorghumclass,
    maize = r$Maizeclass,
    millet = r$Milletclass,
    cotton = r$Cottonclass,
    groundnut = r$Groundnutclass)
  
  crop_vars <- c("sorghum", "maize", "millet", "cotton", "groundnut")
  
  long_list <- lapply(crop_vars, function(x){
    
    temp <- data.frame(
      id=crop$id,
      country = crop$country,
      adm2 = crop$adm2,
      adm3 = crop$adm3,
      crop = x,
      crop_code = crop[[x]])
    
    temp <- temp[temp$crop_code != 0 & !is.na(temp$crop_code), ]})
  
  crop_long <- do.call(rbind, long_list)
  
  rownames(crop_long) <- NULL
  
  d <- merge(d,crop_long, by= c("id","country","adm2","adm3"))
  d <- subset(d, select = -crop_code)

  d$adm2 <- ifelse(d$adm2=="1", "Bougouni","Koutiala")
  
  adm3_labels <- c("1"="Madina","2"="Flola","3"="Dieba","4"="Sibirila","5"="M'pessoba",
                   "6"="Nampossela","7"="Zanzoni","8"="Sirakele","9"="N'golonianasso")
  
  d$adm3 <- adm3_labels[as.character(d$adm3)]
  colnames(d)[1] <- "hhid"
  d$hhid <- as.character(d$hhid)
  
  lat_lon <- data.frame(
    adm3= c("Madina", "Flola", "Dieba", "Sibirila", "M'pessoba"),
    longitude= c(-7.6803, -7.6426, -7.9308, -7.7701, -5.7207),
    latitude= c(11.3472, 11.4201, 11.5132, 11.4265, 12.6655))
  
  d <- merge(d, lat_lon, by= "adm3" , all.x =TRUE)
  
  d$trial_id <- paste(d$hhid, d$adm3, sep = "_")
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  d$irrigated <-FALSE
  d$geo_from_source <- FALSE
  d$planting_date <- as.character(NA)
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
  d$yield <- as.numeric(NA)
  d$yield_part <- "grain"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- NA
  
  carobiner::write_files(path, meta, d)
  }
