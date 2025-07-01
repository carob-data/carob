# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"Grain legume crops are a significant component of the human diet and animal feed and have an important role in the environment, but the global diversity of agricultural legume species is currently underexploited. Experimental assessments of grain legume performances are required, to identify potential species with high yields. Here, we introduce a dataset including results of field experiments published in 173 articles. The selected experiments were carried out over five continents on 39 grain legume species. The dataset includes measurements of grain yield, aerial biomass, crop nitrogen content, residual soil nitrogen content and water use. When available, yields for cereals and oilseeds grown after grain legumes in the crop sequence are also included. The dataset is arranged into a relational database with nine structured tables and 198 standardized attributes. Tillage, fertilization, pest and irrigation management are systematically recorded for each of the 8,581 crop*field site*growing season*treatment combinations. The dataset is freely reusable and easy to update. We anticipate that it will provide valuable information for assessing grain legume production worldwide."
  
    uri <- "doi:10.5061/dryad.mf42f"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
      data_organization = "UPS", #Université Paris-Saclay 
      publication="doi:10.1038/sdata.2016.84", 
      project=NA, 
      data_type= "experiment", 
      treatment_vars= "N_fertilizer; P_fertilizer; K_fertilizer; land_prep_method; variety", 
      response_vars = "fw_yield", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-06-28",
      completion=100,
      notes=NA
   )
   
   
   f1 <- ff[basename(ff) == "Crop_Sequence_Trt.csv"]
   f2 <- ff[basename(ff) == "Crop.csv"]
   f3 <- ff[basename(ff) == "Fertilization.csv"]
   f4 <- ff[basename(ff) == "Irrigation.csv"]
   f5 <- ff[basename(ff) == "Site.csv"]
   f6 <- ff[basename(ff) == "Tillage.csv"]
   f7 <- ff[basename(ff) == "Weed_Insect_Fungi.csv"]
   
   
   ### Process Crop_Sequence_Trt.csv file 
   
   r1<- read.csv(f1)
   
   d1 <- data.frame(
      IDRotation= r1$IDRotation,
      treatment= r1$Crop_Sequence_Trt_Name,
      crop= tolower(r1$Crop_Sequence_Trt_Species_Order),
      variety= gsub(" ","", ifelse(grepl("Alexia|Express|Makwa|Rosetown", r1$Crop_Sequence_Trt_Cultivar_Name), NA,  r1$Crop_Sequence_Trt_Cultivar_Name)),
      #season= as.character(r1$Crop_Sequence_Trt_Growing_Season_Number),
      site_id= r1$IDSite_Site
   )
   
   
   ### Process crop file 
   
   r2 <- read.csv(f2)
   
   d2 <- data.frame(
      IDRotation= r2$IDRotation_CropSystem,
      IDCrop_Crop= r2$IDCrop,
      treatment= r2$Crop_Sequence_Treatment_Name,
      crop= tolower(r2$Crop_Species_Common_Name),
      planting_date= substr(gsub("NA |\\.", "", r2$Crop_Date_Seeding), 1, 11),
      harvest_date= substr(gsub("NA |\\.", "", r2$Crop_Date_Harvest), 1, 11),
      #harvest_days= as.numeric(gsub(" or 212| or 222| or 165", "", r2$Crop_Date_From_Seeding_To_Harvest_Day_Number)),
      rep= r2$Crop_Replicate_Number,
      fw_yield= r2$Crop_Yield_Grain,
      yield_moisture= r2$Crop_Yield_Grain_DM_Percentage*100,
      fwy_total= r2$Crop_Biomass_Aerial,
      harvest_index= r2$Crop_Harvest_Index,
      grain_N= r2$Crop_N_Quantity_Grain*10,
      residue_N= r2$Crop_N_Quantity_Aerial,
      grain_protein= r2$Crop_Protein_Quantity_Percentage_Grain,
      on_farm= TRUE,
      is_survey=  FALSE,
      yield_part= "grain"
   )
   
   ### Fixing harvest and planting date
   
   ## Planting date
   d2$planting_date <-  gsub(" ", "-", carobiner::eng_months_to_nr(d2$planting_date))
   d2$planting_date <- sub("^(\\d{1})-(\\d{4})$", "0\\1-\\2",  d2$planting_date)
   d2$planting_date <- sub("^(\\d{2})-(\\d{1})-(\\d{4})$", "\\1-0\\2-\\3", d2$planting_date)
   d2$planting_date <- gsub("\\(between-ra|na-na-na|na-na|\\(rainy-seas|\\(after-rain|\\(autumn\\)-19|\\(spring\\)-19|\\(summer\\)-19|5-6-200|3-4-200|\\(winter\\)-19", NA, d2$planting_date)
   d2$planting_date <- gsub("-ma|-fe|-ja|-oc", "", d2$planting_date)
   d2$planting_date <- gsub("avr", "03", d2$planting_date)
   ### fixing date format (DD-MM-YY to YY-MM-DD and MM-YY to YY-MM)
   d2$planting_date <- ifelse( grepl("^\\d{2}-\\d{4}$", d2$planting_date), sub("^(\\d{2})-(\\d{4})$", "\\2-\\1", d2$planting_date),       
      ifelse( grepl("^\\d{2}-\\d{2}-\\d{4}$", d2$planting_date), sub("^(\\d{2})-(\\d{2})-(\\d{4})$", "\\3-\\2-\\1", d2$planting_date), NA))
   d2$planting_date <- gsub("1979-1980|1987-1988-1|1994-1995-1|1992-1993-1|3-2003|11-12-199|1983-1984|1986-11-31|1987-11-31"  , NA , d2$planting_date)
   
   # harvest date 
   d2$harvest_date <-  gsub(" ", "-", carobiner::eng_months_to_nr(d2$harvest_date))
   d2$harvest_date <- gsub("\\(between-ra|na-na-na|na-na|\\(rainy-seas|\\(after-rain|\\(autumn\\)-19|\\(spring\\)-19|\\(summer\\)-19|5-6-200|\\(winter\\)-19|1986-1987-1", NA, d2$harvest_date)
   d2$harvest_date <- gsub("-ma|-fe|-ja|-oc", "", d2$harvest_date)
   d2$harvest_date <- sub("^(\\d{1})-(\\d{4})$", "0\\1-\\2",  d2$harvest_date)
   d2$harvest_date <- sub("^(\\d{2})-(\\d{1})-(\\d{4})$", "\\1-0\\2-\\3", d2$harvest_date)
   
   d2$harvest_date <- ifelse( grepl("^\\d{2}-\\d{4}$", d2$harvest_date), sub("^(\\d{2})-(\\d{4})$", "\\2-\\1", d2$harvest_date),       
                     ifelse( grepl("^\\d{2}-\\d{2}-\\d{4}$", d2$harvest_date), sub("^(\\d{2})-(\\d{2})-(\\d{4})$", "\\3-\\2-\\1", d2$harvest_date), NA))
   d2$harvest_date <- gsub("2008-2009-2|1996-or-199|8-9-200|1984-1985-1|1992-1993-1|1997-1998-1|2000-24-7|2000-14-8|11-12-200|1996-1997-1|1994-1995-1|1993-1994-1|1985-1986|1986-1987|9-10-200|1983-1984|1991-1992|1995-1996-1|1999-2000|1-2-199|5-6-199|1987-1988-1|1998-11-31", NA, d2$harvest_date)
   ### removing harvest date less than 45 days from planting date (harvest day for crop related is more than 90 days after planting so it's probably an mistake)
   d2$harvest_date <- ifelse(grepl("2007-08-06|1989-02-28|1989-02-10|1989-01-26|1989-02-09", d2$harvest_date), NA, d2$harvest_date)
   
   #### Merge d2 and d1
   d <- merge(d2, d1, by=c("IDRotation", "crop", "treatment"), all.x = TRUE)
   
   ### Process fertilizer file 
   
   r3 <- read.csv(f3)
   names(r3) <- gsub("Fertilization_NPK_Dose_Product_Name", "fertilizer_type",  names(r3))
   r3$fertilizer_type <- ifelse(grepl("Fertilzer formula|Fertilizer formula", r3$fertilizer_type), "NPK",
                        ifelse(grepl("Monoammonium phosphate", r3$fertilizer_type), "MAP",
                        ifelse(grepl("Potassium chloride|Potassium", r3$fertilizer_type), "KCl",
                        ifelse(grepl("Single super phosphate|Super phosphate", r3$fertilizer_type), "SSP",
                        ifelse(grepl("Double super phosphate", r3$fertilizer_type), "DSP",
                        ifelse(grepl("Potassium muriate", r3$fertilizer_type), "MOP",
                        ifelse(grepl("Ammonium nitrate", r3$fertilizer_type), "AN",
                        ifelse(grepl("Triple super phosphate", r3$fertilizer_type), "TSP",
                        ifelse(grepl("Diammonium phosphate", r3$fertilizer_type), "DAP",
                        ifelse(grepl("Calcium ammonium nitrate", r3$fertilizer_type), "CAN", r3$fertilizer_type))))))))))
   
   
   P <- carobiner::fix_name(r3$fertilizer_type)
   P <- gsub("Urea \\(46.00\\%N)|Urea|Urea \\(46.40\\%N)|NULL or Urea|urea \\(46.00\\%N)|urea \\(46.40\\%N)", "urea", P) 
   P <- gsub("Potassium nitrate","KNO", P)
   P <- gsub("Potassium sulphate","SOP", P)
   P <- gsub("Urea and Ammonium sulphate","urea; DAS", P)
   P <- gsub("Ammonium sulphate","DAS", P)
   P <- gsub("K2O|P2O5","NPK", P)
   P <- gsub("Anhydrous ammonia","unknown", P)
   P <- gsub("urea and DAS|DAS and urea","urea; DAS", P)
   P <- gsub("NULL","none", P)
   P <- gsub("NA and Manure \\(1.80\\%N)","none", P)
   r3$fertilizer_type <- P
   
   
   N_fert <- r3[grepl("N", r3$Fertilization_NPK), ]
   names(N_fert) <- gsub("^Fertilization_NPK_Dose$", "N_fertilizer", names(N_fert))
   names(N_fert) <- gsub("fertilizer_type", "fertilizer_type1", names(N_fert))
   N_fert <- N_fert[, c("N_fertilizer", "fertilizer_type1", "IDCrop_Crop")]
   N_fert$N_fertilizer <- gsub(" and 3000.00|NA and 10.00| and 10.00| and 35.00| and 28.00| and 59.00| and 60.00| or 7.00 or 15.00 or 22.00 or 29.00 or 44.00 and 20.00| or 50.00| or 40.00 or 80.00| or 60.00", NA, N_fert$N_fertilizer)
   N_fert$N_fertilizer <- ifelse(grepl("DAS", N_fert$fertilizer_type1 ), as.numeric(N_fert$N_fertilizer)*0.21, 
                                                 ifelse(grepl("MAP", N_fert$fertilizer_type1), as.numeric(N_fert$N_fertilizer)*0.11, 
                                                 ifelse(grepl("CAN", N_fert$fertilizer_type1), as.numeric(N_fert$N_fertilizer)*0.26,
                                                 ifelse(grepl("urea", N_fert$fertilizer_type1), as.numeric(N_fert$N_fertilizer)*0.46,
                                                 ifelse(grepl("AN", N_fert$fertilizer_type1), as.numeric(N_fert$N_fertilizer)*0.34, as.numeric(N_fert$N_fertilizer)))))) 
   
   
   P_fert <- r3[grepl("P", r3$Fertilization_NPK), ]
   names(P_fert) <- gsub("^Fertilization_NPK_Dose$", "P_fertilizer", names(P_fert))
   names(P_fert) <- gsub("fertilizer_type", "fertilizer_type2", names(P_fert))
   P_fert <- unique(P_fert[, c("P_fertilizer", "fertilizer_type2", "IDCrop_Crop")])
   P_fert$P_fertilizer <- gsub(" and 3000.00| or 30.00 or 90.00| or 30.00 or 60.00| or 17.80| or 7.00 or 15.00 or 22.00 or 29.00 or 44.00", NA, P_fert$P_fertilizer)
   P_fert$P_fertilizer <- ifelse(grepl("DAP", P_fert$fertilizer_type2 ), as.numeric(P_fert$P_fertilizer)*0.201, 
                          ifelse(grepl("MAP", P_fert$fertilizer_type2), as.numeric(P_fert$P_fertilizer)*0.52, 
                          ifelse(grepl("DSP", P_fert$fertilizer_type2), as.numeric(P_fert$P_fertilizer)*0.20,
                          ifelse(grepl("SSP",P_fert$fertilizer_type2), as.numeric(P_fert$P_fertilizer)*0.0874,
                          ifelse(grepl("TSP", P_fert$fertilizer_type2), as.numeric(P_fert$P_fertilizer)*0.1923, as.numeric(P_fert$P_fertilizer)))))) 
   
   K_fert <- r3[grepl("K", r3$Fertilization_NPK),]
   names(K_fert) <- gsub("^Fertilization_NPK_Dose$", "K_fertilizer", names(K_fert))
   names(K_fert) <- gsub("fertilizer_type", "fertilizer_type3", names(K_fert))
   K_fert <- unique(K_fert[, c("K_fertilizer", "fertilizer_type3", "IDCrop_Crop")])
   K_fert$K_fertilizer <- gsub(" and 3000.00", NA, K_fert$K_fertilizer )
   K_fert$K_fertilizer <- as.numeric(K_fert$K_fertilizer)/1.2051

   
   #### merge N_fert, P_fert, K_fert data 
   
   d3 <- merge(P_fert, K_fert, by="IDCrop_Crop", all= TRUE)
   
   d3 <- merge(N_fert, d3, by="IDCrop_Crop", all= TRUE)
   
   d3$fertilizer_type <- gsub("; NA|; none; none|; NA; NA|none; |NA; |; none", "", paste(d3$fertilizer_type1, d3$fertilizer_type2, d3$fertilizer_type3, sep = "; "))
   
   d3$fertilizer_type1 <- d3$fertilizer_type2 <- d3$fertilizer_type3 <- NULL
   d3$fertilizer_type[grepl("NA", d3$fertilizer_type)] <- "none"
   
   ### merge d3 and d 
   
   d <- merge(d, d3, by= "IDCrop_Crop", all.x = TRUE)
   
   
   
   ### Process Irrigation file 
   
   r4 <- read.csv(f4)
   
   d4 <- unique(data.frame(
      irrigation_amount= as.numeric(gsub(" or NA", "",  r4$Irrigation_Presence_Irrigation_Dose)),
      irrigation_method= r4$Irrigation_Presence_Irrigation_Method,
      IDCrop_Crop= r4$IDCrop_Crop,
      irrigated= ifelse(grepl("1", r4$Irrigation_Presence_Irrigation), TRUE, FALSE)
   ))  
   
   P <- carobiner::fix_name(d4$irrigation_method)
   P <- gsub("Alternate furrow|Conventional furrow|Furrow","furrow", P)
   P <- gsub("Canal or Tube","unknown", P)
   P <- gsub("Drip","drip", P)
   P <- gsub("Flood","flood", P)
   P <- gsub("NULL|NULL or Gradient level 1 or Gradient level 2 or Fully irrigated","none", P)
   P <- gsub("Sprinkler","sprinkler", P)
   P <- gsub("T tape","unknown", P)
   P <- gsub("Watering can","unknown", P)
   d4$irrigation_method <- P
   ### merge d and d4
   d <- merge(d, d4, by="IDCrop_Crop", all.x = TRUE) 
   
   ### Process Site file 
   
   r5 <- suppressWarnings(read.csv(f5, header = TRUE))
  
   d5 <- data.frame(
      site_id= r5$IDSite,
      location= r5$Site_Name,
      country= r5$Site_Country,
      adm1= r5$Site_City_State_Region,
      latitude= r5$Site_Latitude,
      longitude= r5$Site_Longitude,
      geo_from_source= TRUE,
      soil_depth= gsub("\\.", "-", r5$Site_Soil_Depth_Variable_m),
      #r5$Site_Soil_Classification_Name,
      soil_type= r5$Site_Soil_Texture_Name,
      soil_sand= r5$Site_Soil_Sand_Percentage,
      soil_silt= r5$Site_Soil_Silt_Percentage,
      soil_clay= r5$Site_Soil_Clay_Percentage,
      soil_pH = ifelse(grepl("H2O", r5$Site_Soil_pH_Basis), r5$Site_Soil_pH, NA),
      soil_pH_CaCl2= ifelse(grepl("CaCl2|Ca", r5$Site_Soil_pH_Basis), r5$Site_Soil_pH, NA),
      soil_pH_KCl= ifelse(grepl("KCl", r5$Site_Soil_pH_Basis), r5$Site_Soil_pH, NA),
      soil_SOM= r5$Site_Soil_Organic_Matter_Percentage,
      soil_N= as.numeric(gsub(" \\(only to 0.10 m depth\\)", "", r5$Site_Soil_N_Percentage)),
      soil_C= as.numeric(gsub(" \\(only to 0.10 m depth\\)", "", r5$Site_Soil_C_Percentage)),
      rain= r5$Site_Precipitation_mm,
      #season= r5$Site_Precipitation_Period,
      year= gsub("from ", "",  r5$Site_Precipitation_Period_Year),
      month= r5$Site_Temperature_Period_Month,
      temp= r5$Site_Temperature_Celsius
   )
   
   i <- grepl("Canada", d5$country) & grepl("Indian Head", d5$location) 
   d5$longitude[i] <- -103.669
   d5$latitude[i] <- 50.533
   d5$geo_from_source[i] <- FALSE
   
   i <- grepl("Canada", d5$country) & grepl("Saint Johns", d5$location) 
   d5$longitude[i] <- -52.7531
   d5$latitude[i] <- 47.5562
   d5$geo_from_source[i] <- FALSE
   
   i <- grepl("Italy", d5$country) & grepl("Pietranera", d5$location) 
   d5$longitude[i] <- 9.3049
   d5$latitude[i] <- 44.5836
   d5$geo_from_source[i] <- FALSE
   
   i <- grepl("Syria", d5$country) & grepl("Breda", d5$location) 
   d5$longitude[i] <- 36.238
   d5$latitude[i] <- 33.551
   d5$geo_from_source[i] <- FALSE
   
   i <- grepl("Syria", d5$country) & grepl("Tel Hadya", d5$location) 
   d5$longitude[i] <- 36.936
   d5$latitude[i] <- 35.9917 
   d5$geo_from_source[i] <- FALSE
   
  
   i <- grepl("Uganda", d5$country) & grepl("Tororo", d5$location) 
   d5$longitude[i] <- 34.185
   d5$latitude[i] <- 0.679
   
   i <- grepl("Uganda", d5$country) & grepl("Abi", d5$location) 
   d5$longitude[i] <- 30.948
   d5$latitude[i] <- 3.08821 
   d5$geo_from_source[i] <- FALSE
   
   i <- grepl("Lebanon", d5$country) & grepl("Beirut", d5$location) 
   d5$longitude[i] <-  35.5018
   d5$latitude[i] <- 33.8950
   d5$geo_from_source[i] <- FALSE
   
   i <- grepl("Uzbekistan", d5$country) & grepl("Fergana Valley", d5$location) 
   d5$longitude[i] <- 71.798
   d5$latitude[i] <- 40.37373
   d5$geo_from_source[i] <- FALSE
   ## merge d and d5 
   
   d <- merge(d, d5, by= "site_id", all.x = TRUE, incomparables = NA)
   
   
   P <- carobiner::fix_name(d$crop)
   P <- gsub("bambarra groundnut", "bambara groundnut", P)
   P <- gsub("blister vetch|cyprus vetch|garden vetch|hungarian vetch|oneflower vetch|purple broad vetch|purple vetch|winter vetch", "vetch", P)
   P <- gsub("common ", "", P)
   P <- gsub("corn", "maize", P)
   P <- gsub("cereal rye", "rye", P)
   P <- gsub("cicercha purpurina|garden pea|red pea|sicklefruit fenugreek|white pea|yellow pea", "pea", P)
   P <- gsub("blue lupine", "blue lupin", P)
   P <- gsub("brassica campestris|rape", "rapeseed", P)
   P <- gsub("castorbean|hyacinthbean|moth bean|sieva bean", "common bean", P)
   P <- gsub("field mustard", "mustard", P)
   P <- gsub("fababean", "faba bean", P)
   P <- gsub("lupinus atlanticus|narrowleaf lupine", "white lupin", P)
   P <- gsub("hairy yellow vetch", "hairy vetch", P)
   P <- gsub("mungbean", "mung bean", P)
   P <- gsub("peanut", "groundnut", P)
   P <- gsub("pigeonpea", "pigeon pea", P)
   P <- gsub("white lupine", "white lupin", P)
   P <- gsub("sweet tarwi", "tarwi", P)
   P <- gsub("oat", "oats", P) 
   P <- gsub("fallow", "unknown", P) 
   P <- gsub("yellow lupine", "yellow lupin", P) 
   P <- gsub("horsegram", "lentil", P) ## not sure
   d$crop <- P
   d$country[grepl("United States of America", d$country)] <- "United States"
#####  Process tillage file
 r6 <- read.csv(f6)
  
 d6 <- data.frame(
    land_prep_method= ifelse(grepl("no till|seeding|Seeding", r6$Tillage_Presence_Tillage_Tool), "minimum tillage",
                      ifelse(grepl("chisel plow|rototiller", r6$Tillage_Presence_Tillage_Tool), "tillage",
                      ifelse(grepl("rotovator", r6$Tillage_Presence_Tillage_Tool), "rotovating",
                      ifelse(grepl("hoe", r6$Tillage_Presence_Tillage_Tool), "hoeing",
                      ifelse(grepl("moldboard plow|plow|till|harrow", r6$Tillage_Presence_Tillage_Tool), "tillage",
                      ifelse(grepl("with hand", r6$Tillage_Presence_Tillage_Tool), "manual",
                      ifelse(grepl("disc", r6$Tillage_Presence_Tillage_Tool), "disk tillage",
                      ifelse(grepl("puddle", r6$Tillage_Presence_Tillage_Tool), "puddled", 
                      ifelse(grepl("burn", r6$Tillage_Presence_Tillage_Tool), "burn tillage", "unknown"))))))))),
    row_spacing= as.numeric(gsub(" or 100.0| or 36.0| or 36.0 or 100.0", "", r6$Tillage_Seeding_Row_Inter)),
    plant_spacing= r6$Tillage_Seeding_Row_Intra,
    seed_density= as.numeric(gsub(" or 75 or 90", "", r6$Tillage_Seeding_Density))*10000,
    inoculated= ifelse(grepl("1", r6$Tillage_Seeding_Inoculation), TRUE, FALSE),
    IDCrop_Crop= r6$IDCrop_Crop
 )
 
 ### merge d and d6
 df <- merge(d, d6, by="IDCrop_Crop", all = TRUE)
 
 ### Process Fungicide file 
 
 r7 <- read.csv(f7, na=c("NULL", "NA"))
 r7$Weed_Insect_Fungi_Presence_Treatment_Chemical_Dose <- gsub(" or 104.00| or 600.00| or 450.00| or 212.00","", r7$Weed_Insect_Fungi_Presence_Treatment_Chemical_Dose)
 d7 <- data.frame(
    ###herbicide 
    herbicide_used= r7$Weed_Insect_Fungi_Presence_Weed,
    herbicide_amount= as.numeric(ifelse(grepl("1", r7$Weed_Insect_Fungi_Presence_Weed), r7$Weed_Insect_Fungi_Presence_Treatment_Chemical_Dose, NA)),
    herbicide_product=  tolower(ifelse(grepl("1", r7$Weed_Insect_Fungi_Presence_Weed), r7$Weed_Insect_Fungi_Presence_Treatment_Chemical_Dose_Product_Name, NA)),
    
    #### insecticide
    insecticide_used= r7$Weed_Insect_Fungi_Presence_Insect,
    insecticide_amount= as.numeric(ifelse(grepl("1", r7$Weed_Insect_Fungi_Presence_Insect), r7$Weed_Insect_Fungi_Presence_Treatment_Chemical_Dose, NA)),
    insecticide_product= tolower(ifelse(grepl("1", r7$Weed_Insect_Fungi_Presence_Insect), r7$Weed_Insect_Fungi_Presence_Treatment_Chemical_Dose_Product_Name, NA)),
    
    ### fungicide
    fungicide_used= r7$Weed_Insect_Fungi_Presence_Fungi,
    fungicide_amount= as.numeric(ifelse(grepl("1", r7$Weed_Insect_Fungi_Presence_Fungi), r7$Weed_Insect_Fungi_Presence_Treatment_Chemical_Dose, NA)),
    fungicide_product= tolower(ifelse(grepl("1", r7$Weed_Insect_Fungi_Presence_Fungi), r7$Weed_Insect_Fungi_Presence_Treatment_Chemical_Dose_Product_Name, NA)),
    IDCrop_Crop= r7$IDCrop_Crop,
    id= r7$IDPDW
 ) 
 cols <- c("fungicide_product", "insecticide_product", "herbicide_product")
 d7 <- d7[!apply(d7[cols], 1, function(row) all(is.na(row))), ]
 d7$herbicide_used <- ifelse(grepl("1", d7$herbicide_used), TRUE, FALSE)
 d7$insecticide_used <- ifelse(grepl("1", d7$insecticide_used), TRUE, FALSE)
 d7$fungicide_used <- ifelse(grepl("1", d7$fungicide_used), TRUE, FALSE)
 

 ### merge d7 and df
 df <- merge(d7, df, by="IDCrop_Crop", all.x = TRUE)
 
 
 ### Keep  only data with site information 
 df <- df[!is.na(df$site_id),]  
 
 ### Fixing herbicide (some check is still needed for unknown name)
 P <- carobiner::fix_name(df$herbicide_product)
 P <- gsub("2-4-d", "2,4-D", P)
 P <- gsub("ally", "mesosulfuron-methyl", P)
 P <- gsub("aryloxyphenoxypropionate or cyclohexanedione", "bensulfuron", P)
 P <- gsub("assist|assure ii", "unknown", P)
 P <- gsub("basagran|bentazon", "bentazon, sodium salt", P)
 P <- gsub("bladex \\(cyanazine\\)", "cyanazine", P)
 P <- gsub("broadstrike \\(flumetsulam\\)", "florasulam", P)
 P <- gsub("bromoxynil and mcpa", "bromoxynil heptanoate", P)
 P <- gsub("buctril|mcpa", "bromoxynil heptanoate", P)
 P <- gsub("butachlor", "unknown", P)
 P <- gsub("carbetamide", "unknown", P) ## not sure
 P <- gsub("chlorothalonil", "unknown", P)
 P <- gsub("cypermethrin and diquat and dibromide|diquat|paraquat|paraquat and diquat|paraquat and diquat or glyphosate|paraquat or diquat or glyphosate", "diquat dibromide", P)
 P <- gsub("diclofop|hoegrass", "unknown", P)
 P <- gsub("difenzoquat", "unknown", P)
 P <- gsub("diflufenican", "diflufenican", P)
 P <- gsub("fusilade or haloxyfop|gallant \\(haloxyfop\\)|verdict \\(haloxyfop\\) or fusilade", "halosulfuron-methyl", P)
 P <- gsub("flurochloridon|fusilade|haloxyfop|quizalofop", "halosulfuron-methyl", P)
 P <- gsub("glean", "unknown", P)
 P <- gsub("imazamox", "imazamox, ammonium salt", P)
 P <- gsub("imazethapyr", "imazethapyr, ammonium salt", P)
 P <- gsub("pursuit \\(mazethapyr, ammonium salt\\)", "imazethapyr, ammonium salt", P)
 P <- gsub("metamitron", "unknown", P)
 P <- gsub("methabenzthiazuron", "unknown", P)
 P <- gsub("prometryne", "unknown", P)
 P <- gsub("prowl \\(pendimethalin\\)", "pendimethalin", P)
 P <- gsub("pyridate", "unknown", P)
 P <- gsub("roundup \\(glyphosate\\)", "glyphosate", P)
 P <- gsub("select \\(clethodim\\)", "clethodim", P)
 P <- gsub("sonalan", "clethodim", P)
 P <- gsub("stomp|stomp \\(pendimethalin\\)", "pendimethalin", P)
 P <- gsub("terbutryne|trebutryne", "terbutryn", P)
 P <- gsub("treflan|treflan \\(trifluralin\\)", "trifluralin", P)
 P <- gsub("trifluralin \\(trifluralin\\)", "trifluralin", P)
 P <- gsub("pursuit \\(imazethapyr, ammonium salt\\)", "imazethapyr, ammonium salt", P)
 P <- gsub("^bromoxynil$", "bromoxynil heptanoate", P)
 P <- gsub("diflufenican", "unknown", P)
 P <- gsub("sencor", "metribuzin", P)
 df$herbicide_product <- P

 ### Fixing insecticide (some check is still needed for unknown name)
 
 P <- carobiner::fix_name(df$insecticide_product)
 P <- gsub("graal|agral", "unknown", P)
 P <- gsub("benzene hexachloride", "gamma-cyhalothrin", P)
 P <- gsub("ddt", "unknown", P)
 P <- gsub("dipel", "unknown", P)
 P <- gsub("dursban \\(chlorpyrifos\\)", "chlorpyrifos", P)
 P <- gsub("fastac", "alpha-cypermethrin", P)
 P <- gsub("imidan", "phosmet", P)
 P <- gsub("karate", "lambda-cyhalothrin", P)
 P <- gsub("le mat \\(omethoate\\)", "unknown", P)
 P <- gsub("mancozeb", "none", P)## fungicide
 P <- gsub("monocrotophos", "unknown", P)
 P <- gsub("mancozebpyrethrin", "none", P)
 P <- gsub("pyrethroid", "cypermethrin", P)
 P <- gsub("sumathion", "tetramethrin", P)
 P <- gsub("thiodan", "endosulfan", P)
 P <- gsub("pyrethrin", "pyrethrins", P)
 df$insecticide_product <- P

 ### Fixing Fungicide (some check is still needed for unknown name)
 
 P <- carobiner::fix_name(df$fungicide_product) 
 P <- gsub("apron or crown", "metalaxyl", P)
 P <- gsub("apron", "metalaxyl", P)
 P <- gsub("bravo \\(chlorothalonil\\)", "chlorothalonil", P)
 P <- gsub("carbathiin", "carboxin", P)
 P <- gsub("crown", "metalaxyl", P)
 P <- gsub("dithane", "mancozeb", P)
 P <- gsub("dividend", "difenoconazole", P)
 P <- gsub("dursban \\(chlorpyrifos\\)", "chlorpyrifos", P)
 P <- gsub("germipro", "unknown", P)
 P <- gsub("helix", "difenoconazole", P)
 P <- gsub("lindane", "unknown", P)
 P <- gsub("ridomil", "metalaxyl", P)
 P <- gsub("rovral \\(iprodione\\)", "iprodione", P)
 P <- gsub("vitaflo", "thiram", P)
 P <- gsub("wakil \\(cymoxanil\\)", "cymoxanil", P)
 P <- gsub("wakil \\(fludioxonil\\)", "fludioxonil", P)
 P <- gsub("wakil \\(metalaxyl\\)", "metalaxyl", P)
 P <- gsub("wakil", "metalaxyl", P)
 df$fungicide_product <- P
 
 df$trial_id <- paste(df$location, "-", df$id)
 
 df$IDCrop_Crop <- df$IDRotation <- df$year <- df$month <- df$site_id <- df$id <- NULL
 
 
 carobiner::write_files(path, meta, df)
   
 
}

