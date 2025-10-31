# R script for "carob"

carob_script <- function(path) {
  
  "Farmers’ participatory researchers managed long-term trials aimed at improving the productivity, profitability, and sustainability of smallholder agriculture in the EGP by addressing the following objectives: 1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear the risk and undertake technological innovation. 2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for smallholders. 3. Facilitate the widespread adoption of sustainable, resilient, and more profitable farming systems."
  
  uri <- "hdl:11529/10548371"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2, 
                                  data_organization = "CIMMYT;BARI;BRRI", 
                                  publication="doi.org/10.1016/j.jclepro.2019.118982", 
                                  project=NA, 
                                  data_type= "experiment", 
                                  response_vars= "dmy_storage,fwy_total", 
                                  treatment_vars = "variety;land_prep_method;crop_rotation", 
                                  carob_contributor= "Mitchelle Njukuya", 
                                  carob_date="2025-10-31"
  )
  
  ff <- ff[grep("xlsx",basename(ff))]
  
  process <- function(f){
    ## site information  
    r <- carobiner::read.excel.hdr(f, sheet="2 - Site information", skip=4, fix_names = TRUE)
    names(r) <- gsub("Type.of.trial", "Crop", names(r))
    d1 <- data.frame(
      year= r$Year,
      season= r$Season,
      crop_sys= r$Cropping.systems,
      farmer_name= r$Farmer.s.name,
      location= r$Node,
      crop= r$Crop,
      treatment= r$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
      plot_area= r$Plot.size.m2.each.plot,
      soil_texture= as.character(r$Soil.texture.sand.silt.clay.etc)
      
    )
    ## removing empty rows 
    d1 <- d1[!is.na(d1$year),] 
    
    r1 <- carobiner::read.excel.hdr(f, sheet = "3-  Land Prep Operations" , skip=4)
    
    #Rename columns
    names(r1)[names(r1) == "X0peration.5.Date.mm.dd.yy"] <- "transplanting_date"
    names(r1)[names(r1) == "Operation.5.Name.of.operation"] <- "planting_method"
    
    d2 <- data.frame(
      year= r1$Year,
      season= r1$Season,
      location= r1$Node,
      crop_sys= r1$Cropping.systems,
      farmer_name= r1$Farmer.s.name,
      treatment= r1$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
      land_prep_method= r1$Operation.1.Name.of.operation,
      land_prep_implement= r1$Operation.1.Implement.used,
      planting_method= r1$planting_method
      
    )
    
    d <- merge(d1, d2, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Stand counts & Phenology
    r2 <- carobiner::read.excel.hdr(f,  sheet = "4- Stand counts & Phenology", skip=4)
    names(r2) <- gsub("Date.of.harvest|Datw.of.harvest","harvest_date", names(r2))
    names(r2) <- gsub(".dd.mm.yy|.mm.dd.yy","", names(r2))
    
    d3 <- data.frame(
      year= r2$Year,
      season= r2$Season,
      location= r2$Node,
      crop_sys= r2$Cropping.systems,
      farmer_name= r2$Farmer.s.name,
      treatment= r2$Tmnt,
      variety= r2$Variety,
      row_spacing= r2$Row.spacing.cm,
      emergence_date= as.character(r2$Date.of.100pct.plant.emergence),
      flowering_date= as.character(r2$Date.of.50pct.first.flower),
      maturity_date= as.character(r2$Date.of.80pct.physiological.maturity),
      emergence_days= as.numeric(r2$X100pct.emergence.DAS),
      flowering_days= as.numeric(r2$X50pct.first.flowering.DAS),
      maturity_days= as.numeric(r2$X80pct.physiological.maturity.DAS),
      harvest_days= as.numeric(r2$Harvesting.DAS),
      planting_date= as.character(r2$Date.of.seeding),
      harvest_date= as.character(r2$harvest_date)
    )
    
    d <- merge(d, d3, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Crop Mgmt Operations
    r3 <- carobiner::read.excel.hdr(f,  sheet = "5- Crop Mgmt Operations", skip=4)
    names(r3) <- gsub("X0peration.1.Implement.used","Operation.1.Implement.used", names(r3))
    names(r3) <- gsub("X0peration.3.Date.mm.dd.yy","Operation.3.Date.mm.dd.yy", names(r3))
    names(r3) <- gsub("Crop.management.operations.includes.weeding.earthing.up.gap.fiiling.fertilizer.top.dressing.etc._Operation.1.Date.mm.dd.yy|Crop.management.operations.includes.weeding.earthing.up.gap.fiiling.fertilizer.top.dressing.etc._0peration.1.Date.mm.dd.yy","Operation.1.Date.mm.dd.yy", names(r3))
    
    d4 <- data.frame(
      year= r3$Year,
      season= r3$Season,
      location= r3$Node,
      crop_sys= r3$Cropping.systems,
      farmer_name= r3$Farmer.s.name,
      treatment= r3$Tmnt,
      fertilizer_date= r3$Operation.3.Date.mm.dd.yy,
      weeding_dates= as.character(r3$Operation.1.Date.mm.dd.yy),
      weeding_method= r3$Operation.1.Implement.used
    )
    
    d <- merge(d, d4, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Fertilizer amounts
    r4 <- carobiner::read.excel.hdr(f,   sheet = "6 - Fertilizer amounts ", skip=4, na=c(".kg.ha.kg.ha",""))
    names(r4)  <- gsub("K2O.kg.ha", "K", names(r4))
    names(r4)  <- gsub("N.kg.ha", "N", names(r4))
    names(r4)  <- gsub("P2O5.kg.ha", "P", names(r4))
    names(r4)  <- gsub("Gypsum.kg.ha", "Gypsum.Kg.ha.1", names(r4))
    names(r4)  <- gsub("Fertilizer.cost.Tk.ha|Calculation_Calculation_Fertilizer.cost.Tk.ha", "Calculation_Fertilizer.cost.Tk.ha", names(r4))
    names(r4)  <- gsub("Fertilizer.1.Application_P2O5.kg.haroduct.used", "Fertilizer.1.Application_Product.used", names(r4))
    names(r4)  <- gsub("Fertilizer.2.Application_P2O5.kg.haroduct.used","Fertilizer.2.Application_Product.used", names(r4))
    names(r4)  <- gsub("Fertilizer.3.Application_P2O5.kg.haroduct.used","Fertilizer.3.Application_Product.used", names(r4))
    names(r4)  <- gsub("Calculation_Urea.kg.ha","Urea.kg.ha", names(r4))
    names(r4) <- gsub("^Calculation_Calculation_", "Calculation_", names(r4))
    
    d5 <- data.frame(
      year= r4$Year,
      season= r4$Season,
      location= r4$Node,
      crop_sys= r4$Cropping.systems,
      farmer_name= r4$Farmer.s.name,
      treatment= r4$Tmnt,
      fertilizer_type= paste(r4$Fertilizer.1.Application_Product.used, r4$Fertilizer.2.Application_Product.used, r4$Fertilizer.3.Application_Product.used, sep =";"),
      fertilizer_amount= rowSums(r4[,c("TSP.kg.ha", "MOP.kg.ha", "Gypsum.Kg.ha","Urea.kg.ha","ZnSO4.kg.ha")]),
      fertilizer_price = r4$Calculation_Fertilizer.cost.Tk.ha / rowSums(r4[, c("TSP.kg.ha", "MOP.kg.ha", "Gypsum.Kg.ha","Urea.kg.ha","ZnSO4.kg.ha")]), ##Tk/ha
      currency= "Tk",
      N_fertilizer= r4$N,
      P_fertilizer= r4$P,
      K_fertilizer= r4$K,
      Zn_fertilizer= r4$ZnSO4.kg.ha.1,
      gypsum= r4$Gypsum.Kg.ha.1
    )
    
    d <- merge(d,d5,by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Pesticide applications
    r5 <- carobiner::read.excel.hdr(f, sheet = "9 - Pesticide applications", skip=4)
    
    d6 <- data.frame(
      year= r5$Year,
      season= r5$Season,
      location= r5$Node,
      crop_sys= r5$Cropping.systems,
      treatment= r5$Tmnt,
      farmer_name= r5$Farmer.s.name,
      herbicide_dates=  as.character(r5$Pesticide.application.1.herbicide.insecticide.or.fungicide._Date.of.application.dd.mm.yy),
      insecticide_dates= as.character(r5$Pesticide.application.2.herbicide.insecticide.or.fungicide._Date.of.application.dd.mm.yy),
      herbicide_product= r5$Product.applied,
      insecticide_product= r5$Product.applied.1,
      herbicide_implement= r5$Method.of.application,
      insecticide_implement= r5$Method.of.application.1,
      herbicide_amount= r5$Pesticides.g.or.ml.ha._P1/1000, ## kg/ha
      insecticide_amount= r5$P2/1000 ## kg/ha
    )
    
    d <- merge(d, d6, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Grain Harvest
    r6 <- carobiner::read.excel.hdr(f, sheet = "14 - Grain Harvest ", skip=4)
    names(r6) <- gsub("Calcultaion_Sun.dry.grain.wt.t.ha", "Sun.dry.grain.wt.t.ha", names(r6))
    names(r6) <- gsub("Calculation_Sun.dry.grain.wt.t.ha", "Sun.dry.grain.wt.t.ha", names(r6))
    names(r6) <- gsub("Biomass.t.Ha|Biomass", "Biomass.t.ha", names(r6))
    # Handle datasets that may or may not have Straw.yield.t.ton
    if ("Straw.yield.t.ton" %in% names(r6)) {
      r6$dmy_residue <- r6$Straw.yield.t.ton * 1000  # kg/ha
    } else {
      r6$dmy_residue <- NA  
    }
    
    d7 <- data.frame(
      year= r6$Year,
      season= r6$Season,
      location= r6$Node,
      crop_sys= r6$Cropping.systems,
      farmer_name= r6$Farmer.s.name,
      treatment= r6$Tmnt,
      plot_area= r6$Plot.size.m2,
      dmy_storage= r6$Sun.dry.grain.wt.t.ha*1000,## kg/ha
      seed_weight= r6$X1000.grain.weight.g,
      fwy_total= r6$Biomass.t.ha*1000, ## kg/ha
      harvest_index= r6$HI,
      dmy_residue= as.numeric(r6$dmy_residue)
    )
    
    d <- merge(d, d7, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name", "plot_area"), all.x = TRUE)
  }
  
  d <- lapply(ff, process)
  d <- do.call(rbind, d) 
  
  d$trial_id <- paste0(d$farmer_name, "_", d$location)
  
  d$crop_rotation <- ifelse(d$crop_sys=="R-M","mung bean","wheat;jute") 
  d$crop <- "rice"
  
  d$country <- "Bangladesh"
  d$irrigated <- TRUE
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  d$inoculated <- FALSE
  d$yield_part <- "grain"
  d$geo_from_source <- FALSE
  
  ## Fixing fertilizer type
  d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type)
  d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
  
  ## Fixing insecticide variables 
  d$herbicide_implement <- gsub("Spray", "backpack sprayer", d$herbicide_implement)  
  d$herbicide_implement <- gsub("NA", "none", d$herbicide_implement)  
  
  d$herbicide_product <-  gsub("Round up","glyphosate", d$herbicide_product)
  
  ## Adding longitude and latitude 
  
  geo <- data.frame(
    location=c("Borodarga", "Mohanpur", "Kolkonda" , "Lakkhitari", "Durgapur"), 
    latitude=c(25.7011, 25.9342, 25.8683, 25.88404, 25.55461), 
    longitude=c(89.3436, 88.6928, 89.2044, 89.25188, 89.29195)
  )
  d <- merge(d, geo, by="location", all.x= TRUE)
  
  ## Fixing treatment and land_prep
  d$treatment[d$treatment == "CTTPR-CTM"] <- "Conventional tillage transplanted puddle rice;maize"
  d$treatment[d$treatment == "CTTPR-CTW"] <- "Conventional tillage transplanted puddle rice;wheat"
  d$treatment[d$treatment == "CTTPR-STM"] <- "Conventional tillage transplanted puddle rice;Strip tillage maize"
  d$treatment[d$treatment == "CTTPR-STW"] <- "Conventional tillage transplanted puddle rice;Strip tillage wheat"
  d$treatment[d$treatment == "UPTPR-STM"] <- "Unpuddle transplanted rice;Strip tillage maize"
  d$treatment[d$treatment == "UPTPR-STW"] <- "Unpuddle transplanted rice;Strip tillage wheat"
  d$treatment[d$treatment == "DSR-STM"] <- "Direct seeded rice;Strip tillage maize"
  
  
  d$land_prep_method[grepl("Strip tillage", d$treatment)] <- "strip tillage"
  d$land_prep_method[grepl("Conventional tillage", d$treatment)] <- "conventional"
  #d$land_prep_method[grepl("Zero tillage", d$treatment)] <- "none"
  d$land_prep_method[grepl("Direct seeded", d$treatment)] <- "none"
  d$land_prep_method[grepl("Unpuddle", d$treatment)] <- "not puddled"
  
  d$land_prep_implement[grepl("power tiller|PTOS", d$land_prep_implement)] <- "unknown"
  
  d$season <- paste0(d$season,"_",d$year)
  d$farmer_name <- d$crop_sys <- d$year <- NULL
  
  d$insecticide_used <- ifelse(is.na(d$insecticide_product)| grepl("none", d$insecticide_product), FALSE, TRUE)
  d$herbicide_used <- ifelse(is.na(d$herbicide_product)| grepl("none", d$herbicide_product), FALSE, TRUE)
  
  d$fertilizer_price <- as.character(d$fertilizer_price)
  
  # Clean up and convert correctly
  d$fertilizer_date <- gsub(" UTC", "", d$fertilizer_date)
  d$herbicide_product <- gsub("Roundup", "glyphosate", d$herbicide_product)
  d$insecticide_implement <- gsub("Spray", "backpack sprayer", d$insecticide_implement)
  d$land_prep_method <- gsub("Tilling", "tillage", d$land_prep_method)
  d$planting_method <- gsub("Seeding", "direct seeding", d$planting_method)
  d$planting_method <- gsub("Transplanting|Transplanting, seedling raising and uprooting", "transplanting", d$planting_method)
  
  d_clean <- d[!duplicated(d), ]
  d <- d_clean
  
  
  carobiner::write_files(path, meta, d)
}


