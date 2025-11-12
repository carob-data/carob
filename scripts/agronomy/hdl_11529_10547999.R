# R script for "carob"

carobiner::on_github(uri="hdl:11529/10547999")
carob_script <- function(path) {
  
  "Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to exiting systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers."
  
  
  uri <- "hdl:11529/10547999"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
                                  data_organization = "CIMMYT",
                                  publication = NA,
                                  project = NA,
                                  data_type = "experiment",
                                  treatment_vars = "crop_rotation",
                                  response_vars = "yield;fwy_total", 
                                  completion = 100,
                                  carob_contributor = "Mitchelle Njukuya",
                                  carob_date = "2025-11-12",
                                  notes = NA, 
                                  design = NA
  )
  
  process <- function(f){
    ## site information  
    r <- carobiner::read.excel.hdr(f, sheet="2 - Site information", skip=4, fix_names = TRUE)
    d1 <- data.frame(
      year= r$Year,
      season= r$Season,
      crop_sys= r$Cropping.System,
      farmer_name= r$Farmer.s.name,
      location= r$Node,
      treatment= r$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
      plot_area= r$Plot.size.m2.each.plot,
      soil_texture= as.character(r$Soil.texture.sand.silt.clay.etc)
      
    )
    ## removing empty rows 
    d1 <- d1[!is.na(d1$year),] 
    
    r1 <- carobiner::read.excel.hdr(f, sheet = "3-  Land Prep Operations" , skip=4)
    names(r1)[names(r1) == "Tillage.and.sowing.operation.should.be.covered.here.Manual.sowing.tillage.transplanting.etc._0peration.1.Date.mm.dd.yy"] <- "planting_date"
    
    d2 <- data.frame(
      year= r1$Year,
      season= r1$Season,
      location= r1$Node,
      crop_sys= r1$Cropping.System,
      farmer_name= r1$Farmer.s.name,
      treatment= r1$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
      planting_method= r1$Operation.1.Name.of.operation,
      land_prep_implement= r1$X0peration.1.Implement.used,
      planting_date=r1$planting_date
      
    )
    
    d <- merge(d1, d2, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Stand counts & Phenology
    r2 <- carobiner::read.excel.hdr(f,  sheet = "4- Stand counts & Phenology", skip=4)
    names(r2) <- gsub("Date.of.harvest|Datw.of.harvest.dd.mm.yy","harvest_date", names(r2))
    
    d3 <- data.frame(
      year= r2$Year,
      season= r2$Season,
      location= r2$Node,
      crop= r2$Crop,
      crop_sys= r2$Cropping.System,
      farmer_name= r2$Farmer.s.name,
      treatment= r2$Tmnt,
      variety= r2$Variety,
      row_spacing= r2$Row.spacing.cm,
      emergence_date= as.character(r2$Date.of.100pct.plant.emergence.dd.mm.yy),
      flowering_date= as.character(r2$Date.of.50pct.first.flower.dd.mm.yy),
      maturity_date= as.character(r2$Date.of.80pct.physiological.maturity.dd.mm.yy),
      harvest_date= as.character(r2$harvest_date)
    )
    
    d <- merge(d, d3, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Crop Mgmt Operations
    r3 <- carobiner::read.excel.hdr(f,  sheet = "5- Crop Mgmt Operations", skip=4)
    names(r3) <- gsub("Crop.management.operations.includes.weeding.earthing.up.gap.fiiling.fertilizer.top.dressing.etc._0peration.1.Date.mm.dd.yy","Operation.1.Date.mm.dd.yy", names(r3))
    
    d4 <- data.frame(
      year= r3$Year,
      season= r3$Season,
      location= r3$Node,
      crop_sys= r3$Cropping.System,
      farmer_name= r3$Farmer.s.name,
      treatment= r3$Tmnt,
      fertilizer_date= as.character(r3$Operation.1.Date.mm.dd.yy),
      irrigation_dates= as.character(r3$X0peration.2.Date.mm.dd.yy)
    )
    
    d <- merge(d, d4, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Fertilizer amounts
    r4 <- carobiner::read.excel.hdr(f,   sheet = "6 - Fertilizer amounts ", skip=4, na=c(".kg.ha.kg.ha",""))
    
    d5 <- data.frame(
      year= r4$Year,
      season= r4$Season,
      location= r4$Node,
      crop_sys= r4$Cropping.System,
      farmer_name= r4$Farmer.s.name,
      treatment= r4$Tmnt,
      fertilizer_type= paste(r4$Fertilizer.1.Application_Product.used, r4$Fertilizer.2.Application_Product.used, r4$Fertilizer.3.Application_Product.used, sep =";"),
      fertilizer_amount= rowSums(r4[,c("DAP.kg.ha", "Calculation_Urea.kg.ha", "MOP.kg.ha","ZnSO4.kg.ha")]),
      fertilizer_price = r4$Fertilizer.cost.Tk.ha / rowSums(r4[, c("DAP.kg.ha", "Calculation_Urea.kg.ha", "MOP.kg.ha","ZnSO4.kg.ha")]), ##Tk/ha
      currency= "Tk",
      N_fertilizer= r4$N.kg.ha,
      P_fertilizer= r4$P.kg.ha,
      K_fertilizer= r4$K.kg.ha,
      Zn_fertilizer= r4$Zn.kg.ha
    
    )
    
    d <- merge(d,d5,by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Pesticide applications
    r5 <- carobiner::read.excel.hdr(f, sheet = "9 - Pesticide applications", skip=4)
    
    d6 <- data.frame(
      year= r5$Year,
      season= r5$Season,
      location= r5$Node,
      crop_sys= r5$Cropping.System,
      treatment= r5$Tmnt,
      farmer_name= r5$Farmer.s.name,
      herbicide_dates=  as.character(r5$Pesticide.application.1.herbicide.insecticide.or.fungicide._Date.of.application.dd.mm.yy),
      herbicide_product= r5$Product.applied,
      herbicide_implement= r5$Method.of.application,
      herbicide_amount= r5$Rate.of.application.g.or.ml.ha.4/1000 ## kg/ha
    )
    
    d <- merge(d, d6, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
    
    ## Grain Harvest
    r6 <- carobiner::read.excel.hdr(f, sheet = "14 - Grain Harvest ", skip=4)
    
    d7 <- data.frame(
      year= r6$Year,
      season= r6$Season,
      location= r6$Node,
      crop_sys= r6$cropping.System,
      farmer_name= r6$Farmer.s.name,
      treatment= r6$Tmnt,
      plot_area= r6$Plot.size.m2,
      fwy_total= r6$Biomass.t.ha*1000, ## kg/ha
      harvest_index= r6$HI,
      yield=r6$GY.ton.ha*1000
    )
    
    d <- merge(d, d7, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name", "plot_area"), all.x = TRUE)
  }
  
  f <- ff[basename(ff) == "1-Rabi 2015-16-ACS- all nodes -Malda.xlsx"]
  d <- process(f)

  d$trial_id <- paste0(d$farmer_name, "_", d$location)
  
  d$crop_rotation <- "none" 
  d$crop_rotation[d$crop_sys=="R-L-Mun"] <- "rice;lentil;mung bean"
  d$crop_rotation[d$crop_sys=="R-L-R"] <- "rice;lentil;rice"
  d$crop_rotation[d$crop_sys=="R-BC-Mun"] <- "rice;black gram;mung bean"
  d$crop_rotation[d$crop_sys=="BG-L-J"] <- "black gram;lentil;jute"
  d$crop_rotation[d$crop_sys=="R-C-Mun"] <- "rice;coriander;mung bean"
  d$crop_rotation[d$crop_sys=="R-W-Mun"] <- "rice;wheat;mung bean"
  d$crop_rotation[d$crop_sys=="BG-W-J"] <- "black gram;wheat;jute"
  
  d$country <- "Bangladesh"
  d$irrigated <- TRUE
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  d$inoculated <- FALSE
  d$yield_part <- "grain"
  d$geo_from_source <- FALSE
  
  ## Fixing fertilizer type
  d$fertilizer_type <- gsub("Muriate", "KCl", d$fertilizer_type)
  d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
  
  ## Fixing insecticide variables 
  d$herbicide_implement <- gsub("SPRAY", "backpack sprayer", d$herbicide_implement)  
  d$herbicide_implement <- gsub("NA", "none", d$herbicide_implement)  
  d$herbicide_product <- "glyphosate;metribuzin;carfentrazone" 
  d$fungicide_product <- "trifloxystrobin;tebuconazole"
  
  ## Adding longitude and latitude 
  geo <- data.frame(
    location = c("Gourangpur", "Bidyanandapur", "Kalinagar"),
    latitude = c(23.6505, 24.0201, 23.2156),
    longitude = c(89.1663, 89.2984, 90.4792)
  )
  
  d <- merge(d, geo, by="location", all.x= TRUE)
  
  d$season <- paste0(d$season,"_",d$year)
  d$farmer_name <- d$crop_sys <- d$year <- NULL
  d$herbicide_used <- ifelse(is.na(d$herbicide_product)| grepl("none", d$herbicide_product), FALSE, TRUE)
  d$fertilizer_price <- as.character(d$fertilizer_price)
  
  # Clean up and convert correctly
  d$planting_date <- gsub(" UTC", "", d$planting_date)
  d$planting_method <- gsub("Seeding", "direct seeding", d$planting_method)
  d$crop <- gsub("BLACK CUMIN","cumin",d$crop) 
  d$crop <- gsub("CORIANDER","coriander",d$crop)
  d$crop <- gsub("LENTIL","lentil",d$crop)
  d$crop <- gsub("MUSTARD","mustard",d$crop)
  d$crop <- gsub("WHEAT","wheat",d$crop)
  
  d$yield_moisture <- 0
  d$season <- "rabi"
  d$row_spacing <- as.numeric(d$row_spacing)
  
  carobiner::write_files(path, meta, d)
}


