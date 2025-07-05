# R script for "carob"

carob_script <- function(path) {
   
"Farmers participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives: 1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation. 2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders. 3. Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems. (2018-02-16) "
   
   uri <- "hdl:11529/10547963"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1, 
      data_organization = "CIMMYT;BARI;BRRI", 
      publication="doi:10.1016/j.fcr.2019.04.005", 
      project=NA, 
      data_type= "experiment", 
      response_vars= "dmy_storage,fwy_total", 
      treatment_vars = "variety;land_prep_method;crop_rotation", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-08-26"
   )
   
   ff <- ff[grep("xlsx",basename(ff))]
   
   process <- function(f){
      ## site information  
      r <- carobiner::read.excel.hdr(f, sheet="2 - Site information", skip=5, fix_names = TRUE)
      names(r) <- gsub("Type.of.trial", "Crop", names(r))
      d1 <- data.frame(
         year= r$Year,
         season= r$Season,
         crop_sys= r$Systems,
         farmer_name= r$Farmer.s.name,
         location= r$Node,
         crop= r$Crop,
         treatment= r$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
         plot_area= r$Plot.size.m2.each.plot,
         land_type= r$X,
         soil_type= r$X.1
         
      )
      ## removing empty rows 
      d1 <- d1[!is.na(d1$year),] 
      

      ## process land prep operation
      sheetLP <- "3-  Land Prep Operations"
      if(grepl("Wheat & Maize 2015-16-LT-Dharampur-Rajshahi.", f)) sheetLP <- "3-Land prep"
      r1 <- carobiner::read.excel.hdr(f, sheet = sheetLP , skip=4)
      
      d2 <- data.frame(
         year= r1$Year,
         season= r1$Season,
         location= r1$Node,
         crop_sys= r1$Systems,
         farmer_name= r1$Farmer.s.name,
         treatment= r1$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
         land_prep_method= r1$Operation.1.Name.of.operation,
         #land_prep_dates= r1$Tillage.and.sowing.operation.should.be.covered.here.Manual.sowing.tillage.transplanting.etc._0peration.1.Date.mm.dd.yy,
         land_prep_implement= r1$X0peration.1.Implement.used
      )
      
      d <- merge(d1, d2, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
      
      ## Stand counts & Phenology
      r2 <- carobiner::read.excel.hdr(f,  sheet = "4- Stand counts & Phenology", skip=4)
      names(r2) <- gsub("Date.of.harvest|Datw.of.harvest","harvest_date", names(r2))
      names(r2) <- gsub(".dd.mm.yy|.mm.dd.yy","", names(r2))
      names(r2) <- gsub("variety","Variety", names(r2))
      names(r2) <- gsub("X","Systems", names(r2))
      names(r2) <- gsub("Systems.6", "Node", names(r2))
      if (is.null(r2$Date.of.100pct.plant.emergence)) r2$Date.of.100pct.plant.emergence <- NA
      if (is.null(r2$Date.of.80pct.physiological.maturity)) r2$Date.of.80pct.physiological.maturity <- NA
      
      d3 <- data.frame(
         year= r2$Year,
         season= r2$Season,
         location= r2$Node,
         crop_sys= r2$Systems,
         farmer_name= r2$Farmer.s.name,
         treatment= r2$Tmnt,
         variety= r2$Variety,
         row_spacing= as.numeric(gsub("B'cast|Random", NA, r2$Row.spacing.cm)),
## not always available         
##         emergence_date=r2$Date.of.50pct.plant.emergence.mm.dd.yy,
         emergence_date= as.character(r2$Date.of.100pct.plant.emergence),
         flowering_date= as.character(r2$Date.of.50pct.anthesis),
         maturity_date= as.character(r2$Date.of.80pct.physiological.maturity),
## not always available
##       emergence_days= r2$X50pct.emergence.DA,
         emergence_days= r2$Systems100pct.emergence.DAS,
         #flowering_days= r2$Systems50pct.first.flowering.DAS,
         flowering_days= r2$Systems50pct.anthesis.DAS,
         maturity_days= as.numeric(r2$Systems80pct.physiological.maturity.DAS),
         harvest_days= r2$Harvesting.DAS,
         harvest_date= as.character(r2$harvest_date)
      )
      d3$row_spacing[d3$row_spacing == 0] <- NA
	  
      d <- merge(d, d3, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
      
      ## Crop Mgmt Operations
      r3 <- carobiner::read.excel.hdr(f,  sheet = "5- Crop Mgmt Operations", skip=4)
      
      d4 <- data.frame(
         year= r3$Year,
         season= r3$Season,
         location= r3$Node,
         crop_sys= r3$Systems,
         farmer_name= r3$Farmer.s.name,
         treatment= r3$Tmnt,
         planting_method= gsub("Fertilizer Broadcast", "broadcasting", r3$Operation.1.Name.of.operation),
         planting_implement= r3$X0peration.1.Implement.used,
         weeding_dates= as.character((r3$X0peration.2.Date.mm.dd.yy)),
         weeding_method= r3$Operation.2.Name.of.operation,
         weeding_implement= r3$X0peration.2.Implement.used
      )
      
      d <- merge(d, d4, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)
      
      ## Fertilizer amounts
      r4 <- carobiner::read.excel.hdr(f,   sheet = "6 - Fertilizer amounts ", skip=4, na=c(".kg.ha.kg.ha",""))
      names(r4)  <- gsub("K2O.kg.ha", "K", names(r4))
      names(r4)  <- gsub("N.kg.ha", "N", names(r4))
      names(r4)  <- gsub("P2O5.kg.ha", "P", names(r4))
      names(r4)  <- gsub("K2O5.kg.ha", "K", names(r4))
      names(r4)  <- gsub("Total.TSP2O5.kg.ha.kg.ha", "Total.TSP.kg.ha", names(r4))
      names(r4)  <- gsub("Total.MOP2O5.kg.ha.kg.ha", "Total.MOP.kg.ha", names(r4))
      names(r4)  <- gsub("Fertilizer.1.Application_P2O5.kg.haroduct.used", "Fertilizer.1.Application_Product.used", names(r4))
      names(r4)  <- gsub("Fertilizer.2.Application_P2O5.kg.haroduct.used","Fertilizer.2.Application_Product.used", names(r4))
      names(r4)  <- gsub("Fertilizer.3.Application_P2O5.kg.haroduct.used","Fertilizer.3.Application_Product.used", names(r4))
      names(r4)  <- gsub("N.kg.haode","Node", names(r4))
      
      d5 <- data.frame(
         year= r4$Year,
         season= r4$Season,
         location= r4$Node,
         crop_sys= r4$Systems,
         farmer_name= r4$Farmer.s.name,
         treatment= r4$Tmnt,
         fertilizer_type= paste(r4$Fertilizer.1.Application_Product.used, r4$Fertilizer.2.Application_Product.used, r4$Fertilizer.3.Application_Product.used, sep =";"),
         fertilizer_amount= rowSums(r4[,c("Total.TSP.kg.ha", "Total.MOP.kg.ha", "Gypsum.kg.ha")]),
         fertilizer_price = r4$Total.fertiliser.cost.Tk.ha / rowSums(r4[, c("Total.TSP.kg.ha", "Total.MOP.kg.ha", "Gypsum.kg.ha")]), ## BDT/kg
         currency= "BDT",
         N_fertilizer= r4$N,
         P_fertilizer= r4$P,
         K_fertilizer= r4$K,
         Zn_fertilizer= r4$ZnSO4.kg.ha.1,
         gypsum= r4$Gypsum.kg.ha.1
      )
      
      d <- merge(d,d5,by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name"), all.x = TRUE)

      ## Pesticide applications
      r5 <- carobiner::read.excel.hdr(f, sheet = "9 - Pesticide applications", skip=4)
      
      d6 <- data.frame(
         year= r5$Year,
         season= r5$Season,
         location= r5$Node,
         crop_sys= r5$Systems,
         treatment= r5$Tmnt,
         farmer_name= r5$Farmer.s.name,
         herbicide_dates=  as.character(gsub("11-19-15", "2015-11-19", r5$Pesticide.application.1.herbicide.insecticide.or.fungicide._Date.of.application.dd.mm.yy)),
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
      names(r6) <- gsub("Biomass.t.Ha", "Biomass.t.ha", names(r6))
      
      d7 <- data.frame(
         year= r6$Year,
         season= r6$Season,
         location= r6$Node,
         crop_sys= r6$Systems,
         farmer_name= r6$Farmer.s.name,
         treatment= r6$Tmnt,
         plot_area= r6$Plot.size.m2,
         dmy_storage= r6$Sun.dry.grain.wt.t.ha*1000,## kg/ha
         seed_weight= r6$X1000.grain.weight.g,
         fwy_total= r6$Biomass.t.ha*1000, ## kg/ha
         harvest_index= r6$HI
      )
      merge(d, d7, by=c("location", "crop_sys", "treatment", "year", "season", "farmer_name", "plot_area"), all.x = TRUE)
   }
   
   d <- lapply(ff, process)
   d <- do.call(rbind, d) 
 
   d$trial_id <- paste0(d$farmer_name, "_", d$location)
   d$planting_date <- as.character(as.Date(d$harvest_date)- d$harvest_days)
   
   d$crop_rotation <- ifelse(grepl("R-M", d$crop_sys), "rice",
                      ifelse(grepl("R-W-MB", d$crop_sys), "rice;mung bean",
                      ifelse(grepl("R-W", d$crop_sys), "rice", "rice"))) 
   
   d$country <- "Bangladesh"
   d$irrigated <- TRUE
   d$is_survey <- FALSE
   d$on_farm <- TRUE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   d$geo_from_source <- FALSE
   
## Fixing crop name 
   p <- carobiner::fix_name(d$crop,"lower")
   p <- gsub("boro rice", "rice",p)
   p <- gsub("rabi maize", "maize",p)
   d$crop <- p
   
## Fixing date variables
   d$insecticide_dates <- gsub("42353", NA, d$insecticide_dates)
   i <- grep("12-16-15|11-25-15|12-29-15|12-23-15|12-14-15|11-23-15", d$insecticide_dates)
   d$insecticide_dates[i] <- as.character(as.Date(d$insecticide_dates[i],"%m-%d-%y"))
   d$herbicide_dates <- gsub("42288", NA, d$herbicide_dates)
   d$weeding_dates <- gsub("42288|42407|42335", NA, d$weeding_dates)
   j <- grep("11-29-15|11-25-15|11-20-15|11-26-15|11-19-15", d$weeding_dates)
   d$weeding_dates[j] <- as.character(as.Date(d$weeding_dates[j],"%m-%d-%y"))
   
  
## Fixing fertilizer type
   d$fertilizer_type <- gsub("Muriate", "KCl", d$fertilizer_type)
   d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
## Fixing insecticide variables 
   d$herbicide_implement <- gsub("Spray", "backpack sprayer", d$herbicide_implement)  
   d$herbicide_implement <- gsub("NA", "none", d$herbicide_implement)  
   d$herbicide_implement <- gsub("Broadcast", "unknown", d$herbicide_implement)
   
   d$insecticide_implement <- gsub("Spray", "backpack sprayer", d$insecticide_implement)  
   d$insecticide_implement <- gsub("Broadcast", "unknown", d$insecticide_implement)
   
   d$herbicide_product <-  gsub("Round up","glyphosate", d$herbicide_product)
   d$herbicide_product[grep("Rifit", d$insecticide_product)] <- "glyphosate;pretilachlor" 
   d$herbicide_product[grep("Affinity|Afinity", d$insecticide_product)] <- "glyphosate;tribenuron-methyl"
   d$herbicide_amount[grep("Rifit|Affinity|Afinity", d$insecticide_product)] <-  d$herbicide_amount[grep("Rifit|Affinity|Afinity", d$insecticide_product)] + d$insecticide_amount[grep("Rifit|Affinity|Afinity", d$insecticide_product)]
   
   d$insecticide_amount[grep("Rifit|Affinity|Afinity", d$insecticide_product)] <- 0 
   d$insecticide_product <- gsub("Rifit", "none", d$insecticide_product)
   d$insecticide_product <- gsub("Regent", "fipronil", d$insecticide_product)
   d$insecticide_product <- gsub("Affinity|Afinity", "none", d$insecticide_product)
   d$insecticide_product <- gsub("Karata|karate", "lambda-cyhalothrin", d$insecticide_product)

## Fixing weeding 
   d$weeding_implement[grepl("Hand|Manual", d$weeding_implement)] <- "manual"
   d$weeding_implement[grepl("Sprayer", d$weeding_implement)] <- "unknown"
   
## Adding longitude and latitude 
   
   geo <- data.frame(
      location = c("Dharampur", "Nabinagar", "Nabinagr", "Baduria", "Bijoy Nagar", "Laxmipur"),
      latitude= c(24.8790, 23.8846, 23.8846, 24.3425, 22.2496, 22.9445),
      longitude= c(89.3533, 90.9699, 90.9699, 88.7201, 91.8252, 90.8276)
   )
   d$geo_from_source <- FALSE
   d <- merge(d, geo, by="location", all.x= TRUE)

## Fixing treatment and land_prep
   d$treatment[grepl("CTTPR", d$treatment)] <- "Conventional tillage transplanted puddle rice"
   d$treatment[grepl("CTW", d$treatment)] <- "Conventional tillage wheat"
   d$treatment[grepl("STW", d$treatment)] <- "Strip tillage wheat"
   d$treatment[grepl("DSR", d$treatment)] <- "Direct seeded rice"
   d$treatment[grepl("STM", d$treatment)] <- "Strip tillage maize"
   d$treatment[grepl("ZTM", d$treatment)] <- "Zero tillage wheat"
   d$treatment[grepl("ZTW", d$treatment)] <- "Zero tillage wheat"
   d$treatment[grepl("UPTPR", d$treatment)] <- "Unpuddle transplanted rice"
   
   d$land_prep_method[grepl("Strip tillage", d$treatment)] <- "strip tillage"
   d$land_prep_method[grepl("Conventional tillage", d$treatment)] <- "conventional"
   d$land_prep_method[grepl("Zero tillage", d$treatment)] <- "none"
   d$land_prep_method[grepl("Direct seeded", d$treatment)] <- "none"
   d$land_prep_method[grepl("Unpuddle", d$treatment)] <- "not puddled"
   
   d$land_prep_implement[grepl("Strip till seeder", d$land_prep_implement)] <- "direct seeder"
   d$land_prep_implement[grepl("power tiller|PTOS", d$land_prep_implement)] <- "unknown"
   
   d$season <- paste0(d$season,"_",d$year)
   d$farmer_name <- d$crop_sys <- d$year <- NULL
   
   d$insecticide_used <- ifelse(is.na(d$insecticide_product)| grepl("none", d$insecticide_product), FALSE, TRUE)
   d$herbicide_used <- ifelse(is.na(d$herbicide_product)| grepl("none", d$herbicide_product), FALSE, TRUE)

   # so that you can specify the price per fertilizer type
   d$fertilizer_price <- as.character(d$fertilizer_price)
   
   carobiner::write_files(path, meta, d)
}


