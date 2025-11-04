# R script for "carob"


carob_script <- function(path) {
   
"To identify the main limiting factors to maize and bean production in Central America, field trials were established in six maize- and bean-producing regions in Guatemala, Honduras and El Salvador. Potential yield-limiting factors were evaluated in the 2017 growing season, and included: water stress, nutrient deficiency, pest and disease pressure, and/or interplant competition. The data set contains daily weather data for the six sites for the duration of the growing season (rainfall, minimum and maximum temperature), crop data (grain yield, emergence and flowering dates) and economic data (costs associated with the different treatments, gross and net profit). Detailed descriptions of the experiments are also provided (in Spanish). (2018-12-17)"
   
   uri <- "hdl:11529/10548162"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1, 
      data_organization = "CIMMYT", 
      publication = "doi:10.1017/S0021859619000571",
      project =NA, 
      data_type = "experiment",
      response_vars = "yield",
      treatment_vars = "variety;irrigated;N_fertilizer;P_fertilizer;K_fertilizer;row_spacing;insecticide_used", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-10-19"
   )
   
   f <- ff[basename(ff)=="MilpaYieldGap-DataShared-2017.xlsx"]
   
   ## Processing maize yield data 
   r1 <- carobiner::read.excel(f, sheet="YieldData", na="NA")
   names(r1) <- r1[1,]
   r1 <- r1[-1,]
   d1 <- data.frame(
      location= r1$Site,
      crop= "maize",
      intercrops= ifelse(!is.na(r1$`Bean Yield`), "common bean", "none"), 
      irrigated= ifelse(r1$Irrigation=="Y", TRUE, FALSE),
      pest= r1$`Pest and Disease Control`,
      fert=r1$Fertilization,
      plt_arr= r1$`Planting Arrangement`,
      yield= as.numeric(r1$`Maize Yield`) * 1000,
      plant_height = as.numeric(r1$`Plant Height`) * 100,
      ear_height = as.numeric(r1$`Ear Height`) * 100,
      emergence_date = as.character(as.Date(as.numeric(r1$`Emergence Date`), origin="1899-12-30") ),
      flowering_date =  as.character(as.Date(as.numeric(r1$`Flowering Date`), origin="1899-12-30") ),
      block= r1$Block
   )
   
   ## Processing maize EconomicAnalysis data 
   r2 <- carobiner::read.excel(f, sheet = "EconomicAnalysis", na=c("NA"))
   names(r2) <- r2[1,]
   r2 <- r2[-1,]
   d2 <- data.frame(
      location= r2$Site,
      irrigated= ifelse(r2$Irrigation=="Y", TRUE, FALSE),
      pest= r2$`Pest and Disease Control`,
      fert= r2$Fertilization,
      crop_price= as.numeric(r2$`Local Maize Price`),
      block= r2$Block,
      plt_arr= r2$`Planting Arrangement`
      
   )
   dm <- merge(d1, d2, by=c("location", "irrigated", "fert", "pest", "block", "plt_arr"), all.x = TRUE)
   
   ## Adding more variables 
   var <- data.frame(
      location= c(rep("Lempira",2), rep("El Paraiso", 2), rep("Suchitepequez", 2), rep("La Libertad", 2), rep("Quetzaltenango", 2), rep("Chimaltenango", 2)),
      plt_arr= c(rep(c("O","L"),times=6)),
      plant_spacing= c(NA, 100, NA, 75, 75, 90, NA, 80, 100, 100, 100, 100),
      row_spacing= c(NA, 50, NA, 40, 25, 50, NA, 40, 50, 100,50, 100 ),
      plant_density= c(NA, 40000, NA, 66700, 53300, 67000, NA, 62500, 60000, 60000, 60000, 50000),
      planting_date= c(rep("2017-06-27", 2), rep("2017-06-23", 2), rep("2017-05-19", 2), rep("2017-06-08", 2),rep("2017-04-21", 2), rep("2017-03-21", 2)),
      variety= c(rep("DICTA Sequia", 2), rep("HS 23 cristiani", 2), rep("Dekalb 390", 2),rep("HS 59", 2), rep("ICTA campuesto", 2), rep("Native white", 2)),
      plot_area= c(rep(40,2), rep(72,2), rep(135, 2), rep(67.2, 2), rep(120, 2), rep(121,2))
   )
   dm <- merge(dm, var, by= c("location", "plt_arr"), all.x = TRUE)
   
   dm$planting_date[is.na(dm$planting_date)] <- ifelse(dm$location[is.na(dm$planting_date)]=="El Paraiso", "2017-06-23",
                                                ifelse(dm$location[is.na(dm$planting_date)]=="La Libertad", "2017-06-08",
                                                ifelse(dm$location[is.na(dm$planting_date)]=="Lempira", "2017-06-27", dm$planting_date)))

   ## Adding insecticide
   Pest <- data.frame(
      location= c(rep("Lempira",2), rep("El Paraiso", 2), rep("Suchitepequez", 2), rep("La Libertad", 2), rep("Quetzaltenango", 2), rep("Chimaltenango", 2)),
      pest= c(rep(c("O","L"),times=6)),
      insecticide_used= c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
   )

   dm <- merge(dm, Pest, by= c("location", "pest"), all.x = TRUE)
   
   ## Processing bean yield data
   r11 <- r1[!is.na(r1$`Bean Yield`),]
   d11 <- data.frame(
      location= r11$Site,
      crop= "common bean",
      intercrops= "maize",
      irrigated= ifelse(r11$Irrigation=="Y", TRUE, FALSE),
      pest= r11$`Pest and Disease Control`,
      fert= r11$Fertilization,
      plt_arr= r11$`Planting Arrangement`,
      yield= as.numeric(r11$`Bean Yield`) *1000,
      emergence_date= as.character(as.Date(as.numeric(r11$`Emergence Date`), origin="1899-12-30") ),
      flowering_date=  as.character(as.Date(as.numeric(r11$`Flowering Date`), origin="1899-12-30") ),
      block= r11$Block
   )
   
   ## Processing bean EconomicAnalysis data 
   r22 <- r2[!is.na(r2$`Local Bean Price`),]
   d22 <- data.frame(
      location= r22$Site,
      irrigated= ifelse(r22$Irrigation=="Y", TRUE, FALSE),
      fert= r22$Fertilization,
      crop_price= as.numeric(r22$`Local Bean Price`),
      plt_arr= r22$`Planting Arrangement`,
      pest= r22$`Pest and Disease Control`,
      block= r22$Block
      
   )
   
   db <- merge(d11, d22, by=c("location", "irrigated", "fert", "pest", "block", "plt_arr"), all.x = TRUE)
   
   ## Adding more variables 
   varb <- data.frame(
      location= c(rep("Lempira",2), rep("El Paraiso", 2), rep("Suchitepequez", 2), rep("La Libertad", 2), rep("Quetzaltenango", 2), rep("Chimaltenango", 2)),
      pest= c(rep(c("O","L"),times=6)),
      plant_density= c(NA, NA, NA, NA, 53000, 53000, NA, NA, 40000, 40000, 80000, 80000),
      planting_date= c(rep("2017", 2), rep("2017-10-02", 2), rep("2017" ,2), rep("2017-09-19", 2), rep("2017-04-21", 2), rep("2017-08-29",2)),
      variety= c(NA, NA, rep("DICTA De HORO", 2), NA, NA, rep("CENTA EAC", 2), rep("ICTA Labor Ovalle", 2), rep("Native climbing", 2)),
      plot_area= c(rep(40,2), rep(72,2), rep(135, 2), rep(67.2, 2), rep(120, 2), rep(121,2)),
      insecticide_used= c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
   )
   db <- merge(db, varb, by=c("location", "pest"), all.x = TRUE)
   
   d <- carobiner::bindr(dm, db)
   d$trial_id <- paste0(d$location, "_", d$block)
   d$crop_price <- d$crop_price/d$yield ## UDB/kg
   d$currency <- "USD"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   
   
   ### Adding longitude and latitude
   geo <- data.frame(
      location= c("Lempira", "El Paraiso", "Suchitepequez", "La Libertad", "Quetzaltenango", "Chimaltenango"),
      latitude= c(14.5575, 13.99918, 14.488670, 13.49405, 14.8304, 14.6651),
      longitude= c(-88.5369, -86.8063,  -91.35723, -89.3208, -91.5412, -90.8282006),
      country=c("Honduras", "Honduras", "Guatemala", "El Salvador", "Guatemala", "Guatemala")
   )
   d$geo_from_source <- FALSE
   d <- merge(d, geo, by="location", all.x = TRUE)

   ### Adding fertilizer application
   fertl <- data.frame(
      location= c(rep("Lempira",2), rep("El Paraiso", 2), rep("Suchitepequez", 2), rep("La Libertad", 2), rep("Quetzaltenango", 2), rep("Chimaltenango", 2)),
      fert= c(rep(c("O","L"),times=6)),
      N_fertilizer= c(207,125, 238, 113, 188, 129, 174, 116, 180, 129, 180, 128),
      P_fertilizer= c(74, 30, 65,20, 56, 17, 65, 32, 13, 17, 13, 39),
      K_fertilizer= c(97, 0, 97, 19, 24, 0, 65, 16, 24, 32, 24, 0)
   )
   d <- merge(d, fertl, by=c("location", "fert"), all.x = TRUE)
   
   d$fert <- d$plt_arr <- d$block <- d$pest <- NULL
   
   ## Adding weather data 
   wh <- carobiner::read.excel(f, sheet = "Weather", na=c("."))
   date <- wh[,c(1:3)]
   loc= c("Chimaltenango", "El Paraiso", "La Libertad", "Lempira","Quetzaltenango","Suchitepequez")
   W <- lapply(loc, \(i){
      l <- grepl(i, names(wh))
      r <- wh[,l]
      r <- cbind(r, date)
      names(r) <- r[1,]
      r <- r[-1,]
      ## processing 
      data.frame(
         date= paste(r$Year, r$Month, r$Day, sep="-"),
         location= i,
         prec= as.numeric(r$Rainfall),
         tmin= as.numeric(r$Temp_Min),
         tmax= as.numeric(r$Temp_Max),
         temp= as.numeric(r$Temp_Avg)
      )}
	)
   wd <- do.call(rbind, W)
	wd <- merge(wd, geo, by="location")
    wd$geo_from_source <- FALSE

	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d, wth = wd)
}


