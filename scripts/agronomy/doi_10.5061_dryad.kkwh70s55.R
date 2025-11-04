# R script for "carob"

carob_script <- function(path) {
   
"An experiment was conducted to evaluate the effects of row spacing (RS) and interplant spacing (IPS) on the yield of total biomass, leaf, and edible twigs, and nutritive value of pigeon pea,(Cajanus cajan L. Millsp.) at three locations in the Rift Valley area of Ethiopia, using a randomized completed block design with three replications in 3 × 3 factorial arrangement; three RS ( 25, 50, and 75 cm) and three IPS (15, 30, and 45 cm). Row spacing × IPS of 25 × 30 cm and 50 × 15 cm, gave greater total biomass yield than the other RS and IPS combinations.  A similar result was found for edible plant yield. At Hawassa the greater leaf CP was found for the wider IPS of 45 cm than the narrower IPS of 30 cm (318 Vs. 303 g kg-1 ). At Wondo-Genet, the greater leaf in vitro digestible organic matter  (597 Vs. 582 g kg-1 ) was found for the wider RS of 75 cm than narrower RS. Similar to leaf, a better nutritional value of edible twigs was found for a wider RS and IPS than narrower RS and IPS at Hawassa and Wondo-Genet. In contrast at Aliyu-Amab, a better nutritional value of edible twigs was found for narrower RS and IPS than the wider spacing. Thus, it can be concluded that RS × IPS of 25 × 30 cm or 50 × 15 cm are advisable for pigeon pea forage production."
  
   uri <- "doi:10.5061/dryad.kkwh70s55"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA, 
		data_organization = "ARARI;HWU",
		publication="doi:10.1002/agj2.20803", 
		project=NA, 
		data_type= "experiment", 
		response_vars= "fwy_total; dmy_leaves; fwy_leaves", 
		treatment_vars = "plant_spacing; row_spacing", 
		carob_contributor= "Cedric Ngakou", 
		carob_date="2025-06-10",
		notes=NA
   )
   
   
   f <- ff[ basename(ff)=="agronomy_data_Aranged_for_analysis.xls"]
   
   r1 <- carobiner::read.excel(f, sheet = "total biomass yield and others", na=c(".")) 
   
   d1 <- data.frame(
      location= r1$Location,
      harvest_date= r1$Harvest,
      rep= as.integer(r1$Block),
      row_spacing= r1$`Row spacing cm`,
      plant_spacing= r1$`Interplant spacing cm`,
      fwy_total= r1$`Total biomass yield t/ha`*1000,
      plant_height= r1$`height cm`,
      plot_area= 3*4.8/10000,## ha 
      weeding_done= TRUE,
      weeding_times= 3L,
      weeding_method= "manual",
      crop= "pigeon pea",
      variety= "Kibret",
      country= "Ethiopia",
      trial_id= "1", 
      planting_date="2019-07-07", 
      on_farm= TRUE, 
      is_survey= FALSE, 
      yield_part="leaves",
      irrigated= FALSE
   ) 
   
   
   r2 <- carobiner::read.excel(f, sheet = "chemical analysis leaf", na=c(".")) 
   
   d2 <- data.frame(
      location= r2$Locatio,
      rep= as.integer(r2$block),
      row_spacing= r2$`Row spacing`,
      plant_spacing= r2$`interplant spacing`,
      dmy_leaves= r2$`DM%`,
      leaf_N= r2$Nitrogen*10 # in mg/g
      #leaf_protein= r2$`crud protein`
      #leaf_OM= r2$IV
   )
   
   r3 <- carobiner::read.excel(f, sheet = "Leaf and edible twig yield", na=c(".")) 
    
   d3 <- data.frame(
      location= r3$Location,
      harvest_date= r3$harvest,
      rep= as.integer(r3$Block),
      row_spacing= r3$`row spacing cm`,
      plant_spacing= r3$`interplant spacing cm`,
      fwy_leaves= r3$`leaf yield t/ha`*1000
   )
   
   
   ### merge 
   
   d <- merge(d1, d2, by= c("rep","row_spacing","plant_spacing","location"), all.x = TRUE)
   
   dd <- merge(d, d3, by= c("rep","row_spacing","plant_spacing","location", "harvest_date"), all.x = TRUE )
   dd$harvest_date <- NULL
   
   dd$longitude <- c(40.3, 38.85, 38.63)[dd$location]
   dd$latitude <-  c(9.93, 7.1, 7.31 )[dd$location]
   dd$elevation <-  c(1844, 1714, 1783)[dd$location]
   dd$geo_from_source <- TRUE
   dd$location <- c("Aliyu-Amba", "Hawassa", "Wondo-Genet")[dd$location]
   
   
   ## Adding soil info
   SI <- data.frame(
      location= c("Aliyu-Amba", "Hawassa", "Wondo-Genet"),
      soil_type= c("loam", "sandy clay loam", "clay"),
      soil_pH= c(7.4, 6.8, 5.6),
      soil_N= c(0.77, 0.61, 0.68),
      soil_P= c(12.75, 35.56, 39.8)/1000,## mg/g
      soil_K_exch= c(0.53, 3.03, 0.91),
      soil_SOC= c(8.9, 7.1, 8),
      soil_SOM= c(15.5, 12.3, 18.8),
      soil_EC= c(0.07, 0.11, 0.06)
   )
   
   dd <- merge(dd, SI, by="location", all.x = TRUE)
   ### Adding fertilizer info from paper
   dd$N_fertilizer <- 19
   dd$P_fertilizer <- 38/2.29
   dd$K_fertilizer <- 0
   dd$S_fertilizer <- 7
   
   ## Process weather data 
  
    r4 <- carobiner::read.excel(f, fix_names = TRUE,  sheet = "envt data for year 2019 2020 ", na=c("."))[,-1] 
    r4 <- data.frame(t(r4))
    r4[1, ] <- ifelse(is.na(r4[1,]), "Date", r4[1,])
    names(r4) <- r4[1,]
    wt <- r4[-1,]
    wt$cont <- 1:nrow(wt)
    wt$year <- ifelse(wt$cont>= 1 & wt$cont<= 6, "2019","2020")
    wt$cont <- NULL
    
    wth <- data.frame(
       date= wt$Date,
       tmax_2= as.numeric(wt$`Max Hawassa`),
       tmin_2= as.numeric(wt$`Min Hawassa`),
       temp_2= as.numeric(wt$`Hawassa average temprature`),
       tmax_1= as.numeric(wt$`Max Aliyu-Amba`),
       tmin_1= as.numeric(wt$`Min Aliyu-Amba`),
       temp_1= as.numeric(wt$`Aliyu-Amba  average temperature`),
       tmax_3= as.numeric(wt$`Wondo-Genet Maximum`),
       tmin_3= as.numeric(wt$`Wondo-Genet Minimum`),
       temp_3= as.numeric(wt$`Wondo-Genet average temperature`),
       prec_1= as.numeric(wt$`Aliyu-Amba`),
       prec_2= as.numeric(wt$Hawassa),
       prec_3= as.numeric(wt$`Wondo-Genet`),
       year= wt$year
    )
    
    wth <- reshape(wth, 
            varying = list(c("tmax_1","tmax_2", "tmax_3"),
                      c("tmin_1", "tmin_2", "tmin_3"),
                      c("temp_1", "temp_2", "temp_3"),
                     c("prec_1", "prec_2", "prec_3")),
            
            v.name= c("tmax","tmin","temp", "prec"),
            timevar= "location",
            times= c(1,2,3),
            direction= "long")
    
    wth$date <- paste0(wth$year, "-",  sprintf("%02d", match(tolower(wth$date), tolower(month.abb)))) 
    wth$longitude <- c(40.3, 38.85, 38.63)[wth$location]
    wth$latitude <-  c(9.93, 7.1, 7.31 )[wth$location]
    wth$location <- c("Aliyu-Amba", "Hawassa", "Wondo-Genet")[wth$location]
    wth$country <- "Ethiopia"
    wth$geo_from_source <- TRUE
    wth$year <-  wth$id <- NULL
    
    
	d$yield_moisture <- as.numeric(NA) #needs to be checked

    carobiner::write_files(path, meta, dd, wth = wth)
}


