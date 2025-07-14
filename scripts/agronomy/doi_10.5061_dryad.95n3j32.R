# R script for "carob"
# license: GPL (>=3)

## ISSUES
### - Weather data is really ambiguous
##  - maximum and minimum temperature contain extreme value 
##  - we assume the data have been collected many time in the same year (at daily time scale)

carob_script <- function(path) {
   
   "
Data from: High soil test phosphorus effect on corn yield

Phosphorus removal in crop harvest has increased as yields have increased.  Fertilizer P use guidelines are based on calibrations often developed for much  lower yield levels and may need recalibration. Corn yields may be greater  with higher than recommended soil test P when springs are relatively wet  and cool. Research was conducted across 12 irrigated and five rainfed siteyr  in Nebraska with initial Bray-1 P ≤ 11 mg kg–1 to evaluate P application  strategies for yield and P uptake of continuous corn (Zea mays L.) with and without tillage. The fertilizer P treatments were maintained on the same plots and included: (i) no P applied (0P); (ii) P applied according to the University  of Nebraska-Lincoln deficiency correction recommendation (UNL_P); (iii) P applied to replace P removed in the previous harvest (Replace_P); (iv) Bray-1 P increased and maintained at 25 mg kg–1 (Bray_25); and (v) Bray-1 P increased and maintained at 35 mg kg–1 (Bray_35). Interactions of P practice with other factors were not significant indicating consistency of P practice effects across varied climate conditions near planting and plant emergence. Grain yield was 9.3% and 0.89 Mg ha–1 more with the P-applied treatments compared with 0P. Grain yield was 3.3% more due to the additional 5.9 kg ha–1 yr–1 P applied with Replace_P compared with UNL_P. Grain yield did not differ for Replace-P, Bray_25, and Bray_35. Plant P uptake was on average linearly increased by 0.27 kg kg–1 P applied. Fertilizer P application for continuous corn should be by Replace_P.
"
   uri <- "doi:10.5061/dryad.95n3j32"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
       data_organization = "UNL", #University of Nebraska–Lincoln
       publication = "doi:10.2136/sssaj2018.02.0068",
       project = NA,
       carob_date = "2025-07-10",
       design = NA,
       data_type = "experiment",
       treatment_vars = "P_fertilizer",
       response_vars = "yield", 
       carob_contributor = "Cedric Ngakou",
       completion = 100,	
        notes = NA
   )
   
   
   f1 <- ff[basename(ff) == "UNL STP data Oct 14 2018.xls"]
   
   r1 <- suppressWarnings(carobiner::read.excel(f1, sheet="Crop data", fix_names = TRUE))
   r2 <- suppressWarnings(carobiner::read.excel(f1, sheet="STP 5cm"))
   r3 <- suppressWarnings(carobiner::read.excel(f1, sheet="Weather", fix_names = TRUE))
   
   ### process crop data 
   d1 <- data.frame(
      location= ifelse(grepl("Haskell Ag Lab", r1$L), "Haskell Agricultural Laboratory", 
                ifelse(grepl("WCREC", r1$L), "West Central Research and Extension Center", "Eastern Nebraska Research and Extension Center")),
      
      planting_date= r1$Y,
      plot_id= as.character(r1$Plot),
      treatment= r1$P,
      P_fertilizer= ifelse(grepl("Control", r1$P), 0 , 
                  ifelse(grepl("UNL_P", r1$P), 4.48*25,
                  ifelse(grepl("Bray_3Replace_PP", r1$P), 35*5.9,
                  ifelse(grepl("Bray_2Replace_PP", r1$P), 25*5.9, 118)))),
      land_prep_method= tolower(r1$T),
      rep= as.integer(r1$R),
      yield= r1$GrYdMgH*1000,
      yield_moisture= 15.5, 
      grain_P= r1$Gr_P*100,
      harvest_index= r1$HI,
      dmy_total= r1$StDMMg_ha*1000,
      residue_P= r1$SPp*100
   )
   
   ### process soil data 
   d2 <- data.frame(
      location= ifelse(grepl("HAL", r2$L), "Haskell Agricultural Laboratory", 
                       ifelse(grepl("WC", r2$L), "West Central Research and Extension Center", "Eastern Nebraska Research and Extension Center")),
      trial_id= ifelse(grepl("HAL", r2$L), "1", 
                       ifelse(grepl("WC", r2$L), "2", "3")),
      planting_date= r2$y,
      plot_id= as.character(r2$pl),
      rep= as.integer(r2$r),
      land_prep_method= tolower(r2$tillage),
      depth= r2$`depth, inches`,
      #soil_P_available1= r2$Bray1,
      soil_P_available= r2$Olsen,
      soil_P_Mehlich= r2$Mehlich3,
      country= "United States"
   )
   
   ### merge d2 and d1
   d <- merge(d2, d1, by=c("location", "rep", "planting_date", "plot_id", "land_prep_method"), all.x = TRUE) 
   d$land_prep_method <- ifelse(grepl("no-till", d$land_prep_method), "minimum tillage", "disk tillage")
   
   d$crop <- "maize"
   d$yield_part <- "grain"
   d$is_survey <- FALSE
   d$on_farm <- TRUE
   d$geo_from_source <- FALSE
   d$irrigated <- NA
   d$N_fertilizer <- 0
   d$K_fertilizer <- 0
   
   ### fixing planting date
   plt <- data.frame(
      planting_date= c(2011, 2012, 2013, 2014, 2015, 2016),
      month_day= c("05-07", "04-19", "05-13", "04-22", "05-16", "05-05",
                   "05-18", "05-08","05-16", "05-20", "05-19", NA, 
                   "05-17", "04-18", "05-14", "04-23", "04-14", "05-10"),
      location= c(rep("Eastern Nebraska Research and Extension Center",6), 
                  rep("Haskell Agricultural Laboratory", 6),
                  rep("West Central Research and Extension Center", 6))
   )
   
   d <- merge(d, plt, by=c("planting_date", "location"), all.x = TRUE)
   d$planting_date <- paste0(d$planting_date, "-", d$month_day)
   d$month_day <- NULL 
   ### Adding long and lat 
   i <- grepl("Eastern Nebraska", d$location)
   d$longitude[i] <- -96.4973
   d$latitude[i] <- 41.14588
   
   i <- grepl("Haskell Agricultural", d$location)
   d$longitude[i] <- -96.9588
   d$latitude[i] <-  42.38148
   
   i <- grepl("West Central", d$location)
   d$longitude[i] <- -100.768
   d$latitude[i] <- 41.0899
   
   
   ### record weather data 
   
   r3 <- r3[, !apply(is.na(r3), 2, all)]
   
   ## Fixing header and reconstructing raw data 
   i <- grep("d", r3$AGRO)
   W1= r3[1:(i[2]-3),]
   W2= r3[(i[2]-2):(i[3]-3),]
   W3= r3[(i[3]-2):(i[4]-3),]
   W4=r3[(i[4]-2):nrow(r3),]
   
   proc_wth <- function(r){
      ii <- grep("T-High", r)
      cols <- c("T-High", "T-Low", "Rel Hum", "Soil Tmp", "Wind Spd","Solar", "Precip", "ET-NE")
      if (length(ii) > 1) {
         new_names <- ifelse(r[grep("T-High", r[, ii[1]]), ] %in% cols, r[grep("T-High", r[, ii[1]]),],
                      ifelse(r[grep("T-High", r[, ii[2]]),] %in% cols, r[grep("T-High", r[, ii[2]]),], 
                      ifelse(r[grep("T-High", r[, ii[3]]),] %in% cols, r[grep("T-High", r[, ii[3]]),], names(r))))
         names(r) <- new_names
      }
      else {
         ifelse(r[grep("T-High", r[, ii[1]]), ] %in% cols, r[grep("T-High", r[, ii[1]]),], names(r))
         names(r) <- new_names
      }
      names(r) <- make.names(names(r), unique = TRUE)
      
      return(r)
   }
 
   ### Fixing date in weather data 
  
   pr_wth <- function(date, d){
      ### datatype 
      cols <- c("tmax", "tmin", "rhum","wspd", "srad", "prec")
      d[cols] <- lapply(d[cols], function(x) suppressWarnings(as.numeric(x)))
      wth <- d[grepl(date, d$year),]
      if (date=="2012"){ group <- rep(1:366, length.out = nrow(wth))}
      else{ group <- rep(1:365, length.out = nrow(wth))}
      result <- lapply(cols, function(col) {
         tapply(wth[[col]], group, mean)
      })
      tt <- do.call(data.frame, result)
      names(tt) <- cols
      tt$year <- date
      tt$doy <- as.integer(1:nrow(tt))
      return(tt)
   }
   
   ## new raw data 
   w1 <- proc_wth(W1) 
   w2 <- proc_wth(W2)
   w3 <- proc_wth(W3)
   w4 <- proc_wth(W4)
   
   
   ### Processing
   date <- c("2010","2011", "2012","2013","2014")   
   ### location1: Eastern Nebraska Research and Extension Center
   wth1 <- data.frame(
      day= w1$AGRO,
      year= w1$FARM,
      tmax= w1$T.High,
      tmin= w1$T.Low,
      rhum= w1$Rel.Hum,
      wspd= w1$Wind.Spd,
      srad= w1$Solar,
      prec= w1$Precip
   ) 
   wth1 <- wth1[-c(1:2),]
   
   wth11 <- data.frame(
      day= w4$AGRO,
      year= w4$FARM,
      tmax= w4$T.High,
      tmin= w4$T.Low,
      rhum= w4$Rel.Hum,
      wspd= w4$Wind.Spd,
      srad= w4$Solar,
      prec= w4$Precip
   ) 
   wth11 <- wth11[-c(1:3),]
   
   wthf <- rbind(wth1, wth11)
   
   wthf$year <- as.numeric(gsub("ate/t|ORD\\(N|HPLAT|AGROF|sed 0|a2560|a2553", NA, wthf$year))
   wthf$day <- as.numeric(gsub("d|ORT|ONC|ORT|EAD|e U", NA, wthf$day))
   ## drop rows with NA in year 
   wthf <- wthf[!is.na(wthf$year),]
   ### fixing date
   wth_mean1 <- lapply(date, function(y) pr_wth(y, wthf))
   wth_mean1 <- do.call(rbind, wth_mean1)
   wth_mean1$location  <- "Eastern Nebraska Research and Extension Center"
   wth_mean1$longitude <- -96.4973
   wth_mean1$latitude <- 41.14588
   
   ## Location 2: West Central Research and Extension Center
   wth2 <- data.frame(
      day= w1$X.26,
      year= w1$X.27,
      tmax= w1$T.High.1,
      tmin= w1$T.Low.1,
      rhum= w1$Rel.Hum.1,
      wspd= w1$Wind.Spd.1,
      srad= w1$Solar.1,
      prec= w1$Precip.1
   ) 
   wth2 <- wth2[-c(1:5),]
   
   wth21 <- data.frame(
      day= w2$AGRO,
      year= w2$FARM,
      tmax= w2$T.High.1,
      tmin= w2$T.Low.1,
      rhum= w2$Rel.Hum.1,
      wspd= w2$Wind.Spd.1,
      srad= w2$Solar.1,
      prec= w2$Precip.1
   ) 
   
   wth21 <- wth21[-c(1:6),]
   
   wth22 <- data.frame(
      day= w3$AGRO,
      year= w3$FARM,
      tmax= w3$T.High,
      tmin= w3$T.Low,
      rhum= w3$Rel.Hum,
      wspd= w3$Wind.Spd,
      srad= w3$Solar,
      prec= w3$Precip
   ) 
   
   wth22 <- wth22[-c(1:3),]
   wthf1 <- carobiner::bindr(wth2, wth21, wth22)
   
   wthf1$year <- as.numeric(gsub("ate/t|ORD\\(N|HPLAT|AGROF|sed 0|a2560", NA, wthf1$year))
   wthf1$day <- as.numeric(gsub("d|ORT|ONC|ORT|EAD|e U", NA, wthf1$day))
   ## drop rows with NA in year 
   wthf1 <- wthf1[!is.na(wthf1$year),]
   
   ## fixing date
   date1 <- c("2010","2011", "2012","2013")
   wth_mean2 <- lapply(date1, function(y) pr_wth(y, wthf1))
   wth_mean2 <- do.call(rbind, wth_mean2)
   wth_mean2$location  <- "West Central Research and Extension Center"
   wth_mean2$longitude <- -100.768
   wth_mean2$latitude <- 41.0899
   
   
   ### Location 3:  Haskell Agricultural Laboratory
   wth3 <- data.frame(
      day= w1$CONCORD,
      year= w1$X.44,
      tmax= w1$X.46,
      tmin= w1$T.High.2,
      rhum= w1$T.Low.2,
      wspd= w1$Rel.Hum.2,
      srad= w1$Wind.Spd.2,
      prec= w1$Solar.2
   ) 

   wth3 <- wth3[-c(1:2),]   
   wth31 <- data.frame(
      day= w2$AGRO,
      year= w2$FARM,
      tmax= w2$T.High,
      tmin= w2$T.Low,
      rhum= w2$Rel.Hum,
      wspd= w2$Wind.Spd,
      srad= w2$Solar,
      prec= w2$Precip
   ) 
   
   wth31 <- wth31[-c(1:3),] 
   
   wth32 <- data.frame(
      day= w2$AGRO,
      year= w2$FARM,
      tmax= w2$T.High.2,
      tmin= w2$T.Low.2,
      rhum= w2$Rel.Hum.2,
      wspd= w2$Wind.Spd.2,
      srad= w2$Solar.2,
      prec= w2$Precip.2
   ) 
   wth32 <- wth3[-c(1:2),] 
   
   wthf2 <- carobiner::bindr(wth3, wth31, wth32)
   
   wthf2$year <- as.numeric(gsub("ate/t|ORD\\(N|HPLAT|AGROF|sed 0|a2560", NA, wthf2$year))
   wthf2$day <- as.numeric(gsub("d|ORT|ONC|ORT|EAD|e U", NA, wthf2$day))
   ## drop rows with NA in year 
   wthf2 <- wthf2[!is.na(wthf2$year),]
   
   ## fixing date
   wth_mean3 <- lapply(date1, function(y) pr_wth(y, wth3))
   wth_mean3 <- do.call(rbind, wth_mean3)
   wth_mean3$location  <- "Haskell Agricultural Laboratory"
   wth_mean3$longitude <- -96.9588
   wth_mean3$latitude <- 42.38148
   
   wth <- carobiner::bindr(wth_mean1, wth_mean2,	wth_mean3)
   wth$date <- as.character(as.Date(wth$doy - 1, origin = paste0(wth$year, "-01-01")))
   wth$geo_from_source <-  FALSE
   wth$country <-  "United States"
   wth$doy <- wth$year <-  NULL
   
   
   carobiner::write_files(path, meta, d, wth = wth)
}

