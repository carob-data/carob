# R script for "carob"


carob_script <- function(path) {
   
"This dataset is generated from the research study that was conducted to understand and compare water use and agricultural productivity following the use of wetting front detectors (WFDs) against farmers' standard practice. The study was conducted in Nyangua and Tekuru communities in the Upper East Region of Ghana for 2 years (2017/2018 and 2018/2019 dry seasons). Pepper (Capsicum annuum L.) was used as a test crop since it is a high-value vegetable with various uses in the study areas. Pepper (Capsicum annuum) variety Scotch Bonnet seeds were sown on 29 September and transplanted on three farmers' fields on 11 November 2017 (year 1) and 7 December 2018 (year 2) at the five to six true leaf stage. In this study, three irrigation regimes and six soil amendment treatments were tested. The irrigation regimes included: (i) farmers' practices(FP)—the quantity and timing of daily irrigation water based on local knowledge and practices; (ii) irrigation quantity was based on farmers' decision about using the WFD whilst the timing was based on local knowledge and practices usually at 3 pm; (iii) irrigation requirement using crop water requirement(IRCWR) which was computed using CROPWAT (Allenet al., 1998). Although the timing and quantity of daily irrigation water depend on the crop water requirement the irrigation interval was set at 24 h (3 pm) for the sake of convenience to farming communities. Irrigation water was recorded daily for each treatment. Pepper yields per subplot were recorded from each treatment containing 30 plants per subplot. Fresh fruits were harvested three times during the first growing season (Year 1) and four times during the second (year2). The first harvest was after 4 months (125 days) of planting. Fresh fruit weight was recorded during harvesting."
   
   uri <- "doi:10.7910/DVN/U9MZZV"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1, 
      data_organization ="IWMI", 
      publication= "doi: 10.1002/ird.2454", 
      project= "Africa RISING", 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "irrigation_amount; N_fertilizer; P_fertilizer; K_fertilizer", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-11-04",
      notes= "WFD= Use of the Fullstop wetting front detector;
             CWF=Irrigation requirement based on crop water requirement computation;
             FP= Farmers' practice scheduling"
   )
   
   f1 <- ff[basename(ff)=="yieldData.csv"]
   f2 <- ff[basename(ff)=="irrigationWaterData.csv"]
   f3 <- ff[basename(ff)=="soilData.csv"]
   
   
   ## processing yield  data
   r1 <- read.csv(f1, fileEncoding="latin1", na=c(""))
   d1 <- data.frame(
      treatment= r1$IR,
      fertilizer= as.numeric(substr(r1$FR, 2,2)),
      planting_date= ifelse(r1$Year=="Y1", "2017-11-11", "2018-12-07"),
      yield= as.numeric(substr(r1$Yield.of.pepepr..t.ha....st..error, 0, 4))*1000 ,
      crop= "pepper",
      adm1= "East Ghana",
      country= "Ghana",
      row_spacing= 70,
      plant_spacing= 50
   )
   d1 <- d1[!is.na(d1$yield),]
   ## processing irrigation data
   r2 <- read.csv(f2, na=c(""))
   d2 <- data.frame(
      treatment= r2$IR,
      planting_date= ifelse(r2$Year=="Y1", "2017-11-11", "2018-12-07"),
      irrigation_amount= r2$Irrigation.water.mm
   ) 
   d2 <- d2[!is.na(d2$irrigation_amount),]
   d2 <- aggregate(irrigation_amount ~ treatment +planting_date, data = d2, FUN = sum)
   d <- merge(d1, d2, by=c("treatment", "planting_date"), all.x = TRUE) |> na.omit()
   
   ## processing soil data
   r3 <- read.csv(f3, na=c(""))
   d3 <- data.frame(
      treatment= r3$IR,
      fertilizer= as.numeric(substr(r3$FR, 2,2)),
      soil_sand= r3$sand.,
      soil_silt= r3$Silt.,
      soil_clay= r3$Clay.,
      soil_N= r3$X..Nitrogen *10000,
      soil_P_total= r3$Total.Phpsphorus..mg.kg.,
      soil_K= r3$Total.Potassium..mg.kg.,
      soil_SOC= r3$X..Organic.Carbon ,
      soil_SOM= r3$X..Organic.Matter
   ) 
   
   d <- merge(d, d3, by=c("treatment","fertilizer"), all.x = TRUE)
  
   d$N_fertilizer <- c(165, 165, 86.4, 64.8, 129.6, 172.8)[d$fertilizer]
   d$P_fertilizer <- c(71.5, 71.5, 18.9, 13.5, 27, 36)[d$fertilizer]
   d$K_fertilizer <- c(33, 33, 9, 6.75, 13.5, 18)[d$fertilizer]
   d$fertilizer <- NULL

   d$trial_id <- "1"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- TRUE
   d$yield_part <- "fruit"
   d$latitude <- 6.73838
   d$longitude <-  -0.420577
   d$geo_from_source <- FALSE
   
   
   carobiner::write_files(path, meta, d)
}


