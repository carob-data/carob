carob_script <- function(path) {
   
"The current farming system is highly reliant on synthetic fertilizers, which adversely affect soil quality, the environment, and crop production. Improving crop productivity on a sustainable basis is a challenging issue in the current agricultural system. To address this issue, we assumed that the combined use of manure and chemical fertilizers (CF) could improve rice grain yield and soil properties without the expense of the environment. Therefore, a two-year field experiment was conducted to explore optimal fertilizer management strategies using a combination of CF and organic fertilizer in the form of cattle manure (CM) or poultry manure (PM). Manure was added at two levels and soil microbial biomass production, enzyme activities, nutrient content, as well as grain yield of rice were measured. The study consisted of six treatments: no N fertilizer control (Neg-Con); 100% chemical fertilizer (Pos-Con); 60% CM + 40% CF (High-CM); 30% CM + 70% CF (Low-CM); 60% PM + 40% CF (High-PM), and 30% PM + 70% CF (Low-PM). Results showed that the addition of manure significantly increased soil enzymatic activities such as soil invertase, acid phosphatase, urease, catalase, ꞵ-glucosidase, and cellulase as compared to sole chemical fertilizer application. Similarly, the combined fertilizers application led to significant increases in soil microbial biomass carbon (MBC), microbial biomass nitrogen (MBN), soil pH, soil organic carbon (SOC), total nitrogen (TN), available nitrogen (AN), available phosphorous (AP) and rice yield. Average increases in soil MBC, MBN, SOC AN, and AP in the 0–20 cm soil depth were 62.2%, 54.5%, 29.2%, 17.4%, and 19.8%, respectively, across the years in the High-CM treatment compared with the Pos-Con. In addition, the linear regression analysis showed that soil enzymatic activities were highly positively correlated with soil MBC and MBN. The PCA and linear regression analyses showed that the increased soil enzyme activities and microbial biomass production played a key role in the higher grain yield of rice. Overall, the results of this study demonstrate that the combined use of synthetic fertilizer and organic fertilizer in paddy fields could be beneficial for the farmers in southern China by improving soil functionality and yield of rice on a sustainable basis."
  
   uri <- "doi:10.5061/dryad.bk3j9kddh"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
      data_organization = "GXU", 
      publication="doi:10.21203/rs.3.rs-461485/v1", 
      project=NA, 
      data_type= "on-station experiment", 
      treatment_vars= "fertilizer_type; N_fertilizer", 
      response_vars = "yield", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-06-08",
      completion=75, # files f3
      notes=NA
   )
   
   f <- ff[basename(ff) == "Articel_Supporting_Data.xlsx"]
   
   
   ##### Process soil data
   
   r0 <- carobiner::read.excel(f, fix_names = TRUE,  sheet = "Soil biochemical traits")
   
   ### Subset SOC data from r0
   soc <- r0[grep("SOC", r0$Soil.pH): grep("TN", r0$Soil.pH)-1,][-1,]
   names(soc) <- soc[1,]
   ### Subset Total Nitrogen data from r0
   TN <- r0[grep("TN", r0$Soil.pH): grep("AN", r0$Soil.pH)-1,][-1,]
   names(TN) <- TN[1,]
   
   ### Subset available Nitrogen data from r0
   AN <- r0[grep("AN", r0$Soil.pH): nrow(r0),]
   names(AN) <- AN[1,]
   
   # combine them to have a large data set 
   r1 <- cbind(r0[1:grep("SOC", r0$Soil.pH)-1,], soc[-1,], TN[-1,], AN[-1,] )
   names(r1) <- gsub("20", "X_20", names(r1))
   
   r1 <- subset.data.frame(r1)
   
   d1 <- data.frame(
      treatment= r1$Soil.pH,
      soil_pH_2019= r1$XX_2019.2,
      soil_pH_2020= r1$XX_20X_20.3,
      soil_P_available_2019= r1$XX_2019.5,
      soil_P_available_2020= r1$XX_20X_20.6,
      soil_SOC_2019= r1$X_2019,
      soil_SOC_2020= r1$X_20X_20,
      soil_K_2019= r1$X_2019.1,
      soil_K_2020= r1$X_20X_20.1,
      soil_N_2019= r1$X_2019.2,
      soil_N_2020= r1$X_20X_20.2,
      depth_top= 0,
      depth_bottom= 20
   ) 
   d1$trt <- 1:nrow(d1)
   d1$rep <- ifelse(d1$trt <= 6,1L,
              ifelse(d1$trt > 6 & d1$trt <= 12, 2L, 3L))
  
    d1 <- reshape(d1, varying = list(c("soil_pH_2019", "soil_pH_2020"), c("soil_P_available_2019", "soil_P_available_2020"),
                                    c("soil_SOC_2019","soil_SOC_2020"), c("soil_K_2019", "soil_K_2020"), c("soil_N_2019", "soil_N_2020")),
                 v.names = c("soil_pH", "soil_P", "soil_SOC", "soil_K", "soil_N"),
                 timevar ="planting_date",
                 times =  c(2019,2020) ,
                 direction = "long")
   
    d1$id <- d1$trt <- NULL
   
   #### process yield data
   
   r2 <- carobiner::read.excel(f, fix_names = TRUE,  sheet = "Grain Yield")
   
   d2 <- data.frame(
      treatment= r2$Yield.kg.ha,
      yield_2019= r2$X2019,
      yield_2020= r2$X2020,
      crop="rice",
      country="China",
      location="Guangxi University",
      latitude=22.82, ## From paper
      longitude=108.31 ,
      geo_from_source= TRUE,
      plot_area= 3.9*6/10000, ## ha
      yield_part= "grain",
      on_farm=FALSE,
      is_survey= FALSE,
      irrigated= TRUE,
      insecticide_used= TRUE,
      N_splits= 3L
   )
   
   d2$trt <- 1:nrow(d2)
   d2$rep <- ifelse(d2$trt <= 6,1L,
                    ifelse(d2$trt > 6 & d2$trt <= 12, 2L, 3L))
   d2 <- reshape(d2, varying = list(c("yield_2019", "yield_2020")),
                 v.names = c("yield"),
                 timevar ="planting_date",
                 times =  c(2019,2020) ,
                 direction = "long")
   d2$id <- d2$trt <- NULL
   
   d <- merge(d1, d2, by=c("treatment","planting_date", "rep"), all.x = TRUE)
   d$planting_date <- ifelse(grepl("2019", d$planting_date), paste0(d$planting_date, "-03"), paste0(d$planting_date, "-07"))
   
   d$trial_id <- ifelse(grepl("2019", d$planting_date), "1", "2")
   
   d$OM_type <- ifelse(grepl("CM", d$treatment), "cattle dung",
                ifelse(grepl("PM", d$treatment),"poultry manure" , "none"))
   
   d$OM_amount <- (ifelse(grepl("High-CM", d$treatment), 21.5,
                  ifelse(grepl("High-PM", d$treatment), 15.5,
                  ifelse(grepl("Low-CM", d$treatment), 10.7,
                  ifelse(grepl("Low-PM", d$treatment), 7.7, 0)))))/d$plot_area
   
   d$fertilizer_type <- ifelse(grepl("Neg-Con", d$treatment), "none","urea;SSP;KCl")
   d$N_fertilizer <- (ifelse(grepl("High-CM", d$treatment), 301*0.46+351,
                     ifelse(grepl("High-PM", d$treatment), 301*0.46 +351,
                     ifelse(grepl("Low-CM", d$treatment),527*0.46 +351,
                     ifelse(grepl("Low-PM", d$treatment), 527*0.46 +351,
                     ifelse(grepl("Pos-Con", d$treatment), 753*0.46+351, 0))))))/((d$plot_area)*1000) ### Kg/ha 
  
   d$P_fertilizer <-  75
   d$K_fertilizer <- 150
   
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(meta, d, path=path)
}
