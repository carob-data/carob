# R script for "carob"


carob_script <- function(path) {
   
" In the Sahel, crop production is dominated by pearl millet [Pennisetum glaucum (L.) R. Br.] cropping systems including intercropping with cowpea [Vigna unguiculata (L.) Walp]. The research objectives were to determine pearl millet sole crop (PMSC) and intercrop nutrient response functions, profit opportunities from fertilizer use, and a means of relating intercrop to PMSC response. Pearl millet–cowpea trials were conducted in Niger and Mali. The treatment structure was an incomplete factorial with five, four, and four levels of P, N, and K, respectively. Response functions were determined for intercropping with cowpea yield converted to a pearl millet grain yield equivalent for ratios of cowpea to pearl millet grain value (CpPm) ranging from 1 to 5 kg kg–1. Response functions were also determined in consideration of fodder value. A method of adjusting PMSC response coefficients with CpPm as the independent variable was developed for determination of intercrop response functions. Yields and responses to nutrients were low but adequate for profitable N and P use. Value of intercrop production ranged from 50 to 125% more compared with PMSC for CpPm of 2 and 4, and the respective response to 10 kg ha–1 P was 28 and 135% more with intercrop. Consideration of fodder added 23% to the yield value. Application of N and P for intercrop compared with PMSC production has much more profit potential. The ability to apply PMSC functions in determination of pearl millet–cowpea responses to applied nutrients offers a means to improve fertilizer use for the intercrop. "
   
   uri <- "doi:10.5061/dryad.4t0v127"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)

 
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA, 
      data_organization ="INRAN;IER;UNL", 
      publication = "doi:10.2134/agronj2017.03.0139", 
      project = NA, 
      data_type = "compilation", #( On-farm experiment, On-station experiment and , On Oxic Haplustalf and Aquic Haplustalf experiment )
      response_vars = "yield;fwy_total",
      treatment_vars = "intercrops;N_fertilizer;P_fertilizer;K_fertilizer", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-08-31"
   )
   
   d  <- lapply(c("NigerOST","NigerOFT","Mali","BurkinaFasoDori"),\(i){
      
      r <- carobiner::read.excel(ff[basename(ff)=="W Africa Pearl Millet data set.xlsx"], sheet = i)
      
      names(r) <- gsub("Year","Y", names(r))
      names(r) <- gsub("PMGrYld","PMGrYd", names(r))
      names(r) <- gsub("Soil","Site", names(r))
      if( is.null(r$CPGrYd)) r$CPGrYd <- NA
      if(is.null(r$PMFoYd)) r$PMFoYd <- NA 
      if( is.null(r$CPFoYd)) r$CPFoYd <- NA
      if( is.null(r$Site)) r$Site <- NA
      if( is.null(r$`Manure rate, Mg ha-1`)) r$`Manure rate, Mg ha-1` <- 2.5 
      
      ### pearl milllet
      d1 <- data.frame(
         location= r$Site,
         planting_date= as.character(r$Y),
         rep= as.integer(r$Rep),
         treatment= paste("N", r$Nrate, "P", r$Prate, "K", r$Krate, sep=""), 
         N_fertilizer= r$Nrate,
         P_fertilizer= r$Prate,
         K_fertilizer= r$Krate,
         OM_amount= ifelse(grepl("Mali", i), 0, (r$`Manure rate, Mg ha-1`)*1000), # kg/ha
         OM_type= "farmyard manure", ## 
         yield= r$PMGrYd*1000,
         fwy_total= r$PMFoYd*1000,
         crop= "pearl millet",
         intercrops= ifelse( grepl("Burkina|OFT",i),"none","cowpea") ,
         trial_id= i,
         on_farm = ifelse(grepl("OFT|Burkina",i), TRUE, FALSE)
      )
      ## Cowpea crop 
      d2 <- data.frame(
         location= r$Site,
         planting_date= as.character(r$Y),
         rep= as.integer(r$Rep),
         treatment= paste("N", r$Nrate, "P", r$Prate, "K", r$Krate, sep=""), 
         N_fertilizer= r$Nrate,
         P_fertilizer= r$Prate,
         K_fertilizer= r$Krate,
         OM_amount= ifelse(grepl("Mali", i), 0, r$`Manure rate, Mg ha-1`),
         OM_type= "farmyard manure",
         yield= r$CPGrYd*1000, ## kg/ha
         fwy_total= r$CPFoYd*1000,
         crop= "cowpea",
         intercrops= ifelse( grepl("Burkina|OFT",i), NA ,"pearl millet") ,
         trial_id= i,
         on_farm = ifelse(grepl("OFT|Burkina",i), TRUE, FALSE)
      )
      
   carobiner::bindr(d1, d2)
      
   })
   
   d <- do.call(rbind, d)
   
   ## removing rows with NA in intercrop ( pearl millet crop was not intercropped)
   d <- d[!is.na(d$intercrops),]
   
## Fixing country name 
   d$country <- ifelse(grepl("Mali", d$trial_id), "Mali",
                 ifelse(grepl("Burkina", d$trial_id), "Burkina Faso","Niger"))
   
   d$trial_id <- paste0(d$trial_id, "_", d$location)
   d$location[d$country=="Burkina Faso"] <- "Dori"
   d$location[d$country=="Mali"] <- "Segou-Cinzana"
   
### Adding longitude, latitude and soil information from publication 
   
   geo <- data.frame(
      location= c("Bengou", "Konni", "Magaria", "Maradi", "Kagara", "Mir", "Sark", "Dori", "Segou-Cinzana"), # Sark is unknown location 
      latitude= c(11.9905, 13.7916, 13.00094, 13.5012, 13.3748, 14.0809, NA ,14.0327651, 13.2500014),
      longitude= c(3.5932, 5.24779, 8.90867, 7.1025, 7.00994, 12.00276, NA , -0.0348286, -5.9670277),
      soil_pH=c(5.69, 5.81, 5.75, 5.84, rep(NA,4), 5.26),
      soil_SOC= c(2.97, 1.54, 1.08, 1.22, rep(NA,4), 5.04),
      soil_N= c(0.175, 0.065, 0.45, 0.05, rep(NA,4), 0.38),
      soil_P_Mehlich = c(57.8, 22.45, 11.3, 13.1, rep(NA,4), 4.6655),
      soil_K= c(69.45, 56.10, 37.05, 42.50,  rep(NA,4), 70.35),
      soil_Ca= c(2.515, 1.380, 1.000, 1.160,  rep(NA,4), 2.655),
      soil_Mg= c(0.750, 0.475, 0.350, 0.405,  rep(NA,4),1.085 ),
      soil_sand= c( 609.0,836.0, 876.0, 872.0, rep(NA,4), 395.5)*0.1,
      soil_silt= c(58.55, 64.50, 51.00, 66.50,  rep(NA,4), 136.00)*0.1,
      soil_clay= c(138.7, 99.0, 70.0,  61.0,  rep(NA,4), 467.5)*0.1)
      
   
   
   d <- merge(d, geo, by= c("location"), all.x = TRUE)
   
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$geo_from_source <- FALSE
   d$yield_part <- "grain"
  
   ### removing duplicate rows ( occur probably during the data collection)  
   d <- unique(d)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)
   
}

