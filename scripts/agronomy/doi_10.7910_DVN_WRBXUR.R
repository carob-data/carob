# R script for "carob"

carob_script <- function(path) {
  
"Low soil fertility especially nitrogen (N) is one of the major constraints to increased maize productivity on smallholder farms in Malawi. Integration of grain legumes in maize based systems is one of the strategies to improve soil fertility and yields as legumes fix atmospheric nitrogen through a symbiotic relationship with Rhizobium bacteria. A study was conducted in the 2013/2014 growing season in Linthipe and Golomoti Extension Planning Areas (EPAs) in Dedza District, and Nsipe EPA in Ntcheu District. The objectives of the study were to evaluate the grain yields and biological nitrogen fixation (BNF) of the sole and intercropped pigeonpea and soybean under two levels of inorganic P fertilizer (0 and 14 kg P ha-1) and to determine the Phosphorus use efficiency (PUE). BNF was assessed using the N difference method. The experiment was laid out in a randomized complete block design (RCBD). Soils were sandy clay loams, loamy sand and sandy loams to sandy clay loams for Linthipe, Golomoti and Nsipe respectively. Soil pH was moderately acidic to acidic, pH 4.9 to 5.8 in the three sites. Available soil P (Mehlich-3) averaged 44, 84 and 39mg kg-1; and the mean soil organic matter (OM) were 2.55, 1.63 and 2.39% for Linthipe, Golomoti and Nsipe EPAs, respectively. " 
  
  uri <- "doi:10.7910/DVN/WRBXUR"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0, 
    data_organization = "ICRAF;CIAT;LUANAR",
    #Mzumara, E. 2016. Phosphorus use effiiency and productivity of pigeonpea (Cajanus cajan (l.) millsp.) and soybean (glycine max (l.) merrill) on smallholder farms in different agro-ecological zones of central Malawi. MSc thesis in Agronomy. Lilongwe, Malawi. Lilongwe University of Agriculture and Natural Resources.
    publication="hdl:10568/125473", 
    project="AfricaRising", 
    data_type= "experiment", 
    response_vars= "yield;BNF", 
    treatment_vars = "intercrops;P_fertilizer", 
    carob_contributor= "Cedric Ngakou", 
    carob_date="2024-08-17"
  )
  
  ff <- ff[grep("Golomoti|Lint|Nsipe", basename(ff))]
  
  process <- function(f){ 
    
    r <- read.csv(f)
    
    ## pigeon pea
    r1 <- r[grep("pigeonpea", r$Cropping.system), ]
    d1 <- data.frame(
      location= r1$Site,
      # ignore rep because we do not have yield data for individual reps      
      # rep= r1$Rep,
      P_fertilizer= ifelse(grepl("P0", r1$P.level), 0, 14),
      crop= "pigeon pea",
      variety= "Mwaiwathualimi",
      insecticide_used= TRUE,
      planting_date= ifelse(r1$Site=="Golomoti", "2013-12-07",
                     ifelse(r1$Site=="Linthipe", "2013-12-16","2013-12-20")),
      insecticide_product="dimethoate",
      inoculated = FALSE,
      weeding_times= as.integer(3),
      plant_spacing= 60,
      row_spacing= 90,
      plant_density= 44.444,
      intercrops= ifelse(grepl("intercrop", r1$Cropping.system), "soybean", "none"),
      BNF= r1$BNF.pigeonpea..kgha.1. # Biological Nitrogen Fixation 
    )
    
    ## soybean  crop
    r2 <- r[grep("soybean",r$Cropping.system),]
    d2 <- data.frame(
      # ignore rep because we do not have yield data for individual reps      
      #rep= r2$Rep,
      location= r2$Site,
      insecticide_used =FALSE,
      P_fertilizer= ifelse(grepl("P0", r2$P.level), 0, 14),
      crop= "soybean",
      variety="Nasoko",
      planting_date= ifelse(r2$Site=="Golomoti", "2013-12-07",
                     ifelse(r2$Site=="Linthipe", "2013-12-16","2013-12-20")),
      weeding_times= as.integer(3),
      plant_spacing= 5,
      row_spacing= 30,
      plant_density= 533.333,
      inoculant= "rhizobium",
      inoculated= TRUE,
      intercrops= ifelse(grepl("intercrop",r2$Cropping.system), "pigeon pea", "none"),
      BNF= r2$BNF.soybean..kgha.1.
    )
    
     carobiner::bindr(d1,d2) |> unique()
  }
  
  d <- lapply(ff, process)
  d <-  do.call(rbind, d)
  
  d$country <- "Malawi"
  d$trial_id <- as.character(as.integer(as.factor(d$location)))
  d$irrigated <- NA
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$yield_part <- "grain"
  d$geo_from_source <- FALSE
  
  ## Adding grain yield base on information from publication
  ## The yield is a mean of each treatment and has not been reported for each replication.
  yd <- data.frame(
    location= rep(c(rep("Linthipe",4),rep("Golomoti",4), rep("Nsipe",4)),2),
    P_fertilizer= rep(c(0, 0, 14, 14),6),
    intercrops= c(rep(c("none","pigeon pea", "none", "pigeon pea"),3), rep(c("none","soybean", "none", "soybean"),3)),
    crop= c(rep("soybean",12), rep("pigeon pea",12)),
    yield= c(1522, 1419, 1941, 1721, 820, 641, 876, 779, 1150, 1073, 1372, 1308, NA, NA, NA, NA, rep(c(783,383,887,503),2))
  )
  
  d <- merge(d, yd, by=c("location", "P_fertilizer", "intercrops", "crop"), all.x = TRUE)
  
  ## adding longitude , latitude and soil information from publication
  geo <- data.frame(
    location= c("Golomoti", "Linthipe", "Nsipe"),
    latitude= c(-14.4156447, -14.1748697, -14.8710636),
    longitude= c(34.6027832, 34.1243749, 34.7484271),
    soil_pH= c(5.4, 5.1, 6.25),
    soil_N= c(0.076, 0.102, 0.094),
    soil_P = c(82.95, 44.1, 35.1),
    soil_SOM= c(1.5, 2.50, 2.25),
    soil_Zn= c(0.30 , 0.25 , 0.29),
    soil_clay= c(12.4, 24.1, 21.85),
    soil_silt= c(4.6, 7.9, 4.9),
    soil_sand= c( 82.9, 67.9, 73.25),
    soil_sample_top= 0,
    soil_sample_bottom= 40
  )
  d <- merge(d, geo, by="location", all.x = TRUE)
  
  d$N_fertilizer <- d$K_fertilizer <- 0
  
  ## Remove duplicate rows ( This is due to NA in the BNF variable )
  d <- unique(d)
  
	d$yield_moisture <- as.numeric(NA) #needs to be checked

  carobiner::write_files(path, meta, d)
}
