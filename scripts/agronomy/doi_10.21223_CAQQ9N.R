# R script for "carob"


carob_script <- function(path) {
   
"As part of the GIZ-PZTM project, agronomic trials were conducted under a strip-plot design in the locations of Fultola, Moshamari and Hogalbunia of Khulna district in Bangladesh. Two varieties of potato (BARI Alu 72 and BARI Alu 78) under zero tillage with  (ZTSM) and conventional treatments were tested. This dataset corresponds to the results of yield (marketable tuber yield and fresh tuber yield) and soil analysis (characterization, fertility, salinity and carbon organic) of the samples collected at the end season 2021-2022."
   
   uri <- "doi:10.21223/CAQQ9N"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2, 
      data_organization ="CIP", 
      publication = NA, 
      project = "GIZ-PZTM", 
      data_type = "experiment",
      response_vars = "yield;yield_marketable",
      treatment_vars = "land_prep_method;variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-09-06"
   )
   
   f1 <- ff[basename(ff)=="02_Yield data.xlsx"]
   f2 <- ff[basename(ff)=="01_Soil analysis.xlsx"]
   
  # Read files
   r1 <- carobiner::read.excel(f1) 
   r2 <- carobiner::read.excel(f2)
   
# Processing yield data 
   d1 <- data.frame(
     location= r1$location,
     rep= as.integer(r1$replication),
     variety= r1$variety,
     land_prep_method= ifelse(grepl("CM", r1$management), "conventional", "none"),
     yield_marketable= r1$MTY*1000, # kg/ha
     yield= r1$FTY*1000, # kg/ha
     trial_id= paste(r1$location, r1$variety, r1$plot_number, sep="-")
   )
  
   # Processing soil data 
   d2 <- data.frame(
      location= r2$location,
      rep= as.integer(r2$replication),
      variety= r2$variety,
      land_prep_method= ifelse(grepl("CM", r2$management), "conventional", "none"),
      soil_depth= r2$soil_depth,
      soil_sand= r2$sand,
      soil_silt= r2$silt,
      soil_clay= r2$clay,
      soil_type= r2$TC,
      soil_CEC= r2$CEC, 
      soil_pH= r2$pH,
      soil_EC= r2$EC,
      #soil_ECE= r2$Ece,
      soil_SOM= r2$OM,
      soil_N= r2$N...18,
      soil_P_available= r2$P,
      soil_K= r2$K...21*200, ## 1meq/100g = 200mg/kg
      soil_Zn= r2$Zn,
      soil_Cu= r2$Cu,
      soil_Fe= r2$Fe,
      soil_Mn= r2$Mn,
      soil_Ca= r2$Ca,
      soil_Mg= r2$Mg,
      soil_Na= r2$Na,
      soil_B= r2$B,
      soil_SOC= r2$OC
   )
   
   d  <- merge(d1, d2, by= c("location", "land_prep_method", "rep", "variety"),  all.x = TRUE)
    
   d$crop <- "potato"
   d$mulch_type <- "straw"
   d$country <- "Bangladesh"
   d$planting_date <- "2021"
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$on_farm <- TRUE
   d$yield_part <- "tubers"
   d$geo_from_source <- FALSE
   
   ## Add longitude and latitude 
   geo <- data.frame(
      location= c("Fultola", "Hogalbunia", "Moshamari"),
      latitude= c(23.8690465, 22.21285, 24.5038),
      longitude= c(90.1476495, 90.43229, 89.164334)
   )
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files (path, meta, d)
}


