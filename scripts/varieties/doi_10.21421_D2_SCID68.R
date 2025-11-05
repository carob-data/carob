# R script for "carob"


carob_script <- function(path) {
   
"Farmer field level Trail data has been collected under HOPE Project - Farmer Field Trials at Solapur - Sorghum for the season 2011-12 Grain Yield and Fodder Yield have been recorded in different villages"
   
   uri <- "doi:10.21421/D2/SCID68"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4, 
      data_organization ="ICRISAT", 
      publication= NA, 
      project= "HOPE", 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-11-18"
   )
   
   f <- ff[basename(ff)=="Solapur FLD data 2011-12.xlsx"]
   
   r <- carobiner::read.excel(f,sheet = "Observation", na=c("."))
   ### Process data
   
   d <- data.frame(
      trial_id= r$Farmer_name,
      location= r$Village,
      variety= r$Variety,
      yield= as.numeric(r$Grain_Yield)*100,
      fwy_total= as.numeric(r$Fodder_Yield)*100,
      crop= "sorghum",
      country= "India",
      adm1= "Maharashtra",
      adm2= "Solapur",
      planting_date= "2011"
   )
   
   d <- d[!is.na(d$yield),]
   d$location <- gsub("A/P-", "", d$location)
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   
   geo <- data.frame(
      location= c(" Bandarkapthe Tal SouthSolapur, Dist- Solapur", " Degaon Tal Pandharpur, Dist- Solapur", " Raleras Tal North Solapur, Dist- Solapur", " Maniknal, Tal Jat, Dist: Sangli", " Vaikwak Tal SouthSolapur, Dist- Solapur", "Kombhali, Tal Karjat, Dist. A, Nagar", " Mulegaon Tal South Solapur, Dist- Solapur", " Mangi No.1 Tal Karmala, Dist- Solapur", " Anjangao(khu) Tal Madha, Dist- Solapur", " Bhogaon Tal North Solapur, Dist- Solapur",    
                  " Panmangrul Tal Akhalkot, Dist- Solapur", " Achegaon Tal South Solapur, Dist- Solapur"),
      latitude= c(17.8929601, 17.6659105, 18.00216, 13.35422, 17.66570, 18.72285, 17.685554, 18.405339, 17.93644, 17.73297, 17.52479, 20.971911),
      longitude= c(75.024468, 75.864956, 75.82378, 74.78704, 75.90522, 74.917386, 75.968907, 75.195278, 75.54618,  75.89036, 76.20565, 75.92171)
   )
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

