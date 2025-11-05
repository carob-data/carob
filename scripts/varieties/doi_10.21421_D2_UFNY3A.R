# R script for "carob"


carob_script <- function(path) {
   
"Farmer field level Trail data has been collected under HOPE Project - Farmer Field Trials at Pune- Sorghum for the season 2011-12 Grain Yield and Fodder Yield have been recorded in different villages"
  
   uri <- "doi:10.21421/D2/UFNY3A"
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
      carob_date="2024-11-14"
   )
   
   f <- ff[basename(ff)=="Pune FLD data 2011-12.xlsx"]
   
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
      adm2= "Pune",
      planting_date= "2011"
   )
   
   d$location <- gsub("A/P-", "", d$location)
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   
   geo <- data.frame(
      location= c("Vanjarwadi, Tal Jamkhed  Dist- A’Nagar", " Tisgaon, Tal Nagar, Dist- A’Nagar", " Vanjarwadi, Tal Jamkhed  Dist- A’Nagar", " Kombali, Tal Karjat,  Dist- A’ Nagar", "Joukhede, Tal Nagar, Dist- A’Nagar",
                  "Datta’s Shingwe, Tal Pathardi Dist-A’Nagar", "Dhamori, Tal Rahuri, Dist- A’Nagar", "Datta’s Shingwe, TalPathardi Dist-A’Nagar", "Datta’s Shingwe, TalPathardi Dist- A’Nagar", " Khare Karjune, Tal Nagar, Dist- A’ Nagar", "Pimpalgaonwaga, Tal. Tal Nagar, Dist- A’ Nagar", " vambori, Tal Rahuri, Dist-A’Nagar", " Chinchole, Tal Rahuri,  Dist- A’ Nagar", " Kolhar,  Tal Pathrdi, Dist- A’ Nagar"),
      latitude= c(19.81709, 19.19108, 18.7384, 18.723009, 19.10279, 19.28088, 19.17653, 19.28082, 19.28082,  19.1624320, 19.064139, 19.290014, 20.294279, 19.205724),
      longitude= c(73.7619, 75.07004, 75.31240, 74.916656, 74.7748, 74.90930, 75.176648, 74.909329, 74.909329, 74.630510, 74.609613, 74.73153, 74.168330, 74.8893)
   )
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

