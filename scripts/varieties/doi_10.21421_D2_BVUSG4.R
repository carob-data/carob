# R script for "carob"


carob_script <- function(path) {
   
"Farmer field level Trail data has been collected under HOPE Project - Farmer Field Trials at Ahmednagar- Sorghum for the season 2009-10 Grain Yield and Fodder Yield have been recorded in different villages"
   
   uri <- "doi:10.21421/D2/BVUSG4"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3, 
      data_organization ="ICRISAT", 
      publication= NA, 
      project= NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-11-11"
   )
   
   f <- ff[basename(ff)=="Ahmednagar FLD data 2009-10.xlsx"]
   
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
      adm2= "Ahmednagar",
      planting_date= "2009"
   )
   d <- d[!is.na(d$yield),]
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   
   geo <- data.frame(
      location= c("Bherdapur Tal Shrirampur", "Ruichtrapati Tal Parner", "Ralegansiddhi Tal Parner", "Padhegaon Tal Shrirampur", "Mohaj Devadhe Tal Pathardi", "Mulewadi Tal Karjat", "Kharwandi Tal Newasa", "Sarola Kasar Tal Parner",
                  "Dahegaon Tal Shevgaon", "Lohagaon Tal Newasa", "Padali Tal Parner", "Chinchodi Patil Tal Nagar", "Dulechndgaon Tal Pathardi", "Kasar Pimpalgaon Tal Pathardi", "Bhistbag Tal Nagar", "Karanji Tal Pathardi", "Nadurhki Tal Rahata", "Ruicchatishi Tal Nagar", "Arangaon Tal Jamkhed", "Karjat Tal Karajat" ),
      latitude= c( 19.6202, 19.00039, 19.00039, 19.6202, 19.17653, 18.91105, 19.550115, 18.9638, 19.3514, 19.60413, 17.4809, 18.996, 19.17669, 19.2178, 19.13256, 19.93881, 19.74230, 18.90154, 19.02678, 18.9113),
      longitude= c(74.6559, 74.43965, 74.43965, 74.6559, 75.17533, 73.32836, 74.92881, 74.6550, 75.2192, 75.2644, 74.18642, 74.9023, 75.1749, 75.0975, 74.73819, 74.52779, 74.4478, 74.83753, 74.714820, 73.3288)
   )
   d$geo_from_source <- FALSE
  
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

