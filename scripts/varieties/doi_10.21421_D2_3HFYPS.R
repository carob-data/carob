# R script for "carob"


carob_script <- function(path) {
   
"Farmer field level Trail data has been collected under HOPE Project - Farmer Field Trials at Ahmednagar Followup data for Phule Vasudha variety of Sorghum for the season 2009-10 and 2010-11. Grain Yield and Fodder Yield have been recorded in the villages Bhoirepathar, Hivarebajar, Jakhangaon, Pimpalgaonkauda, TakaliKhatgaon of Ahmednagar district of Maharashtra state."
   
   uri <- "doi:10.21421/D2/3HFYPS"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5, 
      data_organization ="ICRISAT", 
      publication= NA, 
      project= "HOPE", 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-11-21"
   )
   
   f <- ff[basename(ff)=="Ahmednagar Followup data 2009-10 and 2010-11.xlsx"]
   
   r <- carobiner::read.excel(f,sheet = "Observation", na=c("."))
   ### Process data
   
   d <- data.frame(
      trial_id= r$Farmer_name,
      location= r$Village,
      variety= r$Variety,
      yield= as.numeric(r$GY_local)*100,
      fwy_total= as.numeric(r$Fy_local)*100,
      crop= "sorghum",
      country= "India",
      latitude= r$Latitude,
      longitude= r$Longitude,
      geo_from_source= TRUE,
      adm1= "Maharashtra",
      planting_date= "2009"
   )
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"

   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

