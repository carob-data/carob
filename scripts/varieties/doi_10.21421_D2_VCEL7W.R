# R script for "carob"


carob_script <- function(path) {
   
"Farmer field level Trail data has been collected under HOPE Project - Farmer Field Trials at Solapur - Sorghum for the season 2009-10 Grain Yield and Fodder Yield have been recorded in different villages"
   
   uri <- "doi:10.21421/D2/VCEL7W"
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
   
   f <- ff[basename(ff)=="Solapur FLD data 2009-10.xlsx"]
   
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
      planting_date= "2009"
   )
   
   d <- d[!is.na(d$yield),]
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   
   geo <- data.frame(
      location= c("Sasure Tal Barshi", "Mandrup Tal S. Solapur", "Ropale (k) Tal Mhada", "Jeur TalKarmala"),
      latitude= c(18.017894, 17.495322, 17.78692, 18.260449 ),
      longitude= c(75.80001, 75.821242, 75.41780, 75.163392)
   )
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

