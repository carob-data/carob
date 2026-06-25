# R script for "carob"

carob_script <- function(path) {
   
" 4 varieties of potato are recommended for release following 2 seasons of National Performance Trial evaluation."
   
   uri <- "doi:10.21223/CEDN2O"
   group <- "varieties_potato" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1, 
      data_organization = "CIP", 
      publication=NA, 
      project= NA,
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_effort = NA,
      carob_date="2024-09-10"
   )
   
   ### process legume yield file
   f <- ff[basename(ff)=="01-9778-Data-varieties.xlsx"]
   r <- carobiner::read.excel(f, na=("*"))
   
  d <- data.frame(
     country= "Rwanda",
     crop= "potato",
     location= gsub("_4x|16A.","",r$Local),
     planting_date= as.character(as.Date(r$Planting,"%d/%m/%Y")),
     harvest_date=  as.character(as.Date(r$Harvest,"%d/%m/%Y")),
     variety= r$Variety,
     yield= r$yield*1000, # kg/ha
     trial_id= r$Local_var
  )
   
   d$planting_date[is.na(d$planting_date)] <- "2016"
   d$irrigated <- NA
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "tubers"
   d$geo_from_source <- FALSE
  
   ## Fixing longitude 
   
   geo <- data.frame(
      location= c("Bugeshi", "Burera", "Gihonga", "Muhoza", "Nyabihu"),
      latitude= c(-1.6503, -1.46088, -1.6912432, -1.6411383, -1.6411383),
      longitude= c(29.5718, 29.80119, 29.7659862, 29.5180007, 29.5180007)
   )
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   
   carobiner::write_files(path, meta, d)
}

