# R script for "carob"


carob_script <- function(path) {
   
"Final dataset from agronomic experiment in Gumara Maksegnit (2016), as elaborated by GARC researcher in charge for this trial (Muuz Gebretsadik). Please contact author and contact person at ICARDA to obtain more detailed metadata or to propose collaboration."
   
   uri <- "hdl:20.500.11766.1/FK2/M95ZDZ"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0, 
      data_organization ="ICARDA;ARARI", 
      publication= NA, 
      project= NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "irrigated", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-11-06"
   )
   
   FF <- ff[grepl("Onion", basename(ff))]
   
   ## processing  data
   process <- function(f){
      r <- read.csv(f, sep=";")
      treat <- read.csv(ff[basename(ff)=="Treatment.csv"], sep=";")
      d1 <- data.frame(
         planting_date= c("2015", "2016")[r$Year],
         treatment= substr(treat$Treatment_Detail, 1, 35)[r$Treatment],
         rep= r$Replicate,
         location= r$Site_Name,
         yield_marketable= r$Yld_Marketable,
         yield= r$Yld_Total,
         crop= "onion",
         country= "Ethiopia",
         trial_id= as.character(r$Year)
         )
   }
   
   d <- lapply(FF, process)
   d <- do.call(rbind, d)
   
   d$location[is.na(d$location)] <- "Gumara Maksegnit"
   d$treatment <- gsub(": In the|: Only from |: From 90 d| T|: Throughout th|: Farmers use borde|: On", "", d$treatment)
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- TRUE
   d$yield_part <- "stems"
   
   geo <- data.frame(
      location=c("Ambachew", "Abera", "Gumara Maksegnit"),
      latitude= c(11.61237, 11.57234, 12.3813),
      longitude= c(37.4275, 37.382830, 37.5558)
   )
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files(path, meta, d)
}

