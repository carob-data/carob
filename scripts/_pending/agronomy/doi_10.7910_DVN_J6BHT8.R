# R script for "carob"


## "farming_technique" is not a good variable. It appears to capture fertilizer application (method, material, and amount) and these should become treatment variables.

carob_script <- function(path) {
   
"In Ghana, Zai has been successfully been introduced on a limited scale to farmers in the East Gonja, East Mamprusi districts in the Northern region and Guru and Builsa district in Upper East region Presbyterian Agricultural Services. However, Zai farming is an innovative technology which involves the burial of manure/organic matter in holes/ pits and planting the crop later on top of the pit. The objectives of this study are to evaluate and disseminate the Zai and fertilizer Micro-dosing techniques on-farm; to undertake cost benefit analysis of the Zai and fertilizer Micro-dosing; to explore the potential increase in grain yield of Zai and fertilizer Micro-dosing over farmers practice in the Africa RISING intervention communities; to undertake comparative analysis of the effect of Zai and fertilizer Micro-dosing on maize,sorghum, and millet."
   
   uri <- "doi:10.7910/DVN/J6BHT8"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0, 
      data_organization ="IITA", 
      publication = NA, 
      project ="Africa RISING", 
      data_type = "experiment",
      response_vars = "yield;fwy_total;fwy_residue",
      treatment_vars = "farming_technique", 
      carob_contributor = "Cedric Ngakou", 
      carob_effort = NA,
      carob_date = "2024-09-14"
   )
   
   f <- ff[basename(ff)=="Data.csv"]
   
      # Read file
      r <- read.csv(f)  
      
      # Processing data 
      d <- data.frame(
         country= "Ghana",
         farming_technique= r$TRT,
         crop= r$CRP,
         adm2= ifelse(grepl("1", r$DIST), "KASSENA NANKANA", "BONGO"),
         location = ifelse(grepl("1", r$CMMTY), "Nyangua",
                                 ifelse(grepl("2", r$CMMTY), "Gia", "Samboligo")),
         plant_height= r$PLNTH,
         maturity_days= r$DPHMTY,
         plant_density= as.numeric(gsub(",", "", r$NPLTHV)),
         yield= r$GYD ,
         fwy_total= r$TBMSS,
         fwy_residue= r$FODDER,
         harvest_index=  r$HINDX,
         geo_from_source= FALSE
      )
      
   d$crop <- c("maize", "millet", "sorghum")[d$crop]
   d$farming_technique <- c("ZAI PRACTICE", "ZAI + MICRO DOSING", " MICRODOSING", "FARMER PRACTICE")[d$farming_technique]
   d$planting_date <- "2014-06-01"
   d$trial_id <- paste0(d$crop, "-", d$location)
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$on_farm <- TRUE
   d$yield_part <- "grain"
   
   ## adding longitude and latitude 
   
   geo <- data.frame(
      location=c("Gia", "Nyangua", "Samboligo") ,
      latitude= c(5.5624, 10.8805, 10.95418),
      longitude= c(-0.1949, -1.1067, -0.86039)
   )
   
   d <- merge(d, geo, by= "location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
    
   
   carobiner::write_files (path, meta, d)
}


