# R script for "carob"


carob_script <- function(path) {
   
"The African Cassava Agronomy Initiative (ACAI) aims at improving cassava root yield and quality, and cassava supply to the processing sector. The project has 6 use cases of which best planting practices (BPP) is one. BPP is focusing on guiding farmers in choosing best-suited planting practices for cassava, with a focus on tillage operations and in close relation with improved weed control recommendations."
   
   uri <- "doi:10.25502/88g2-xc50/d"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA, 
      data_organization ="IITA", 
      publication= NA, 
      project="ACAI", 
      data_type= "experiment", 
      response_vars= "dmy_roots; dmy_stems; dmy_leaves", 
      treatment_vars = "N_fertilizer; P_fertilizer; K_fertilizer; Mn_fertilizer", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-11-22"
   )
   
   f <- ff[basename(ff)=="DMYield_ 2016_2017.csv"]
   
   r <- read.csv(f)
   ### Processing 
   d <- data.frame(
      planting_date= as.character(r$Year),
      country= r$Country,
      location= tolower(r$Loc),
      treatment= r$Treatment,
      #r$HarvestNumber,
      rep= r$Rep,
      N_fertilizer= r$N_application_kg_ha,
      P_fertilizer= r$P_application_kg_ha,
      K_fertilizer= r$K_application_kg_ha,
      Mn_fertilizer= r$Mn_application_kg_ha,
      dmy_leaves= r$DMLeaves_g_m2*10,
      dmy_stems= r$Dmstems_g_m2*10,
      yield= r$DMRoots_g_m2*10,
      crop= "cassava"
   )
   yield_part = "roots"
   
   d$trial_id <- paste0(d$planting_date, "-", d$location)
   d$treatment <- paste("N", d$N_fertilizer, "P", d$P_fertilizer, "K", d$K_fertilizer, sep="")
   
   ## removing 10 duplicate rows
   d <- unique(d) 
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "roots"
   
   geo <- data.frame(
      location=c("benue", "crs", "edo"),
      latitude= c(7.341605, 5.99146, 6.64397),
      longitude= c(8.73115, 8.60098, 5.9349)
   )
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

 carobiner::write_files(path, meta, d)
 
}



