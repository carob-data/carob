# R script for "carob"



carob_script <- function(path) {
   
" Experiments were installed in Cajamarca, with the objective of identifying clones with high potential for being varieties applying the Participatory Varietal Selection methodology. For the period 2016-2017, 18 clones with high resistance to late blight were planted, belonging to the B population and developed in the International Potato Center together with Two control varieties, Amarilis and Yungay (susceptible). Finally, in the harvest 5 clones with high yield, low glycoalkaloid content and good organoleptic quality were selected as a result of the Participatory Variety Selection of the farmers and the analysis of mixed models and BLUPs for the yield data. The 5 selected clones were planted again in the period 2017-2018 and through the Participatory Varietal Selection, three promising clones were selected (CIP308488.92, CIP308495.227 and CIP308478.59). " 
   
   uri <- "doi:10.21223/UGHHPK"
   group <- "varieties_potato"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2, 
      data_organization = "CIP", 
      publication ="doi:10.1007/s11540-021-09495-z", 
      project = NA, 
      data_type = "experiment", 
      response_vars = "yield",
      treatment_vars = "variety_code", 
      carob_contributor = "Cedric Ngakou", 
      carob_effort = NA,
      carob_date = "2024-07-08"
   )
   
   ff <- ff[grep("PTPVS", basename(ff))]
   
   
   process <- function(f) {
      r1 <- carobiner::read.excel(f, sheet="F4_harvest_mother")
      r2 <- carobiner::read.excel(f , sheet = "Minimal")
      m <- data.frame(rbind(r2$Value))
      names(m) <- r2$Factor
      
      data.frame(
         rep= as.integer(r1$REP),
         variety_code= r1$INSTN,
         yield=  (r1$TTYNA)*1000, ## kg/ha
         trial_id= basename(f),
         country= m$Country,
         adm1= m$Admin1,
         adm2= m$Admin2,
         adm3= m$Admin3,
         location= m$Locality,
         longitude= as.numeric(m$Latitude),
         latitude= as.numeric(m$Longitude),
         elevation= as.numeric(m$Elevation),
         planting_date= as.character(as.Date(m$Begin_date, "%d/%m/%Y")),
         harvest_date= as.character(as.Date(m$End_date, "%d/%m/%Y"))
      ) 
   }
   
   d <- lapply(ff, process) 
   d <- do.call(rbind, d)
   
   d$irrigated <- as.logical(NA)
   d$inoculated <- FALSE
   d$is_survey <- FALSE
   d$on_farm <- TRUE
   d$crop <- "potato"
   d$yield_part <- "tubers"
   d$geo_from_source <- TRUE
   d$row_spacing <- 100  
   d$plant_spacing <- 30 
   d$harvest_days <- 120
   
   ## Fixing planting_date
   i <- grep("exp3.xlsx",d$trial_id)
   d$planting_date[i] <- "2017"
   
   # fertilizer 180-160-160 of NPK from paper 
   
   d$N_fertilizer <- 180
   d$P_fertilizer <- 160 / 2.29
   d$K_fertilizer <- 160 / 1.2051
   d$fungicide_used <- TRUE
   d$fungicide_product <- "mancozeb"
   
   
   carobiner::write_files (path, meta, d) 
   
}
