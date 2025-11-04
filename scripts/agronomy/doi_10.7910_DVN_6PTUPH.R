# R script for "carob"

carob_script <- function(path) {
   
"This dataset was generated from the study conducted to determine whether there is any significant difference in the uptake of sustainable intensification (SI) technologies by farmers because of their interaction with the research team during project implementation. Africa RISING project started interacting with farmers during the 2012/13 cropping season in three agro-ecologies in central Malawi. International Maize and Wheat Improvement Center (CIMMYT) started interacting with farmers who were using conservation agriculture-based SI technologies since the 2007/2008 cropping season in three agro-ecologies in central Malawi. In this study, both CIMMYT and Michigan State University (MSU) led trials were selected. Over time, some new farmers were engaged, creating an opportunity to also study exposure time as a factor to understand the intensity and use of SI technologies. We grouped farmers into two categories to assess the effect of exposure time: (1) farmers who were engaged at the onset of the project (2012/2013), and (2) farmers who were engaged starting 2016/2017 cropping season. Farmers were primarily engaged at different levels"

   uri <- "doi:10.7910/DVN/6PTUPH"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1, 
      data_organization = "MSU",
      publication=NA, 
      project= "AfricaRISING",
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-08-18",
      notes = "treatment farmer_category (mother/baby/control) not considered"
   )
   
   ### process legume yield file
   ff <- ff[grep("csv", basename(ff))]
   
   process <- function(f) {
      
     r <- read.csv(f)
     if (is.null(r$Average.yield.field..kg.ha.)) {
       r$Average.yield.field..kg.ha. <- rowMeans(r[, c("yield..kg.ha..point.A", "yield..kg.ha..point.B", "Grain.yield..kg.h.A.APa..point.C")])
     }
   
     data.frame(
         country=  r$Country,
         adm1= r$District,
         location= r$EPA,
         crop= tolower(r$Crop),
         variety= r$Variety,
         farmer_category= r$Farmer.category,
         plot_area=(r$Net.plot.A..m2.+r$Net.plot.B +r$Net.plot.C)/3,
         yield= r$Average.yield.field..kg.ha.,
         plant_density= rowMeans(r[,c("plant.count..A.","plant.count..B.","plant.count..C.")]),
         cob_density= ifelse(grepl("Maize",r$Crop),rowMeans(r[,c("no.of.cobs..A.","no.of.cobs..B.","no.of.cobs..C.")]), NA),
         trial_id = gsub(".csv","", basename(f))  
      )  
   }
                               
   d <- lapply(ff, process) 
   d <- do.call(rbind, d)
   
   d$plant_density <- (d$plant_density/d$plot_area) * 10000 ## plant/ha
   d$cob_density <- (d$cob_density/d$plot_area) * 10000  ## cob/ha
   ## removing two rows with plot_area and yield zero (0) 
   d <- d[d$yield > 0,  ]
   
   ## Fixing crop names
   d$crop <- gsub("soya bean|soyobean", "soybean", d$crop)
   d$crop <- gsub("mzama|zama", "bambara groundnut", d$crop) # 
   d$crop <- gsub("beans", "common bean", d$crop) 
   
   d$irrigated <- NA
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   d$geo_from_source <- FALSE
   d$planting_date <- "2012"
   
   ## Fixing longitude 
   
   geo <- data.frame(
      location= c("Songani", "Mtubwi", "Bazale", "Golomoti", "Kandeu", "Linthipe"),
      latitude= c(-15.3156, -14.9027, -15.04847, -14.4156, -14.6039, -14.1748),
      longitude= c(35.3923, 35.60258, 35.05315, 34.60278, 34.6172, 34.1243)
   )
      
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   d$farmer_category <- NULL
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

