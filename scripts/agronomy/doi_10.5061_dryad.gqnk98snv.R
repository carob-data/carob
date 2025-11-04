# R script for "carob"


carob_script <- function(path) {
   
"Rice (Oryza sativa L.) re-cultivation plays an important role in increasing land productivity and efficiency in crop rotation. Planting density (PD) is an important agronomic factor to achieving maximum grain yield (GY). The current study aimed to determine the best PD for the first cultivation and re-cultivation of rice. The experiment was conducted as a split plot based on the randomized complete block design with three replications at Ghaemshahr University, Mazandaran (northern Iran) from 2013 to 2014. Treatments consisted of cultivar, i.e. Tarom Hashemi and Koohsar, at two levels as the main factor and PDs at the three levels of 16, 25, and 33.3 hills m-2 with planting spaces of 25 × 25, 20 × 20, and 30 × 10 cm2, respectively, as the sub factor. The results of the current study showed that cultivar for the first cultivation, and year for the re-cultivation had a significant effect on GY. PD had a significant effect on GY in both first cultivation and re-cultivation. The GY of Hashemi in the first cultivation (36.1%) and re-cultivation (18.5%) was higher than that of Koohsar. GY in the first cultivation and re-cultivation had an increasing trend; PD increased up to 33.3 hills m-2 as 19.9% and 21.4% in the first cultivation and re-cultivation, respectively, due to the increase in number of panicles m-2 (30.1% and 30.6%, respectively). Hashemi is suitable for the first cultivation. It is noteworthy, also, that a density of 33.3 hills m-2 is recommended for the first cultivation and re-cultivation."
  
   uri <- "doi:10.5061/dryad.gqnk98snv"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=NA, 
		data_organization = "IAU", #Islamic Azad University, Tehran
		publication="doi:10.1002/agg2.20213", 
		project=NA, 
		data_type= "experiment", 
		response_vars= "yield; fwy_total", 
		treatment_vars = "plant_density; variety", 
		carob_contributor= "Cedric Ngakou", 
		carob_date="2025-06-09",
		notes=NA
   )
   
   ff <- ff[grepl("csv", basename(ff))] 
   
   procs <- function(f){   
      r1 <- read.csv(f) 
      data.frame(
         planting_date= as.character(r1$Year),
         rep= r1$Replicate,
         variety= r1$Cultivar,
         plant_density= r1$Density*10000, # plants/ha or hills/ha
         plant_height= r1$Plant.height,
         #leaf_length= r1$Flag.leaf.length,
         #r1$Tillers,
         #r1$Panicles,
         seed_weight= r1$Thousand.grain.weight*10,
         fwy_total= r1$Biological.yield,
         yield= r1$Grain.yield,
         harvest_index= r1$Harvest.index,
         crop= "rice",
         country= "Iran",
         adm1= "Mazandaran",
         location= "Ghaemshahr University",
         latitude= 36.466, 
         longitude= 52.866, 
         geo_from_source= TRUE,
         elevation= 51.2,
         trial_id= gsub(".csv", "", basename(f)),
         on_farm = TRUE, 
         is_survey= FALSE,
         irrigated= NA,
         yield_part= "grain"
      )
      
   }
    
   d <-  lapply(ff, procs) 
   d <- do.call(rbind, d)   
   
   ### Adding soil information from paper
   
   SI <- data.frame(
      planting_date= c("2013", "2014"),
      soil_pH= c(7.72, 6.94),
      soil_SOM= c(1.91, 2.85),
      soil_sand= c(42, 32),
      soil_silt= c(30, 39),
      soil_clay= c(28, 29),
      soil_EC= c(0.43, 0.34),
      soil_type= c(rep("clay loam", 2)),
      soil_N= c(0.11, 0.16),
      soil_P= c(42, 36),
      soil_K= c(213, 181)
   )
   
   d <-  merge(d, SI, by="planting_date", all.x = TRUE)
   
   ## Adding planting data 
   pd <- data.frame(
      trial_id= c(rep("first_cultivation", 4), rep("re-cultivation", 4)),
      planting_date= c(rep("2013",2), rep("2014", 2), rep("2013",2), rep("2014", 2)),
      variety= c(rep(c("Koohsar","Tarom Hashemi"),  times= 2), rep(c("Koohsar","Tarom Hashemi"),  times = 2) ),
      plantD= c(rep("2013-05-25", 2), rep("2014-05-02", 2), "2013-08-25", "2013-08-13", "2014-08-17", "2014-08-01" ),
      harvest_date= c("2013-07-28", "2013-08-06", "2014-07-16", "2014-07-27", "2013-12-08", "2013-11-26", "2014-12-06","2014-11-02")
   )
   
   d <- merge(d, pd, by= c("trial_id", "planting_date", "variety"), all.x = TRUE) 
   
   d$planting_date <- d$plantD   
   d$plantD <- NULL       
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}


