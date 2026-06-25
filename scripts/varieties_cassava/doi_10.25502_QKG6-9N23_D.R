# R script for "carob"
   
carob_script <- function(path) {
      
"Preliminary Yield Trial using 14 GS C2 clones in Ibadan 2015/2016 Breeding season" 
      
      uri <- "doi:10.25502/QKG6-9N23/D"
      group <- "varieties_cassava" 
      ff  <- carobiner::get_data(uri, path, group)
      
      meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA, 
         data_organization ="IITA", 
         publication = NA, 
         project = NA, 
         data_type = "experiment",
         response_vars = "yield;fwy_storage;disease_severity",
         treatment_vars = "variety", 
         carob_contributor = "Cedric Ngakou", 
         carob_effort = NA,
         carob_date = "2024-09-17",
         notes = NA
      )
      
      f <- ff[basename(ff)=="2019-06-10t045818phenotype_download.csv"]
      
      r <- read.csv(f)  
      
	  plotSize <- r$plotWidth * r$plotLength
      d <- data.frame(
         country= "Nigeria",
         #description= r$programDescription,
         planting_date= r$plantingDate,
         harvest_date= r$harvestDate,
         location= r$locationName,
         trial_id= r$observationUnitName,
         rep= r$replicate,
         variety= r$germplasmNam,
         plot_width= r$plotWidth,
         plot_length= r$plotLength,
         plot_area= r$plotWidth*r$plotLength, # m2
         fwy_residue= 1000 * r$fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016 / plotSize,
         yield = 1000 * r$fresh.storage.root.weight.per.plot.CO_334.0000012 / plotSize,
         mc1= r$cassava.mosaic.disease.severity.1.month.evaluation.CO_334.0000191,
         mc3= r$cassava.mosaic.disease.severity.3.month.evaluation.CO_334.0000192,
         Bb3= r$cassava.bacterial.blight.severity.3.month.evaluation.CO_334.0000175        
      )
	  d$fwy_storage = d$yield
	  
      d$record_id <- 1:nrow(d)
      d$planting_date <- gsub("December", "12", d$planting_date)
      d$harvest_date <- gsub("October", "10", d$harvest_date)
      d$crop <- "cassava"
      d$irrigated <- NA
      d$is_survey <- FALSE
      d$inoculated <- FALSE
      d$on_farm <- TRUE
      d$yield_part <- "roots"
      d$geo_from_source <- FALSE
      d$longitude <- 3.9435
      d$latitude <- 7.3808 
      
      d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
      
      ### capturing disease score
      dmb <- names(d)[grepl("mc|Bb", names(d))]
      
      dd <- d[,c("record_id", dmb)]
      dates <- c("2016-01-09", "2016-03-09", "2016-03-09")
      x <- reshape(dd, direction="long", varying =dmb , v.names="disease_severity", timevar="step")
      x$time <- dates[x$step]
      x$diseases <- ifelse(x$step < 3, "mosaic", "bacterial blight")
      x$severity_scale <- as.character(NA) #perhaps "1-4?  
      x$disease_severity <- as.character(x$disease_severity)
      x$id <- x$step <- NULL      
      d[dmb] <-  NULL
      
      carobiner::write_files (path, meta, d, long=x)      
}
   
   
   

