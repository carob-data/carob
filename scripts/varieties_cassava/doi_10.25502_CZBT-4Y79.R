# R script for "carob"


carob_script <- function(path) {
   
" Biofortification of Cassava using Uniform Yield Trial (21 clones) in Ibadan 2008/2009 Breeding Season "
   
   uri <- "doi:10.25502/CZBT-4Y79"
   group <- "varieties_cassava" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA, 
      data_organization ="IITA", 
      publication = NA, 
      project = NA, 
      data_type = "experiment",
      response_vars = "yield;dmy_roots;disease_severity",
      treatment_vars = "variety_code", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-09-05"
   )
   
   f <- ff[tolower(basename(ff))==tolower("2020-04-03T101459phenotype_download.csv")]
   
   r <- read.csv(f)  
   
   d <- data.frame(
      country= "Nigeria",
      #description= r$programDescription,
      planting_date= r$plantingDate,
      location= r$locationName,
      trial_id= r$observationUnitName,
      rep= r$replicate,
      variety_code= r$germplasmName,
      dmy_roots= r$dry.yield.CO_334.0000014*1000,
      yield= r$fresh.root.yield.CO_334.0000013*1000,
      harvest_index= r$harvest.index.variable.CO_334.0000015,
      mc1= r$cassava.mosaic.disease.severity.1.month.evaluation.CO_334.0000191,
      mc3= r$cassava.mosaic.disease.severity.3.month.evaluation.CO_334.0000192,
      mc6= r$cassava.mosaic.disease.severity.6.month.evaluation.CO_334.0000194,
      Bb3= r$cassava.bacterial.blight.incidence.3.month.evaluation.CO_334.0000178,
      Bb6= r$cassava.bacterial.blight.severity.6.month.evaluation.CO_334.0000176
      
   )
   d$record_id <- 1:nrow(d)
   d$planting_date <- gsub("July","07", d$planting_date)
   d$crop <- "cassava"
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$on_farm <- TRUE
   d$yield_part <- "roots"
   d$geo_from_source <- FALSE
   d$longitude <- 3.8970
   d$latitude <- 7.3786 
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   ### capturing disease score
   dmb <- names(d)[grepl("mc|Bb", names(d))]
   
   dd <- d[,c("record_id", dmb)]
   dates <- c("2008-08-09", "2008-09-09", "2008-12-09", "2008-09-09", "2008-12-09")
   x <- reshape(dd, direction="long", varying =dmb , v.names="disease_severity", timevar="step")
   x$time <- dates[x$step]
   x$diseases <- ifelse(x$step < 4, "mosaic", "bacterial blight")
   x$severity_scale <- as.character(NA) #"1-4"	# not sure  
   x$disease_severity <- as.character(x$disease_severity)
   x$id <- x$step <- NULL
   
   d[dmb] <-  NULL
   
   carobiner::write_files (path, meta, d, long=x)
   
}
   
   
   