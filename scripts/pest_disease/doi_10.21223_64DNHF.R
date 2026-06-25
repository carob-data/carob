# R script for "carob"

carob_script <- function(path) {
   
"Dataset for yield and stability advanced trial for late blight and heat tolerant (LBHT) potato population conducted in Oxapampa, Peru. 150 advanced clones of the LBHT and heat-tolerant population, with three control varieties Yungay, Kory, and Amarilis, besides with 23 parents were planted in Oxapampa, Peru between 2021 and 2022. (16 Rows x 12 Columns)"
   
   uri <- "doi:10.21223/64DNHF"
   group <- "pest_disease"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
      data_organization = "CIP",
      publication= NA,
      project=NA,
      data_type= "experiment",
      response_vars = "yield",
      treatment_vars = "variety",
      carob_contributor= "Cedric Ngakou",
      carob_effort = NA,
      carob_date="2024-06-26"
   )
   
   r <- carobiner::read.excel(ff[basename(ff)=="01_Advanced_Trial_LBHTC2_Oxapampa_2021-2022.xlsx"])
   lbvars <- grep('^LB', colnames(r), value=TRUE)
   d <- data.frame(
      country="Peru",
      crop="potato",
      variety= r$CIPN,
      rep= as.integer(r$Rep),
      yield=r$MTYNA*1000, # to kg/ha
      nodule_weight= r$`Fresh weight`,
      AUDPC= r$AUDPC /100,
      on_farm= TRUE,
      inoculated= FALSE,
      irrigated= NA,
      yield_part= "tubers",
      trial_id= "1",
	  is_survey = FALSE,
	  record_id= 1:nrow(r)
   )
   

   ## Location
   d$adm1 <- "Oxapampa"
   d$longitude <- -75.0833
   d$latitude <- -10.3333
   d$geo_from_source <- FALSE
   d$planting_date <- "2021-09-30"
   d$harvest_date  <- "2022-01-31"
	d$pathogen <- "Phytophthora infestans"
	d$diseases <- "potato late blight"
   
   ## Disease scores during the season
   dd <- r[,lbvars]
   dd$record_id <- as.integer(1:nrow(dd))
   dates <- as.character(as.Date(c("2021-11-08", "2021-11-15", "2021-11-22", "2021-11-29", "2021-12-06", "2021-12-13",  "2021-12-20", "2021-12-27")))
   x <- reshape(dd, direction="long", varying =lbvars, v.names="disease_severity", timevar="step")
   x$time <- dates[x$step]
   x$step <- x$id <- NULL  
   x$disease_severity <- as.character(x$disease_severity)
   ## fertilizer
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   d$is_survey = FALSE

   carobiner::write_files(path, meta, d, long=x)  
}

