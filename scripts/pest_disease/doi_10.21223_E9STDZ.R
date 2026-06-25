# R script for "carob"

carob_script <- function(path) {
   
"Dataset about the yield and stability advanced trial for late blight and heat tolerant (LBHT) potato population conducted in Huanuco, Peru. 150 advanced clones of the LBHT and heat-tolerant population, with three control varieties Yungay, Kory, and Amarilis, besides with 23 parents were planted in Huanuco, Peru between 2021 and 2022. (16 Rows x 12 Columns)"
   
   uri <- "doi:10.21223/E9STDZ"
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
      carob_date="2024-06-21"
   )
   
   r <- carobiner::read.excel(ff[basename(ff)=="01_data_advanced_trial_LBHTC2_Huanuco_2021-2022.xlsx"])
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
      irrigated= FALSE,
      yield_part= "tubers",
      trial_id= "1" ,
      record_id= 1:nrow(r)
   )
   
   d$adm1 <- "Huanuco"
   d$longitude <- -75.833333
   d$latitude <- -9.5
   d$geo_from_source <- FALSE
   d$planting_date <- as.character(as.Date("2021-09-21"))
   d$harvest_date  <- as.character(as.Date("2022-02-10"))
	d$pathogen <- "Phytophthora infestans"
	d$diseases <- "potato late blight"
	d$is_survey <- FALSE
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   
   ### Add disease scores during the season
   dd <- r[,lbvars]
   dd$record_id <- as.integer(1:nrow(dd))
   dates <- as.character(as.Date(c("2021-10-31", "2021-11-06", "2021-11-14", "2021-11-21", "2021-11-28",  "2021-12-5", "2021-12-12","2021-12-19")))
   x <- reshape(dd, direction="long", varying =lbvars, v.names="disease_severity", timevar="step")
   x$time <- dates[x$step]
   x$step <- x$id <- NULL  
   x$disease_severity <- as.character(x$disease_severity)
   
   carobiner::write_files(path, meta, d, long=x)  
}

