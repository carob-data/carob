# R script for "carob"

carob_script <- function(path) {
   
"Panel evaluation for resistance to late blight in advanced potato clones from populations: B: B1C5, B3C1, B3C2 - 369 clones with resistance to Late Blight and LTVR- 294 clones with resistance to virus PVX, PVY and/or PLRV and BW - 24 clones with Bacterial Wilt resistant, under natural infection in Oxapampa, planted in September 2014 and harvested in January 2015, using observation plots, three replications/ten plants. Clones from population B showed in average AUDPC value of 185, and clones from population LTVR showed AUDPC average of 1630. This inforamtion will be used in manuscript “Global multi-environment resistance QTL for foliar late blight resistance in tetraploid potato with tropical adaptation"
 
   uri <- "doi:10.21223/6TRC9T"
   group <- "pest_disease"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
      data_organization = "CIP",
      publication="doi:10.1093/g3journal/jkab251",
      project=NA,
      data_type= "experiment",
      response_vars = "yield",
      treatment_vars = "variety",
      carob_contributor= "Cedric Ngakou",
      carob_effort = NA,
      carob_date="2024-06-22"
   )
   
   r1 <- carobiner::read.excel(ff[basename(ff)=="PTLate blight092014_OXAPMP_exp8.xlsx"],sheet = "Fieldbook")
   
   d <- data.frame(
      crop="potato",
      variety= r1$INSTN,
      rep= as.integer(r1$REP),
      yield=(r1$TTWP/2.7 )*10000, # to kg/ha
      AUDPC= r1$AUDPC/100,
      on_farm= TRUE,
      irrigated= NA,
      inoculated= FALSE,
      yield_part= "tubers",
      trial_id= "1",
      record_id= 1:nrow(r1)
   )
   ## Add more variables
   m <- carobiner::read.excel(ff[basename(ff)=="PTLate blight092014_OXAPMP_exp8.xlsx"], sheet = "Minimal")
   r2 <- data.frame(rbind(m$Value))
   names(r2) <- m$Factor
   d$country <- r2$Country
   d$adm1 <- r2$Admin1
   d$adm2 <- r2$Admin2
   d$adm2 <- r2$Admin2
   d$adm3 <- r2$Admin3
   d$location <- r2$Locality
   d$latitude <- as.numeric(r2$Latitude)
   d$longitude <- as.numeric(r2$Longitude)
   d$elevation <- as.numeric(r2$Elevation)
   d$geo_from_source <- TRUE
   d$planting_date <- as.character(as.Date(r2$Begin_date, "%d/%m/%Y"))
   d$harvest_date <- as.character(as.Date(r2$End_date, "%d/%m/%Y"))
   d$plant_spacing <- 30 
   d$row_spacing <- 90
   d$plant_density <- 37037.03037037
   d$plot_area <- 2.7 # m2
   
   ### disease scores during the season
   lbvars <- grep('^LB', colnames(r1), value=TRUE)
   dd <- r1[,lbvars]
   dd$record_id <- as.integer(1:nrow(dd))
   dates <- c("2014-10-31","2014-10-27", "2014-11-03", "2014-11-11", "2014-11-19",  "2014-11-25", "2014-12-02")
   x <- reshape(dd, direction="long", varying =lbvars, v.names="disease_severity", timevar="step")
   x$time <- dates[x$step]
   x$step <- x$id <- NULL 
   x$disease_severity <- as.character(x$disease_severity)
   
   ### fertilizer
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$pathogen <- "Phytophthora infestans"
	d$diseases <- "potato late blight"
   d$is_survey = FALSE

   carobiner::write_files(path, meta, d, long=x) 
   
}

