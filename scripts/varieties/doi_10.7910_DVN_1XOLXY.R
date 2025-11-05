# R script for "carob"


carob_script <- function(path) {
   
"Traits related with drought resistance in common beans. A set of 36 bean genotypes belonging to the Middle American gene pool were evaluated under field conditions with two levels of water supply (irrigated and drought) over two seasons. Eight bean lines (NCB 280, NCB 226, SEN 56, SCR 2, SCR 16, SMC 141, RCB 593 and BFS 67) were identified as resistant to drought stress. Resistance to terminal drought stress was positively associated with EUW combined with increased dry matter partitioned to pod and seed production and negatively associated with days to flowering and days to physiological maturity. Based on phenotypic differences in CID, leaf stomatal conductance, canopy biomass and grain yield under drought stress, the lines tested were classified into two groups, water savers and water spenders."
   
   uri <- "doi:10.7910/DVN/1XOLXY"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3, 
      data_organization ="CIAT;UAB", 
      publication="doi:10.3389/fpls.2016.00660", 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield;fwy_total", 
      treatment_vars = "variety;irrigated", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-10-23"
   )
   
   f <- ff[basename(ff)=="02. Fisiologia 36 lineas 2012 2013.csv"]
   
   r <- read.csv(f)
   names(r) <- gsub("100SW", "SW100", names(r))
   ## processing data
   d <- data.frame(
      country= "Colombia",
      location= "Palmira",
  	  site = "CIAT campus",
      crop= "common bean",
      planting_date= as.character(r$YEAR),
      irrigated= grepl("Riego", r$STRESS),
      rep= as.integer(r$REP),
      LAI= r$LAI,
      variety= r$LINE,
      treatment= ifelse(r$STRESS=="Riego", "Irrigated", "drought"),
      yield= as.numeric(r$YDHA),
      fwy_total= as.numeric(r$CB),
      flowering_days= r$DF,
      trial_id= paste0(r$YEAR, "_", r$STRESS),
      seed_weight=r$XSW100 * 10,
      #harvest_index= r$HI,
      irrigation_number= 3L,
      irrigation_amount= 35,
      plant_spacing= 7,
      row_spacing= 60 
   )
   
   d$on_farm <- FALSE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   # CIAT campus
   d$latitude <- 3.4961
   d$longitude <-  -76.3553
   d$geo_from_source <- TRUE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
  
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}


