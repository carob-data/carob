# R script for "carob"


carob_script <- function(path) {
   
"A new methodology to estimate phenotypic differences in SNF ability. We used 15N natural abundance method to compare SNF ability estimated from shoot tissue sampled at mid-pod filling growth stage vs. grain tissue sampled at harvest. We suggest that the method of estimating Ndfa using grain tissue (Ndfa-G) could be applied in bean breeding programs to improve SNF ability. Using this method of Ndfa-G, we identified four bean lines (RCB 593, SEA 15, NCB 226 and BFS 29) that combine greater SNF ability with greater grain yield under drought stress and these could serve as potential parents to further improve SNF ability of common bean." 
   
   uri <- "doi:10.7910/DVN/1VAWHY"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=4, 
      data_organization ="CIAT;UAB", 
      publication= "doi:10.1016/j.eja.2016.05.014", 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield;fwy_total", 
      treatment_vars = "variety;irrigated;N_fertilizer", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-10-20"
   )
   
    f <- ff[basename(ff)=="02. Mesos 2012_2013_SNF_Datos.csv"]
    
   r <- read.csv(f)
   
   ## processing data
   d <- data.frame(
      country= "Colombia",
      location= "Palmira",
	  site = "CIAT campus",
      crop= "common bean",
      planting_date= as.character(r$YEAR),
      irrigated= grepl("Riego", r$STRESS),
      rep= r$REP,
      LAI= r$LAI,
      variety= r$LINE,
      treatment= ifelse(r$STRESS=="Riego", "Irrigated", "drought"),
      yield= r$YDHA,
      fwy_total= r$CB,
      flowering_days= r$DF,
      #grain_N= r$NSEED,
      trial_id= paste0(r$YEAR, "_", r$STRESS),
      irrigation_number= 3L,
      irrigation_amount= 35,
      irrigation_method= "furrow",
      row_spacing= 60,
      plant_spacing= 7,
      plant_density=24*10000,
      N_fertilizer= r$YDHA/r$NUE
      )
      
   d$on_farm <- FALSE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"

   # CIAT campus
   d$latitude <- 3.4961
   d$longitude <-  -76.3553
   
   d$geo_from_source <- TRUE
   
   d$P_fertilizer <- d$K_fertilizer <- 0

	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

