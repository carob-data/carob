# R script for "carob"


carob_script <- function(path) {
   
"The dataset includes observation data about the experiment FLRP_FBMPYT_2023: FLRP-Faba Bean Multilocation Preliminary Yield Trial-2023. The experiment is part of faba bean nursery-36 (2022-2023). Location: PAU, Punjab."
   
   uri <- "hdl:20.500.11766.1/FK2/DS0WJN"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=5, minor=0, 
      data_organization ="ICARDA", 
      publication= NA, 
      project= NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-10-31"
   )
   
   f1 <- ff[basename(ff)=="FBMPYT_2023.csv"]
   f2 <- ff[basename(ff)=="Trial_Info_2023.csv"]
   
   ## processing yield data
   r1 <- read.csv(f1, sep=";")
   d1 <- data.frame(
      ENTRY= r1$Entry_No,
      rep= r1$Rep_No,
      flowering_days= r1$Flw_dt_day,
      maturity_days= r1$Mat_dtFirstPd_day,
      plant_height= r1$PH_M_cm,
      seed_weight= r1$HSW_M_g*10,
      yield= (r1$SYld_M_gplot/4*0.5)*10, # kg/ha
      crop= "faba bean",
      adm1= "Punjab",
      country= "India",
      plot_length= 4,
      plot_width= 0.5
   )
   ## processing variety data
   r2 <- read.csv(f2, sep=";")
   d2 <- data.frame(
      ENTRY= r2$Entry_No,
      variety= r2$Designation,
      variety_pedigree= r2$Pedigree
   ) 
   
   d <- merge(d1, d2, by="ENTRY", all.x = TRUE)
   d$ENTRY <- NULL
   
   d$trial_id <- "1"
   d$planting_date <- "2022"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "seed"
   d$latitude <- 30.90424
   d$longitude <- 75.8065
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

