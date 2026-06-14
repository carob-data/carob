# R script for "carob"
## Many valuable variables can still be captured from raw data


carob_script <- function(path) {
   
"a) to determine the extent of the utilization of rice by-products and biomass and their importance as a source of livelihood; b) to identify farmers'problems, in the effective utilization of rice by-products; c) to identify engineering technologies for optimum utilization of biomass and increase the economic value of rice by-products." 
  
    uri <- "doi:10.7910/DVN/26231"
   group <- "survey" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1, 
      data_organization ="IRRI", 
      publication =NA,
      project =NA, 
      data_type = "survey",
      response_vars = "none",
      treatment_vars = "none", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-10-11"
   )
   
   f <- ff[basename(ff)=="CRPSYS.xls"]

   ## processing crop system data
   r <- carobiner::read.excel(f, fix_names = TRUE)
   
   d <- data.frame(
      adm2= ifelse(grepl("1", r$PROV), "Nueva Ecija", NA),
      adm3= ifelse(grepl("1", r$TOWN), "Munoz", NA),
      location= "Gabaldon",
      season= ifelse(grepl("1", r$SSON), "wet", "dry"),
      #ecosystem= r$RECO,
      variety= c("IR64", "IR64,36", "IR64,42", "C3", "IR42", "IR36", "R4", "IR64, C2", "R10, IR64", "IR64, R4", "IR64 malagkit", "R10", "Malagkit")[r$VAR],
      planting_date= c("1992-06", "1992-07", "1992-08", "1992-06","1992-07")[r$MONTH],
      irrigation_source= c("pump","rain","pump and rain")[r$WATER],
      land_ownedby= c("Owner", "Amortizing owner", "Leasehold tenant", "Share tenant", "Mortgaged")[r$TENURE],
      yield= r$GYCAV/r$ARHV,
      plot_area= r$ARHV*10000,# m2
      crop_price=  r$PKG,
      crop="rice",
      trial_id= "1"
      
   )
   
   d$country <- "Philippines"
   d$adm1 <- "Luzon"
   d$latitude <- 15.5182
   d$longitude <-  121.3081
   d$geo_from_source <- FALSE
   d$on_farm <- FALSE
   d$irrigated <- ifelse(grepl("pump", d$irrigation_source), FALSE, TRUE)
   d$is_survey <- TRUE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	d$yield_moisture <- NA_real_
	d$yield_isfresh <- TRUE

   
   carobiner::write_files (path, meta, d)
}


