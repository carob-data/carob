# R script for "carob"



carob_script <- function(path) {
   
"Evaluation of elite lines for productivity diseases and drought resistance in the location Gumel"
   
   uri <- "doi:10.21421/D2/26DKAT"
   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization = "ICRISAT", 
      publication =NA, 
      project = NA, 
      data_type = "experiment",
      treatment_vars = "variety", 
      response_vars = "yield;disease_severity",
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-07-28"
   )
   
   r <- carobiner::read.excel(ff[basename(ff)=="Data file of Elite groundnut trial in Gumel 2016.xlsx"], na=c("NA",NA))
   
   d <- data.frame(
      rep =  as.integer(r$`Replication number`),
      variety = r$Genotypes,
      dmy_storage = r$DPWkgha,
      dmy_residue = r$DFWkgha,
      seed_weight = as.numeric(r$SW), ## 1000 seed weight,
      shelling_percentage = r$SH_Calc_pct,
      diseases = "rust;late leaf spot;early leaf spot",
      disease_severity = apply(r[, c("Rust_E_1to9", "LLS_E_1to9", "ELS_E_1to9")], 1, \(i) paste0(i, "(1-9)", collapse=";"))
   )
   
   d$country <- "Nigeria"
   d$location <- "Gumel"
   d$crop <- "groundnut"
   d$trial_id <- "1"
   d$irrigated <- as.logical(NA) 
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "pod"
   
   d$planting_date <- "2016"
   d$longitude  <- 9.4044169
   d$latitude  <-  12.6232524
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)    
}

