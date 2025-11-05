# R script for "carob"

carob_script <- function(path) {

"Medium-Duration Multi-location trial conducted under Post-season .The trial was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses across the location which includes, Fud(Federal University Dutse, Nigeria)"
   
   uri <- "doi:10.21421/D2/THR8L8"

   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization = "ICRISAT", 
      publication =NA, 
      project = NA, 
      data_type = "experiment", 
      response_vars = "dmy_storage",
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-08-09"
   )
   
   f <- ff[basename(ff)=="Data file of medium duration multi-location trial post rainy in Fud.xlsx"]
  r <- carobiner::read.excel(f, fix_names=TRUE)
   
   d <- data.frame(
      rep= as.integer(r$Replication.number),
      variety= r$Variety,
      dmy_storage = r$DPWkgha,
      dmy_residue= r$DFWkgha,
      fwy_residue= r$Fykg_h,
      emergence_days= r$DEM,
      flowering_days= r$DFF,
      maturity_days= r$DM,
      shelling_percentage= r$Shelling.ratio,
      seed_weight = as.numeric(r$HSW)* 10, ## 1000 seed weight,
      diseases = "rust;late leaf spot;early leaf spot",
      disease_severity = apply(r[, c("RUST", "LLS", "ELS")], 1, \(i) paste0(i, "(1-9)", collapse=";"))
      )

   d$country <- "Nigeria"
   d$adm1 <- "Dutse"
   d$location <- "Federal University Dutse"
   d$crop <- "groundnut"
   d$trial_id <- "1"
   d$irrigated <- NA # post rainy season
   d$on_farm <- FALSE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$geo_from_source <- FALSE
   d$yield_part <- "pod"
   
   d$planting_date <- "2015"
   d$longitude  <- 9.3392
   d$latitude  <- 11.7594
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)    
}
