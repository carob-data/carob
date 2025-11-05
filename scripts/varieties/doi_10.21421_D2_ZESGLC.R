# R script for "carob"

carob_script <- function(path) {
      
" Short Duration Multi-location Trial was carried out to confirm their yield ( pod and fodder) superiority and to also evaluate their resistance and tolerance to biotic stresses. Forty five (45) groundnut lines were used, trial conducted in location: Bayero University research Farm "
   
   uri <- "doi:10.21421/D2/ZESGLC"

   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization = "ICRISAT", 
      publication =NA, 
      project = NA, 
      data_type = "experiment", 
      response_vars = "yield",
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-07-10"
   )
   
   f <- ff[basename(ff) == "Data file of short duration lines for agronomic traits in Minjibir 2014.xlsx"] 	
   r <- carobiner::read.excel(f, sheet = "Short duration lines",na=c("NA", NA))
   
   d  <- data.frame(
      rep= as.integer(r$`Replication number`),
      variety= r$Genotypes,
      emergence_days= r$DEM,
      flowering_days= r$DFF,
      maturity_days= r$DM,
      dmy_storage= r$DPWkgha,
      dmy_residue= r$DFWkgha,
      seed_weight = as.numeric(r$`SW_ M_g100sd`)* 10, ## 1000 seed weight,
      diseases = "early leaf spot;late leaf spot;rust",
      disease_severity=	apply(r[, c("ELS_E_1to9", "LLS_E_1to9", "Rust_E_1to9")], 1, \(i) paste0(i, "(1-9)", collapse=";")),
      LAI= r$LAI70,  ### Leaf index at 70 days 
      shelling_percentage = r$SH_Calc_pct
   )
   
   # ### Adding agronomic traits
   # ##CN: I Commented this out but it might be interesting 
   # r1 <- carobiner::read.excel(f,sheet = "Agronomic traits")
   # d$EN <- r$`Entry number`
   # d1 <- data.frame(
   #    rep= r1$`Replication number`,
   #    pod_width= r1$`Pod width`,
   #    leaf_lenght= r1$`Leaf length`,
   #    leaf_width= r1$`Leaf width`,
   #    mature_plant= r1$`Mature plant`,
   #    Immature_plant= r1$`Immature plant`,
   #    leaf_shape= r1$`Leaf shape`,
   #    EN= r1$`Entry number`
   #    
   # )
   # d <- merge(d,d1, by=c("rep","EN"),all.x = TRUE)
   # d$EN <- NULL
   
   d$country= "Nigeria"
   d$location= "Minjibir"
   d$crop= "groundnut"
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- NA 
   d$inoculated <- FALSE
   d$yield_part <- "pod"
   d$trial_id <- "1"
   d$planting_date <- "2014"
   d$longitude  <- 8.6142
   d$latitude  <-  12.1936
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d) 
   
}
