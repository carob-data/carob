# R script for "carob"
# license: GPL (>=3


carob_script <- function(path) {
   
"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under post rainy season in Buk

Medium-Duration Multi-location trial conducted under Post-season .The trial was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses across the location which includes, Buk
"

  uri <- "doi:10.21421/D2/LIY0MP"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)

  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
              data_organization = "ICRISAT",                                  
              publication = NA,
              project = NA,
              data_type = "experiment",
              treatment_vars = "variety", 
              response_vars = "dmy_storage; dmy_residue",  
              completion = 100,
              carob_contributor = "Gacheri Nturibi",
              carob_date = "2025-08-07",
              notes = NA,
              design = NA
  )
  
  
  r <- carobiner::read.excel(ff[basename(ff) == "Data file of medium duration multi-location trial post rainy in Buk.xlsx"])
  
  d <- data.frame(
    rep = as.integer(r$`Replication number`),
    plot_id = as.character(r$`Plot no`),
    variety = r$Variety,
    emergence_days = r$DEM,
    flowering_days = r$DFF,
    dmy_storage = r$DPWkgha,
    dmy_residue = r$DFWkgha,
    seed_weight = as.numeric(r$HSW)*10, # From 100 to 1000 Seed Weight
    shelling_percentage = r$`Shelling ratio`,
    maturity_days = r$DM
  )
  
  d$planting_date <- "2015-06-14" # from dataverse metadata 
  
  d$trial_id <- "1"
  d$crop <- "groundnut"
  d$yield_part <- "pod"
  d$country <- "Nigeria"
  d$adm1 <- "Kano"
  
  d$location <- "Kano"
  d$site <- "Bayero University"

  d$longitude <- 8.4752
  d$latitude <- 11.9840
  d$geo_from_source <- FALSE
  
  d$on_farm <- NA
  d$is_survey <- FALSE
  d$irrigated <- NA
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
    
	
	d$record_id <- 1:nrow(d)

	x <- reshape(r[, c("ELS", "LLS", "RUST")], direction="long", varying=c("ELS", "LLS", "RUST"), v.names="disease_severity", timevar="diseases")
	x$diseases <- c("early leaf spot", "late leaf spot", "rust")[x$diseases]
	colnames(x)[3] <- "record_id"
	x$severity_scale <- "1-9"
	x$disease_severity <- as.character(x$disease_severity)
		
  carobiner::write_files(path, meta, d, long=x)
}
