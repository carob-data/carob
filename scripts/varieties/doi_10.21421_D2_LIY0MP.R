# R script for "carob"
# license: GPL (>=3


carob_script <- function(path) {
  
  "Evaluation of medium duration lines for agronomic traits and Foliar diseases under post rainy season in Buk"
  
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
              notes = "All variables denoted as dates, (DEM, DFF and DM) in the metadata were treated as days",
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
    maturity_days = r$DM,
    diseases = "early leaf spot;late leaf spot; rust",
    disease_severity = apply(r[, c("ELS", "LLS", "RUST")], 1, \(i) paste0(i, collapse=";")),
    severity_scale = "1-9"
  )
  
  cc=  paste(r$RUST, r$LLS,  r$ELS, sep = ";")
  d$trial_id <- "1"
  d$crop <- "groundnut"
  d$yield_part <- "pod"
  d$country <- "Nigeria"
  d$adm1 <- "Kano"
  
  d$location <- "BUK (Bayero University, Kano)" 
  d$longitude <- mean(c(8.474923, 8.475408))
  d$latitude <- mean(c(11.983994, 11.983964))
  d$geo_from_source <- TRUE
  
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- NA
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  d$planting_date <- "2015"
  
  carobiner::write_files(path, meta, d)
}
