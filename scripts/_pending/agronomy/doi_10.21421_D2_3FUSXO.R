# R script for "carob"

#NOTE: Treatment not included in script

carob_script <- function(path) {
   
"The objective of this work conceived an idea of using Polythene mulch on broad bed furrows to improve groundnut production and productivity during the low temperature months in Sudan savanna of Nigeria. The experiments were conducted in 2 seasons (2014 and 2015 dry seasons). The treatments used in the experiment consisted of Polythene mulch (Mulch and Without mulch) and Groundnut varieties ( Samnut 23, Samnut 24, Samnut 26 and Ex-Dakar) laid out in Split plot design with 4 replications " 
   
   uri <- "doi:10.21421/D2/3FUSXO"
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
      carob_effort = NA,
      carob_date = "2024-08-14",
      notes="Germination and Spad Chlorophyll Meter Reading variables are not yet processed"
   )
   
   f <- ff[basename(ff)=="Data file of Irrigated groundnut to polythene mulching on broad-bed and furrows .xlsx"]
   r <- carobiner::read.excel(f, fix_names=TRUE)
   
   d <- data.frame(
      rep= as.integer(r$Replication.number),
      variety= r$Variety,
      treatment= r$Mulch,
      yield = r$Pod.Yield,
      fwy_residue=  r$Haulm.Yield,
      flowering_days= r$DFF,
      maturity_days= r$DM,
      shelling_percentage= r$Shelling.ratio,
      seed_weight = as.numeric(r$HSW)* 10, ## 1000 seed weight
      planting_date= as.character(r$Year),
      LAI= r$LAI_80.DAS
   )
   
   d$country <- "Nigeria"
   d$adm1 <- "Kano"
   d$crop <- "groundnut"
   d$trial_id <- "1"
   d$irrigated <- NA 
   d$on_farm <- FALSE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$geo_from_source <- FALSE
   d$yield_part <- "pod"
   
   d$longitude  <- 8.51672
   d$latitude  <- 12.00012 
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   
   carobiner::write_files (path, meta, d)    
}
