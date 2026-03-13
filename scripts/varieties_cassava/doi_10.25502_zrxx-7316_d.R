# R script for "carob"


carob_script <- function(path) {
   
"Agronomic performance of two cassava varieties (MM96/0287 and MM96/0735) introduced within PRODEMA project sites in Burundi"
   
   uri <- "doi:10.25502/zrxx-7316/d"
   group <- "varieties_cassava"
   ff  <- carobiner::get_data(uri, path, group)
   
    meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA, 
      data_organization ="IITA", 
      publication= NA, 
      project= "PRODEMA", 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-11-08"
   )
   
   ff <- ff[grepl("-", basename(ff))]
   ff <- ff[!grepl("data-dictionary.csv", basename(ff))]
   
   ## processing  data
   process <- function(f){
      r <- read.csv(f, fileEncoding="latin1", na=c(""))
      d <- data.frame(
         location= r$Localité,
         variety= r$Variétés,
         yield_isfresh = as.logical(TRUE),
         yield_moisture = as.numeric(65), # Typical cassava fresh harvest moisture
         yield= r$Rdt_T_ha *1000,
         planting_date= as.character(as.Date(r$DP, "%m/%d/%Y")),
         harvest_date= as.character(as.Date(r$DR, "%m/%d/%Y")),
         country= "Burundi",
         crop= "cassava",
         trial_id= paste0(r$Blocs, "-", gsub(".csv", "", basename(f)))
      )
   }
   
   d <- lapply(ff, process)
   d <- do.call(rbind, d)
   d <- d[!is.na(d$yield)&!is.na(d$variety),]
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "tubers"
   d$location <- gsub("- S/C ", "-", d$location)
   geo <- data.frame(
      location=c("Butare-Kayogoro", "Cendajuru", "Ngozi-Gitasi", "Ngozi-Kiremba", "Mishiha-Kaniha", "Murore-Bahizi", "Busoni-Mubira", "Mutukura", "Minago"),
      latitude= c(-2.95960, -3.152217, -2.86154617, -2.784120770, -3.07349, -2.53674, -2.5513, -2.9921, -3.80703),
      longitude= c(29.83188, 30.600657, 29.8031288, 29.9795020, 30.69783, 30.23947, 30.24972, 30.8079, 29.34851)
   )
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files(path, meta, d)
}

