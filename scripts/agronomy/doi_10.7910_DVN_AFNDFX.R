# R script for "carob"



carob_script <- function(path) {
   
"Traits related with drought resistance in common beans. A set of 16 bean genotypes belonging to the Middle American gene pool were evaluated under field conditions with two levels of water supply (irrigated and drought) over two seasons. Lines SEN 56, BFS 29, NCB 226 and SER 16 showed high grain yield in both irrigated and terminal drought conditions, while RCB 593 and G 40001 showed high grain yield in drought. Drought resistant lines produced at least 15% and 50% more grain yield than EAP 9510-77, commercial check for Central America, in irrigated and drought condition, respectively. In addition, they were also superior to other commercial checks such as DOR 390 and Bribri."
   
   uri <- "doi:10.7910/DVN/AFNDFX"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=4, 
      data_organization = "UCRC;CIAT;UNAL", 
      publication = "doi:10.15517/ma.v29i1.27618",
      project =NA, 
      data_type = "experiment",
      response_vars = "yield;fwy_total",
      treatment_vars = "variety;irrigated", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-10-13"
   )
   
   f <- ff[basename(ff)=="02. Datos _Chaves.csv"]
  
   ## processing data
   r <- read.csv(f, fileEncoding='latin1',check.names=F, na=c("."))
   names(r) <- gsub("100SW", "Sw100", names(r))
   irrigated <- grepl("Riego", r$Ambiente)
   
   d <- data.frame(
     country= "Colombia",
     location= "Palmira",
     crop= "common bean",
     irrigated= irrigated,
     planting_date= ifelse(irrigated, 
						ifelse(r$Año == 2012, "2012-08-10", "2013-07-18"),
						ifelse(r$Año == 2012, "2012-08-03", "2013-07-15")),
     treatment=  ifelse(irrigated, "irrigated", "drought"),
     rep= r$Rep,
     variety= r$Genotipo,
     yield= r$YDHA,
     seed_weight= r$Sw100*10,
     flowering_days= r$DF,
	# these numbers do not seem right
	# harvest_index= r$HI,
     fwy_total= as.numeric(r$CB),
     LAI= r$LAI,
     # that seems to be the number of seeds harvested
	 #seed_density= r$SNA*10000,
     trial_id= paste0(r$Año, "_", r$Ambiente)
     
   )
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "seed"
   d$latitude <- 3.53782
   d$longitude <- -76.2968
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
#   d$harvest_index[d$harvest_index > 100] <- NA
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)
}


