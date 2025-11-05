# R script for "carob"


carob_script <- function(path) {
   
" This dataset includes the research work carried out in evaluating marker assisted backcross (MABC) lines for rust and late leaf spot (LLS) resistance in five locations during 2015 rainy season. The evaluation were carried out at Aliyarnagar, Tamil Nadu; Directorate of Groundnut Research (DGR) Junagadh, Gujarat; Dharwad, Karnataka; ICRISAT, Patancheru and Kasbe Digraj, Maharashtra. A set of 41 ILs – 18 ILs in ICGV 91114 background, 14 ILs in TAG 24 background and 9 in JL 24 background along with recurrent parents – ICGV 91114, TAG 24, JL24; donor parent – GPBD 4 and three local checks were evaluated. Multi-location evaluation is useful to identify stable performing entries across locations to facilitate their release both at national and state level. Based on the data on disease score, yield and yield associated traits (Shelling percent and Hundred seed weight), 12 ILs were selected and used for conducting first ever near isogenic line (NIL) trial under All India Co-ordinated Research Project on Groundnut (AICRP-G). Subsequently three best performing ILs were identified and advanced to Advanced Varietal Trials (AVT-1 & AVT-2) under AICRP-G. The data presented here is the mean location wise performance of the entries for disease resistance, yield and associated traits that enabled selection and advancement of best performing ILs at Kasbe Digraj, Maharashtra, India location. "

# Keywords for understanding the data: PYH: Pod yield in Kg/Hectare SHP: Shelling percent (%) HSW: Hundred Seed Weight (g) LLS 90: Late Leaf Spot @ 90 DAS Rust: Rust @ 90 DAS IR: Irrigated RF: Rainfed KDG: Kasbe Dighraj, Maharashtra

   
   uri <- "doi:10.21421/D2/LC1VFT"
   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2, 
      data_organization = "ICRISAT", 
      publication =NA, 
      project = NA, 
      data_type = "experiment", 
      response_vars = "yield",
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-07-12"
   )
    	
   ff <- ff[grep(".xlsx", basename(ff))]
   
   process <- function(f){
      
      r <- carobiner::read.excel(f)
      d  <- data.frame(
         variety= r$Genotype,
         yield= r$PYH,
         seed_weight = as.numeric(r$HSW)* 10, ## 1000 seed weight,
         diseases = "late leaf spot",
         trial_id= gsub(".xlsx", "", basename(f)),
		 disease_severity = as.character(round(r$`LLS 90`, 2)),
		 shelling_percentage = r$SHP
      )
      d
      
   }
   
   d <- lapply(ff, process)
   d <- do.call(rbind,d)
   
   d$country <- "India"
   d$adm1 <- "Maharashtra"
   d$location <- "Kasbe Digraj"
   d$crop <- "groundnut"
   
   d$irrigated <- TRUE
   d$irrigated[grep("RF KDG", d$trial_id)] <- FALSE 
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "pod"
   
   d$planting_date <- "2015"
   d$longitude  <- 74.5125
   d$latitude  <-  16.9095
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d) 
   
}
