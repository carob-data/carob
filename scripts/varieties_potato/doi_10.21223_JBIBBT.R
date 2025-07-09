# R script for "carob"

carob_script <- function(path) {
   
"Experiments were installed in Huancavelica, with the objective of identifying clones with high potential for being varieties applying the Participatory Varietal Selection methodology. For the period 2016-2017, 18 clones with high resistance to late blight were planted, belonging to the B population and developed in the International Potato Center together with Two control varieties, Canchan and Yungay (susceptible). Finally, in the harvest 5 clones with high yield, low glycoalkaloid content and good organoleptic quality were selected as a result of the Participatory Variety Selection of the farmers and the analysis of mixed models and BLUPs for the yield data. The 5 selected clones were planted again in the period 2017-2018 and through the Participatory Varietal Selection, three promising clones were selected (CIP308488.92, CIP308495.227, and CIP308478.59)."

   uri <- "doi:10.21223/JBIBBT"
   group <- "varieties_potato"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
      data_organization = "CIP;YANAPAI",
      publication="doi:10.1007/s11540-021-09495-z",
      project=NA,
      data_type= "experiment",
      response_vars = "yield",
      treatment_vars = "variety_code",
      carob_contributor= "Cedric Ngakou",
      carob_date="2024-06-19"
   )
   
   ff <- ff[grep("exp", basename(ff))]
   
   process <- function(f) {
		r <- carobiner::read.excel(f, sheet="F4_harvest_mother")      
		d <- data.frame(
			 rep = as.integer(r$REP),
			 variety_code = r$INSTN,
			 yield = r$TTYNA* 1000, ## kg/ha
			 season = as.character(substr(basename(f), start = 8, stop = 11)),
			 trial_id = basename(f)
		)
		r <- carobiner::read.excel(f, sheet="Crop_management")      	  
		i <- r$Intervention_type == "Date_of_Harvest_(dd-mm-yyyy)"
		d$harvest_date <- as.character(as.Date(r$Date[i]))
		d
   }
   
   d <- lapply(ff, process) 
   d <- do.call(rbind, d)


    m <- carobiner::read.excel(ff[1], sheet="Minimal")
	r <- data.frame(rbind(m$Value))
	names(r) <- m$Factor
	
    d$country <- r$Country
    d$adm1 <- r$Admin1
    d$adm2 <- r$Admin2
    d$adm3 <- r$Admin3
    d$location <- "Cañaypata" # fixed spelling from r$Locality
    d$latitude <- as.numeric(r$Latitude)
    d$longitude <- as.numeric(r$Longitude)
    d$elevation <- as.numeric(r$Elevation)
	d$geo_from_source <- TRUE
	
    d$irrigated <- FALSE
    d$inoculated <- FALSE
    d$is_survey <- FALSE
    d$on_farm <- TRUE
    d$crop  <- "potato"
    d$yield_part  <- "tubers"
	
	d$trial_id <- as.factor(d$trial_id) |> as.integer() |> as.character()

   d$row_spacing <- 100  # From "DOI:10.1007/s11540-021-09495-z"
   d$plant_spacing <- 30 
   d$harvest_days <- 120

   d$planting_date <- "2017-11-25"
   d$planting_date[d$season=="2016"]<- "2016-11-07"
   d$season <- NULL
   
# ambiguous from methods in paper: the dose of fertilisation was 180-160-160 of NPK
	d$N_fertilizer <- 180
	d$P_fertilizer <- 160 / 2.29
	d$K_fertilizer <- 160 / 1.2051
   
	d$fungicide_used <- TRUE
	d$fungicide_product <- "mancozeb"
	carobiner::write_files(path, meta, d)   
}


