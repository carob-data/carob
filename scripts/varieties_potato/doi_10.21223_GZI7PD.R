# R script for "carob"

carob_script <- function(path) {
   
"Experiments were installed in La Libertad, with the objective of identifying clones with high potential for being varieties applying the Participatory Varietal Selection methodology. For the period 2016-2017, 18 clones with high resistance to late blight were planted, belonging to the B population and developed in the International Potato Center together with Two control varieties, Amarilis and Yungay (susceptible). Finally, in the harvest 5 clones with high yield, low glycoalkaloid content and good organoleptic quality were selected as a result of the Participatory Variety Selection of the farmers and the analysis of mixed models and BLUPs for the yield data. The 5 selected clones were planted again in the period 2017-2018 and through the Participatory Varietal Selection three promising clones were selected (CIP308488.92, CIP308495.227 and CIP308478.59)."

	uri <- "doi:10.21223/GZI7PD"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0, 
		publication="doi:10.1007/s11540-021-09495-z", 
		data_organization = "CIP;PATAZ", 
		carob_contributor="Cedric Ngakou", 
		data_type="experiment", 
		response_vars = "yield",
		treatment_vars = "variety", 
		project=NA, 
		carob_date="2023-12-09"
	)
   
   
	ff <- ff[grep("PTPVS", basename(ff))]

	process <- function(f) {
		r <- carobiner::read.excel(f, sheet="F4_harvest_mother")
		r <- r[, c("REP", "INSTN", "TTYNA")]
		colnames(r) <- c("rep", "variety", "yield")
		r$location <- basename(f)
		r
	}

   d <- lapply(ff, process) 
   d <- do.call(rbind, d)
   d$rep <- as.integer(d$rep)
   d$yield <- d$yield * 1000 ## kg/ha
   
 ## most of the information is coming from DOI:10.1007/s11540-021-09495-z
   i <- grepl("ARCOPAMPA", d$location)
   d$location[i] <- "Arcopampa"
   d$season[i] <- "2017-2018"
   
   i <- grepl("LSOLE", d$location)
   d$location[i] <- "La Soledad"
   d$season[i] <- "2016-2017"
   
   i <- grepl("MACULL", d$location)
   d$location[i] <- "Macullida"
   d$season[i] <- "2016-2017"
   
   i <- grepl("AURORITA", d$location)
   d$location[i] <- "La Aurorita"
   d$season[i] <- "2017-2018"
   
   d$row_spacing <- 100  
   d$plant_spacing <- 30 
   d$harvest_days <- 120
   
   d$country <- "Peru"
   d$adm1 <- "La Libertad"
   d$adm2 <- "Sanchez Carrion" # from DOI:10.1007/s11540-021-09495-z
   d$trial_id <- as.character(as.integer(as.factor(paste(d$location))))
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$is_survey <- FALSE
   d$on_farm <- TRUE
   d$crop <- "potato"
   d$yield_part <- "tubers" 
   ## add lon and lat
   geo <- data.frame(
		location=c("Arcopampa", "La Soledad", "Macullida", "La Aurorita"), 
		longitude=c(-77.89139, -77.87619, -77.883, -77.87444), 
		latitude=c(-7.976944, -7.923611, -7.913889, -7.829167),
		geo_from_source = FALSE
	)
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$planting_date <- "2016" 
   d$planting_date[d$season=="2017-2018"] <- "2017"
   d$harvest_date <- "2017" 
   d$harvest_date[d$season=="2017-2018"] <- "2018"
   d$season <- NULL

	
# ambiguous from methods in paper: the dose of fertilisation was 180-160-160 of NPK
	d$N_fertilizer <- 180
	d$P_fertilizer <- 160 / 2.29
	d$K_fertilizer <- 160 / 1.2051
	d$fungicide_used <- TRUE
	d$fungicide_product <- "mancozeb"
	
	carobiner::write_files(meta, d, path=path)
	

}


