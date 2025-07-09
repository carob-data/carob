# R script for "carob"

carob_script <- function(path) {
   
"The International Potato Center has developed a new population: LBHTxLTVR, in order to exploit heterosis, crossing elite clones of two previously developed populations, one for resistance to late blight, heat tolerance and adaptation to mid elevation and Highland tropics, with germplasm mainly Solanum tuberosum spp. Andigena, named LBHT (late blight resistance and heat tolerance) population and the other population for resistance to virus, tolerance to heat, earliness and adaptation to lowland tropics using germplasm mostly Solanum tuberosum spp tuberosum, named LTVR (lowland tropics and virus resistance) population . Twenty-eight advanced clones of this population with high levels of resistance to late blight and tuber yield were studied to determine their phenotypic stability for tuber yield in the 2019-2020 season. Four experiments in contrasting environments, Huancayo, San Ramon, Huamachuco and Huanuco in Peru .
We use the randomized complete blocks (RCB) statistical design, with three repetitions of 10 plants each. The NPK dose of 200-180 -160 was used. No fungicide was applied to control the late blight."

	uri <- "doi:10.21223/WX2HIT"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		publication= NA,
		data_organization = "CIP;PATAZ",
		carob_contributor="Cedric Ngakou",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project=NA,
		carob_date="2023-12-12"
	)
   
   
	ff <- ff[grep("PTYield", basename(ff))]

	process <- function(f) {
		r <- carobiner::read.excel(f, sheet="Fieldbook")
		r <- r[, c("REP", "INSTN", "TTYNA")]
		colnames(r) <- c("rep", "variety", "yield")
		m <- carobiner::read.excel(f, sheet="Minimal")
		n <- as.list(m$Value)
		names(n) <- m$Factor
		r$adm1 <- n$Admin1
		r$adm2 <- n$Admin2
		r$adm3 <- n$Admin3
		r$location <- n$Locality
		r$latitude <- as.numeric(n$Latitude)  
		r$longitude <- as.numeric(n$Longitude) 
		r$planting_date <- n$Begin_date
		r$harvest_date <- n$End_date
		r$harvest_date <- gsub("23/1/20","23/11/2020",r$harvest_date) 
		r$planting_date <- gsub("14/8/20","14/8/2020" ,r$planting_date)
		r
	}
   
	d <- lapply(ff, process) 
	d <- do.call(rbind, d)
	d$rep <- as.integer(d$rep)
	d$yield <- d$yield * 1000 ## kg/ha
	## add columns
	
	d$country <- "Peru"
	d$trial_id <- paste(d$adm3, d$variety, sep = "_")
	d$irrigated <- FALSE
	d$inoculated <- FALSE
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$crop <- "potato"
	d$yield_part <- "tubers" 
	d$geo_from_source <- TRUE
	## NPK dose used (200-180 -160)
	d$N_fertilizer <- 200
	d$P_fertilizer <- 180/2.29
	d$K_fertilizer <- 160/1.2051
	
	# fix date format and date type
	i <- d$planting_date %in% c("43473", "43746")
	d$planting_date[!i] <- as.character(as.Date(d$planting_date[!i], format = "%d/%m/%Y"))
	d$planting_date[i] <- as.character(as.Date(as.numeric(d$planting_date[i]), origin = "1899-12-31"))

	i <- d$harvest_date == "43596"
	d$harvest_date[!i] <- as.character(as.Date(d$harvest_date[!i], format = "%d/%m/%Y"))
	d$harvest_date[i] <- as.character(as.Date(as.numeric(d$harvest_date[i]), origin = "1899-12-31"))


	d$location[d$location == "HUMACHUCO"] <- "Humachuco"
	d$location[d$location == "EXP"] <- NA
	d$adm1[d$adm1 == "HUANUCO"] <- "Huánuco"
	d$adm2[d$adm2 == "HUANUCO"] <- "Huánuco"
	d$adm3[d$adm3 == "HUANUCO"] <- "Huánuco"

	carobiner::write_files(meta, d, path=path)	
}


