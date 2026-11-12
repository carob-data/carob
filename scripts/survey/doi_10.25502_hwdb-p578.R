# R script for "carob"

carob_script <- function(path){

"N2Africa farm monitoring - Mozambique, 2012 - 2013
N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
 
	uri <- "doi:10.25502/hwdb-p578"
	group <- "survey"
	ff <- carobiner::get_data(uri,path,group)
	 
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
        project="N2Africa",
		publication = NA,
		carob_contributor = "Effie Ochieng'",
		carob_date="2022-09-07",
		carob_effort = NA,
		data_type = "survey",
		response_vars = "none",
		treatment_vars = "none",
		data_organization="IITA;ICRAF;WUR"
	)
	
	n2afun <- carobiner::get_function("N2A_monitoring_2", path, group)
	d <- n2afun(ff)

	d$adm2 <- carobiner::replace_values(d$adm2, 
			c("Maica","Tsangano-Fonte boa"),
	        c("Matica", "Tsangano"))

	d$adm3 <- carobiner::replace_values(d$adm3,
		c( "Agonia", "Calipo/Muhua","Calipo/Mucua", "Calipo/Marasse", "Calipo/Mirasse","Calopo/Marrasse","Calipo/ Marasse","Calipo/ Muhua","Calipo/ Mirasse","Calipo/Maleliha","Uerro","Marasse"),
        c("Angonia","Muhua","Mucua","Mirrasse","Mirrasse","Mirrasse","Mirrasse","Muhua","Mirrasse","Calipo","Uorra","Mirrasse"))

	d$location <- carobiner::replace_values(d$location, 
		c("Siwama", "Gurue-UP2","Murimo","Lioma-Namiepe","Lioma-Nintulo","Madiea","Niza","Mpupha","Namurekele","Mulosa","Namphi","Nahaco","Pwasiua","Mugunuwa","Jordan", "Vaiya","Nvine","Ruace-pissi","Nakuilo","Jordao","Mwetxo", "Mpupwa","Mohiyera","Viola 2"),
		c("Manica","Gurue","Murrimo","Namiepe","Nintulo","Madea","Zembe","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Pissi","Gurue","Gurue","Gurue","Gurue","Gurue","Viola"))
	
	# the given latitudes were not negative
	d$latitude <- -(d$latitude)
	# all longitudes were the same as the latitude
	d$longitude <- NA
	
 # creating a data frame with the unique entries and the latitudes and longitudes, coordinates hard coded
	geo <- data.frame(
		location = c("Barue", "Domue", "Ewarelo", "Gurue", "Lioma", "Macanga", "Magige", "Manica", "Mossurize", "Murrimo", "Mussacumbira", "Mutequelesse", "Nametil", "Nintulo", "Pissi", "Ruace", "Serra", "Tetete", "Tsangano", "Ulongue", "Zembe"), 
	
		lat = c(-17.5, -14.47, -15.16, -15.46, -15.17, -14.68, -15.32, -19, -21.21, -15.37, -13.43, -19, -14.21, -15.09, -14.9, -14.91, -16.47, -15.38, -15.16, -14.72, -19.29), 

		lon = c(33.65, 34.2, 36.94, 36.98, 36.8, 32.73, 36.7, 33.5, 33.38, 36.8, 38.6, 33.5, 40.55, 37.1, 36.6, 36.35, 35.7, 36.5, 34.5, 34.36, 33.35),
		geo_from_source = FALSE
	)	
	
	d <- merge(d, geo, by="location", all.x=TRUE)
 
	nna <- !is.na(d$lat)
	d$longitude[nna] <- d$lon[nna]
	d$latitude[nna] <- d$lat[nna]
	d$lat <- d$lon <- NULL
    
	i <- which(d$variety %in% c("Y", "N") & d$inoculated == "")
	d$inoculated[i] <- d$variety[i]

	d$inoculated[d$inoculated == ""] <- NA
	d$inoculated <- d$inoculated == "Y"
	
	i <- d$crop=="100"
	d$crop[i] <- tolower(d$variety[i])
	d$variety[i] <- NA
	i <- which(d$variety == "Mamane")
	d$crop[i] <- "groundnut"
	d$yield_part[i] <- "pod"
			
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$on_farm <- TRUE

	d$planting_date[is.na(d$planting_date)] <- "2012"
	d$harvest_date[is.na(d$harvest_date)] <- "2013"
	d$season <- NULL

	d$yield_moisture <- NA_real_ 
	d$yield_isfresh <- TRUE
	
	#missing
	#village found in GeoNames 
	vlon <- c(
	  "Chia" = 34.17028,        # Aldeia Chyia; https://www.geonames.org/1049974
	  "Chindeque" = 34.33389,   # https://www.geonames.org/1049176
	  "Chingola" = 34.04583,    # Aldeia Chingola; https://www.geonames.org/1087553
	  "Dala" = 34.0475,         # Aldeia Dala; https://www.geonames.org/1087554
	  "Lifidzi" = 34.23639,     # Lifidzi mission; https://www.geonames.org/1089009
	  "Massoko" = 34.22778,     # Massoco; https://www.geonames.org/1040177
	  "Mpenha" = 34.43306,      # Aldeia Mepenha; https://www.geonames.org/1036952
	  "Wilson" = 34.34556,      # https://www.geonames.org/1024559
	  "Mangani" = 34.32361,     # Mangani (survey point); https://www.geonames.org/1089012
	  "Maula" = 39.28806        # Muaula, Mogovolas; https://www.geonames.org/1039672
	)
	i <- is.na(d$longitude) & d$location %in% names(vlon)
	d$longitude[i] <- vlon[d$location[i]]
	
	#village not found: longitude of the district (adm2)
	dlon <- c(
	  "Angonia" = 34.367,       # Angonia District; https://www.geonames.org/11204389
	  "Tsangano" = 34.32685,    # Tsangano District; https://www.geonames.org/7874106
	  "Gurue" = 36.98167,       # Gurue District (Namiepe); https://www.geonames.org/7873991
	  "Nampula" = 39.2666,      # Nampula (Muriaze); https://www.geonames.org/1033356
	  "Zembe" = 33.34222        # Zembe, Manica (Repungue); https://www.geonames.org/1096285
	)
	i <- is.na(d$longitude) & d$adm2 %in% names(dlon)
	d$longitude[i] <- dlon[d$adm2[i]]
	d$geo_uncertainty[i] <- 25000    #longitude from the district, latitude from the source (0.1 deg)
			
	# all scripts should end like this
	carobiner::write_files(meta, d, path=path)

}


