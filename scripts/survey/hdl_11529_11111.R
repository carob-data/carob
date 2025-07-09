# R script for "carob"

carob_script <- function(path) {

"Rice crop cuts conducted in Odisha (Mayurbhanj, Balasore, Keonjhar, Bhadrak, Khorda, Puri districts)"
	
	uri <- "hdl:11529/11111"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		project=NA,
		publication= NA,
		data_organization = "CIMMYT;IRRI",
		data_type="crop cuts", 
		carob_contributor="Effie Ochieng'",
		carob_date="2023-09-25",
		response_vars = "none",
		treatment_vars = "none"
	)
	
	f <- ff[basename(ff) == "CSISA_OD_RiceCropCut_AllYearRawDataFinal.csv"]
	r <- read.csv(f)

	d <- data.frame(
		country = "India",
		crop = "rice",
		yield_part = "grain",	
		on_farm = TRUE,
		is_survey = TRUE,
		adm1="Odisha",
		adm2=r$District_Name,
		season=r$Season, 
		latitude =r$Latitude,
		longitude=r$Longitude,
		variety=r$VAR, 
		planting_date = r$DOS,
		transplanting_date = r$DOT,
		harvest_date = r$DOH,
		planting_method = r$CEM,
		trial_id = r$FID
	)


#		herbicide_product
#r$Pre_Herbicide_Name
#r$Post_Herbicide_Name

	dap <- r$DAP_Total
	mop <- apply(r[, grep("MoP_", names(r))], 1, sum)
	urea <- apply(r[, grep("Urea_", names(r))], 1, sum)
	ft <- cbind(dap>0, mop>0, urea>0)
	ft[is.na(ft)] <- FALSE
	ft <- apply(ft, 1, \(i) paste(c("DAP", "KCl", "urea")[i], collapse="; "))
	ft[ft == ""] <- "none"
	ft[is.na(dap)] <- NA
	
 # 2.4711 to convert from kg/acre to kg/ha 
	d$P_fertilizer <- dap * 2.4711 * .201
	d$K_fertilizer <- mop * 2.4711 * .498
	d$N_fertilizer <- dap * 2.4711 * 0.180 + urea * 2.4711 * 0.46

	biom <- r[, c("TAGB_A", "TAGB_B", "TAGB_C")]
	biom[biom=="-"] <- NA
	biom <- sapply(biom, as.numeric)
	biom <- rowMeans(biom, na.rm=TRUE)

	yld <- r[, c("Yield_A", "Yield_B", "Yield_C")]
	yld[yld=="-"] <- NA
	yld <- sapply(yld, as.numeric)
	yld <- rowMeans(yld, na.rm=TRUE)
	
	# efyrouwa : from dataset description
	#Grain weight 
	#Year 2013 - 5X5 sq meter in Kg 
	#Year 2014 - 2X2.5 sq meter in Kg 
	#Year 2015 - 1X1 sq meter in gm 
	#Year 2016 - 2X2 sq meter in gm
	# to calculate in kg/ha
	d$yield <- ifelse(r$Year == 2013, yld * 400,
			 ifelse(r$Year == 2014, yld * 2000,
			 ifelse(r$Year == 2015, yld * 10,  
			 yld * 2.5))) 
	
	
	#Total above ground weight 
	#Year 2013 - 5X5 sq meter in Kg Year 
	#2014 - 2X2.5 sq meter in Kg 
	#Year 2015 - 1X1 sq meter in gm 
	#Year 2016 - 2X2 sq meter in Kg
	# to calculate in kg/ha
	d$dmy_total <- ifelse(r$Year == 2013, biom * 400,
					 ifelse(r$Year == 2014, biom * 2000,
					 ifelse(r$Year == 2015, biom * 10, 
					 biom * 2500)))
	
	# RH: all the values above 10000 are really suspect relative to grain yield
	d$dmy_total[d$dmy_total > 50000] <- NA
	
	d$planting_date <- as.character(as.Date(d$planting_date, format = "%d-%b-%y")) 
	i <- is.na(d$planting_date)
	d$planting_date[i] <- as.character(r$Year[i])
	
	d$harvest_date <- as.character(as.Date(d$harvest_date, format = "%d-%b-%y")) 
	d$transplanting_date <- as.character(as.Date(d$transplanting_date, format = "%d-%b-%y")) 

	

# merging last because the order changes
	x <- data.frame(adm2 = c("Bhadrak", "Mayurbhanj", "Puri"), 
					lon = c(86.498, 86.3957, 85.8391), 
					lat = c(21.0666, 21.9156, 19.8068))

	d$latitude[d$latitude == "-"] <- NA
	d$longitude <- as.numeric(d$longitude)
	d$latitude <- as.numeric(d$latitude)
	i <- which(d$longitude < 80)
	d$longitude[i] <- d$latitude[i] <- NA

	d <- merge(d, x, by="adm2")
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$lon <- d$lat <- NULL
	i = which(d$latitude > 50)
	tmp <- d$latitude[i]
	d$latitude[i] <- d$longitude[i]
	d$longitude[i] <- tmp

	d$irrigated <- NA

	carobiner::write_files(meta, d, path=path)
}


