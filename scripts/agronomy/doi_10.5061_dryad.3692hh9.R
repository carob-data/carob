# R script for "carob" 	

carob_script <- function(path) {
   
"Wheat (Triticum æstivum L.) is an important East Africa highland crop but yields are low. Information is scarce for optimization of fertilizer use. Research was conducted to determine yield response functions for N, P and K, and to diagnose Mg–S–Zn–B deficiencies. The average grain yield increase in Rwanda due to N application was 1.5 Mg ha−1 with a mean economically optimal rate (EOR) of 68 kg ha−1 N. In Kenya and Tanzania, yield was increased by 29% with EOR N for two SY but unaffected by N rate for four other SY which on average had 50% of the soil organic C (SOC) as the N-responsive SY. Yield was increased, on average, with application of P and K by 0.47 and 0.23 Mg ha−1, respectively, at EOR in Rwanda but effects were inconsistent for other SY where soil test K was higher than in Rwanda. Application of Mg–S–Zn–B resulted in 0.46 Mg ha−1 more yield in Rwanda but did not affect yield at other SY where the average soil test values for these nutrients was 35% higher than in Rwanda. If the financially constrained farmer opts to apply the affordable fertilizer to twice as much land at 50% EOR compared with 100% EOR, the mean yield increase is reduced by 27% but production and PCR are increased by 43 and 72%, respectively. Nutrient effects were relatively consistent and positive in Rwanda, but less and less inconsistent elsewhere with generally less SOC, more K–Mg–S–Zn–B availability, and often lower yields."
 
 
	uri <- "doi:10.5061/dryad.3692hh9"
	group <- "agronomy" 
   
	ff  <- carobiner::get_data(uri, path, group)
     
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA, 
		data_organization = "RAB;IITA;TARI;KALRO;ICRAF;NARL;UNL", 
		publication = "doi:10.1007/s10705-018-9912-z", # potentially also: "doi:10.1017/S0014479722000096"
		project = NA, 
		data_type = "experiment", 
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;Mg_fertilizer;Zn_fertilizer;B_fertilizer", 
		carob_contributor = "Robert Hijmans", 
		carob_date = "2024-06-29"
	)
   
	f <- ff[basename(ff)=="EA wheat database.xlsx"]
   
	r1 <- carobiner::read.excel(f, sheet = "Rwanda")
	r2 <- carobiner::read.excel(f, sheet = "Kenya", fix_name=TRUE)
	r3 <- carobiner::read.excel(f, sheet = "Tanzania")

	r0 <- carobiner::read.excel(f, sheet = "Readme", skip=29, fix=TRUE)
	i <- which(is.na(r0[,1]))
	geo <- r0[1:(i[1]-1) , ]
	soil <- r0[(i[3]-1):(nrow(r0)-1), ]
	names(soil) <- soil[1,]
	soil <- soil[-(1:2), ]
	names(soil) <- 	c("Site_year", "soil_texture", "pH", "SOC", "soil_P", "soil_K", "soil_Ca", "soil_Mg", "soil_S", "soil_Zn", "soil_B")
	i <- soil$Site_year == "RW_Rusarabuye14b,15b"
	soil$Site_year[i] = "RW_Rusarabuye14b"
	soil <- rbind(soil, soil[i, ])
	soil$Site_year[nrow(soil)] = "RW_Rusarabuye15b"
	i <- soil$Site_year == "RW_Tare14a,15a,b"
	soil$Site_year[i] = "RW_Tare14a"
	soil <- rbind(soil, soil[i, ])
	soil$Site_year[nrow(soil)] = "RW_Tare15a"
	soil <- rbind(soil, soil[i, ])
	soil$Site_year[nrow(soil)] = "RW_Tare15b"
	geosoil <- merge(geo, soil, by=1)
	
	
	d1 <- data.frame(
		country= "Rwanda",
		trial_id= r1$SiteYr,
		planting_year = gsub("b", "",  r1$y),
		treatment= r1$Treatment,
		N_fertilizer = r1$N,
		P_fertilizer = r1$P,
		K_fertilizer = r1$K,
		S_fertilizer = r1$S,
		Mg_fertilizer = r1$Mg,
		Zn_fertilizer = r1$Zn,
		B_fertilizer = r1$B,
		yield = r1$GY * 1000,
		rep = r1$r
	)
	d1$planting_year[grep("15", d1$trial_id)] <- "2015"
	
	d2 <- data.frame(
		country= "Kenya",
		trial_id= r2$Tt,
		treatment= r2$Trtcode ,
		N_fertilizer = r2$N,
		P_fertilizer = r2$P,
		K_fertilizer = r2$K,
		yield = r2$GY * 1000,
		rep = r2$R
	)
	d2$planting_date <- "2014"
	d2$planting_date[grep("15", d2$trial_id)] <- "2015"

	d3 <- data.frame(
		country= "Tanzania",
		trial_id= paste0(r3$S, r3$Y),
		location = r3$S,
		planting_year = r3$Y,
		N_fertilizer = r3$N,
		P_fertilizer = r3$P,
		K_fertilizer = r3$K,
		yield = r3$`GraYldHa (kg)`,
		fwy_residue = r3$`StoYldHa (kg)`,
		rep = r3$Rep
	)
	
	d <- carobiner::bindr(d1, d2, d3)

	d$rep <- as.integer(d$rep)
	d$on_farm= NA
    d$irrigated= NA
    d$is_survey= FALSE
	d$crop <- "wheat"
	d$yield_part <- "seed"
	
	# EGB:
	# # As per publication (Table 3)
	i <- which(d$treatment == "Diagnostic"| d$treatment == "Diagnostic package" | d$N_fertilizer  == "Diagnostic package")
	
	d$N_fertilizer[i] <- 90
	d$P_fertilizer[i] <- 15
	d$K_fertilizer[i] <- 20
	d$Mg_fertilizer[i] <- 10
	d$S_fertilizer[i] <- 15
	d$Zn_fertilizer[i] <- 2.5
	d$B_fertilizer[i] <- 0.5
	
	d$N_fertilizer <- as.numeric(d$N_fertilizer)
	d$K_fertilizer <- as.numeric(d$K_fertilizer)
	
		
	# EGB:
	# # Georeferencing from publication
	d$geo_from_source <- FALSE
	d$longitude <- NA
	d$longitude[d$trial_id == "Njoro14"] <- 35.946
	d$longitude[d$trial_id == "Eldoret14"] <- 35.708 
	d$longitude[d$trial_id == "Selian15"] <- 36.628
	d$longitude[d$trial_id == "NBURGah15b"] <- 29.675
	d$longitude[d$trial_id %in% c("SNyamAka14b", "SNyaTare14b", "SNyaTar2015A", "SNYATar15b")] <- 29.5
	d$longitude[d$trial_id %in% c("NBureRus14b", "NBurRusa14b", "NBURRus15b")] <- 29.842
	d$longitude[d$trial_id %in% c("NBurButa14b", "NBurBut2015A")] <- 29.85
	d$longitude[d$trial_id %in% c("NMusaBus14b", "NMusGat2015A")] <- 29.569
	d$longitude[d$trial_id %in% c("Selian2015", "Selian2016")] <- 36.628
	d$longitude[d$trial_id %in% c("Uyole2015", "Uyole2016")] <- 33.514
	d$latitude <- NA
	d$latitude[d$trial_id == "Njoro14"] <- -0.344
	d$latitude[d$trial_id == "Eldoret14"] <- 0.569
	d$latitude[d$trial_id == "Selian15"] <- -3.357
	d$latitude[d$trial_id == "NBURGah15b"] <- -1.443
	d$latitude[d$trial_id %in% c("SNyamAka14b", "SNyaTare14b", "SNyaTar2015A", "SNYATar15b")] <- -2.511
	d$latitude[d$trial_id %in% c("NBureRus14b", "NBurRusa14b", "NBURRus15b")] <- -1.507
	d$latitude[d$trial_id %in% c("NBurButa14b", "NBurBut2015A")] <- -1.379
	d$latitude[d$trial_id %in% c("NMusaBus14b", "NMusGat2015A")] <- -1.549
	d$latitude[d$trial_id %in% c("Selian2015", "Selian2016")] <- -3.357
	d$latitude[d$trial_id %in% c("Uyole2015", "Uyole2016")] <- -8.916
	
	d$planting_date <- NA
	d$planting_date[d$trial_id == "Njoro14"] <- "2014-05"
	d$planting_date[d$trial_id == "Eldoret14"] <- "2014-06-19"
	d$planting_date[d$trial_id == "Selian15"] <- "2015-04-06"
	d$planting_date[d$trial_id == "NBURGah15b"] <- "2015-03-16"
	d$planting_date[d$trial_id == "SNYATar15b"] <- "2015-10-18"
	d$planting_date[d$trial_id == "SNyaTar2015A"] <- "2015-03-28"
	d$planting_date[d$trial_id == "NBureRus14b"] <- "2014-04-22"
	d$planting_date[d$trial_id == "NBurRusa14b"] <- "2014-04-22"
	d$planting_date[d$trial_id == "NBURRus15b"] <- "2015-04-27"
	d$planting_date[d$trial_id == "NBurBut2015A"] <- "2015-10-13"
	
	d$harvest_date <- NA
	d$harvest_date[d$trial_id == "Njoro14"] <- "2014-08"
	d$harvest_date[d$trial_id == "Eldoret14"] <- "2014-10-28"
	d$harvest_date[d$trial_id == "Selian15"] <- "2015-08-22"
	d$harvest_date[d$trial_id == "Selian2015"] <- "2015-08-22"
	d$harvest_date[d$trial_id == "NBURGah15b"] <- "2015-08-16"
	d$harvest_date[d$trial_id == "SNYATar15b"] <- "2016-02-27"
	d$harvest_date[d$trial_id == "SNyaTar2015A"] <- "2015-08-10"
	d$harvest_date[d$trial_id == "NBureRus14b"] <- "2014-08-23"
	d$harvest_date[d$trial_id == "NBurRusa14b"] <- "2014-08-23"
	d$harvest_date[d$trial_id == "NBURRus15b"] <- "2015-08-18"
	d$harvest_date[d$trial_id == "NBurBut2015A"] <- "2015-02-24"
	
	d$country[grep("Selian15", d$trial_id)] <- "Tanzania"
	d$planting_year <- NULL

	d <- d[!is.na(d$N_fertilizer), ]
	
	carobiner::write_files(path, meta, d)
}


