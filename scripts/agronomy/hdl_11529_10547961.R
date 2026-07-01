# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
1.7-Kharif (summer) rice-all nodes-Long term trial (LT)-Choohbehar-West Bengal

Farmers' participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives:   1.	Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation.   2.	Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders.   3.	Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems.
"

	uri <- "hdl:11529/10547961"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT; UBKV",
		publication = "doi:10.1016/j.fcr.2019.04.005",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "crop_rotation;land_prep_method",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-01",
		carob_completion = 80,	
		carob_effort = 7
	)
	
  ff <- ff[grepl("nodes", basename(ff))]
	
  ### process
 
	proc <- function(f){
	  r3 <- carobiner::read.excel(f, sheet="2 - Site information")
	  if(grepl("2016-LT-All| 2017-LT-All", f)){hdr <- rbind(r3[4, ], r3[3, ], r3[2, ])} else { hdr <- rbind(r3[5, ], r3[3, ], r3[4, ])}
	  
	  names(r3) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  names(r3) <- make.names(names(r3), unique = TRUE)
	  names(r3) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r3))
	  
	  d1 <- data.frame(
	    year = as.character(r3$Year),
	    season = r3$Season,
	    trial_id = as.character(r3$Trial.Code),
	    cropping_system = gsub("Note:", NA, r3$Cropping.System),
	    location = carobiner::fix_name(r3$Node, "title"),
	    #site = as.character(r3$Site.No..Unique.farmer.ID.),
	    treatment = r3$Tmnt..Short.abbreviation.as.mentioned.in.protocol.,
	    plot_area = r3$Plot.size..m2...each.plot,
	    land_type = r3$Land.type..land.topography..HL..MHL..MLL.LL.,
	    soil_texture = r3$Soil.texture..sand..silt..clay.etc..,
	    #r3$`Soil problem, if any (Flood, acidic soils etc.)`,
	    latitude = suppressWarnings(as.numeric(r3$Latitude.North..Degree.and.decimal.format.only.)),
	    longitude = suppressWarnings(as.numeric(r3$Longitude.East..Degree.and.decimal.format.only.)),
	    farm_nm = r3$Farmer.s.name
	  )
	  
	  d1 <- d1[-c(1:5),]
	  d1$plot_area <- as.numeric(d1$plot_area)
	  d1 <- d1[!is.na(d1$year),]
	  ####
	  if(grepl("2017-LT-All|2016-LT-All", f)){ r4 <- carobiner::read.excel(f, sheet="3-  Land Preparation Operations", skip=4)} else{ r4 <- carobiner::read.excel(f, sheet="3-  Land Prep Operations", skip=4)}
	  names(r4) <- make.names(names(r4), unique = TRUE)
	  names(r4) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r4))
	  d2 <- data.frame(
	    year = as.character(r4$Year),
	    season = r4$Season,
	    trial_id = gsub("0", NA, as.character(r4$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r4$Cropping.System),
	    location = carobiner::fix_name(r4$Node, "title"),
	    #site = as.character(r4$Site.No..Unique.farmer.ID.),
	    treatment = gsub("=|_", "-", r4$Tmnt..Short.abbreviation.as.mentioned.in.protocol.),
	    plot_area = as.numeric(r4$Plot.size..m2...each.plot),
	    #r4$`Labour wages per person-day`,
	    land_prep_method = paste(r4$Operation.2.......Name.of.operation,
	                             r4$Operation.3.......Name.of.operation, sep = ";"),
	    planting_method = paste(r4$Operation.1.......Name.of.operation,
	                             r4$Operation.4.......Name.of.operation,
	                             r4$Operation.5.......Name.of.operation, sep = ";"),
	    farm_nm = r4$Farmer.s.name
	  )
	  
	  d2 <- d2[!is.na(d2$year),]
	  ####
	  ## merge d1 and d2 
	  d <- merge(d1, d2, by=intersect(names(d1), names(d2)), all = TRUE)
	  
	  r5 <- carobiner::read.excel(f, sheet="4- Stand counts & Phenology")
	  hdr <- rbind(r5[4, ], r5[3, ])
	  names(r5) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  
	  names(r5) <- make.names(names(r5), unique = TRUE)
	  names(r5) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r5))
	  names(r5) <- gsub("Date.of.80..physiological.maturity..dd.mm.yy.", "Date.of.80..physiological.maturity..mm.dd.yy.", names(r5))
	  names(r5) <- gsub("Datw.of.harvest..dd.mm.yy.", "Date.of.harvest..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.50..first.flower..mm.dd.yy.", "Date.of.50..first.flower..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.100..plant.emergence..mm.dd.yy.", "Date.of.100..plant.emergence..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Sedding.date", "Date.of.seeding..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.transplanting..mm.dd.yy.", "Date.of.transplanting..dd.mm.yy.", names(r5))
	  
	  d3 <- data.frame(
	    year = as.character(r5$Year),
	    season = r5$Season,
	    trial_id = gsub("0", NA, as.character(r5$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r5$Cropping.System),
	    location = carobiner::fix_name(r5$Node, "title"),
	    country= "India",
	    #site = as.character(r5$Site.No.),
	    treatment = r5$Tmnt,
	    plot_area = r5$Plot.size..m2.,
	    crop = "rice",
	    variety = r5$Variety,
	    plant_height =  suppressWarnings(as.numeric(r5$Height.of.rice.stubble.in.plot..cm.)),
	    planting_date = r5$Date.of.seeding..dd.mm.yy.,
	    transplanting_date = r5$Date.of.transplanting..dd.mm.yy.,
	    #planting_method = r5$`Row seeded/ transplanted (1) or B'cast/random (2)`,
	    row_spacing =  suppressWarnings(as.numeric(r5$Row.spacing..cm.)),
	    seed_rate =  suppressWarnings(as.numeric(r5$Seedrate..kg.ha.)),
	    seed_price = suppressWarnings(as.numeric(r5$Cost.of.seed.per.kg)),
	    emergence_date = r5$Date.of.100..plant.emergence..dd.mm.yy.,
	    ## anthesis = flowering
		flowering_date = r5$Date.of.50..anthesis..mm.dd.yy.,
	    ## first flower ... 
		## flowering_date = r5$Date.of.50..first.flower..dd.mm.yy.,
	    maturity_date = r5$Date.of.80..physiological.maturity..mm.dd.yy.,
	    harvest_date = r5$Date.of.harvest..dd.mm.yy.,
	    emergence_days =  suppressWarnings(as.numeric(r5$X100..emergence..DAS.)),
	    flowering_days =  suppressWarnings(as.numeric(r5$X50..first.flowering..DAS.)),
	    anthesis_days =  suppressWarnings(as.numeric(r5$X50..anthesis..DAS.)),
	    maturity_days =  suppressWarnings(as.numeric(r5$X80..physiological.maturity..DAS.)),
	    harvest_days =  suppressWarnings(as.numeric(r5$Harvesting..DAS.)),
	    seed_cost =  suppressWarnings(as.numeric(r5$Seed.cost..INR.ha.)),
	    farm_nm = r5$Farmer.s.name
	  )
	  
	  d3 <- d3[-c(1:4),]
	  d3$plot_area <- as.numeric(d3$plot_area)
	  d3 <- d3[!is.na(d3$year),]
	  
	  ### Fixing date 
	  conv <- function(x) {
	    x <- suppressWarnings(as.numeric(x))
	    ifelse(
	      is.na(x) | x <= 0,
	      NA_character_,
	      as.character(as.Date(x, origin = "1899-12-30"))
	    )
	  }
	  
	  d3$emergence_date <- conv(d3$emergence_date)
	  d3$flowering_date <- conv(d3$flowering_date)
	  ## d3$anthesis_date <- conv(d3$anthesis_date)
	  d3$planting_date <- conv(d3$planting_date)
	  d3$harvest_date <- conv(d3$harvest_date)
	  d3$maturity_date <- conv(d3$maturity_date)
	  d3$transplanting_date <- conv(d3$transplanting_date)
	  d3$emergence_days[which(d3$emergence_days <0)] <- NA
	  d3$anthesis_days[which(d3$anthesis_days <0)] <- NA
	  ### merge d and d3
	  d <- merge(d, d3, by=intersect(names(d), names(d3)), all = TRUE)
	  ####
	  
	  if(grepl("2017-LT-All|2016-LT-All", f)){r6 <- carobiner::read.excel(f, sheet="5- Crop Managment Operations", skip= 4)} else{r6 <- carobiner::read.excel(f, sheet="5- Crop Mgmt Operations", skip= 4)}
	  names(r6) <- make.names(names(r6), unique = TRUE)
	  names(r6) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r6))
	  d4 <- data.frame(
	    year = as.character(r6$Year),
	    season = r6$Season,
	    trial_id = gsub("0", NA, as.character(r6$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r6$Cropping.System), 
	    location = carobiner::fix_name(r6$Node, "title"),
	    #site = as.character(r6$Site.No.),
	    treatment = r6$Tmnt,
	    plot_area = as.numeric(r6$Plot.size..m2.),
	    weeding_dates = as.character(r6$X0peration.2.........Date..mm.dd.yy.),
	    weeding_done = grepl("WEEDING", r6$Operation.2.......Name.of.operation),
	    weeding_implement = tolower(r6$X0peration.3...Implement.used),
	    labour = r6$Total.labor..no.ha.,
	    farm_nm = r6$Farmer.s.name
	    
	  )
	  
	  d4 <- d4[!is.na(d4$year),]
	  
	  ### merge d and d4
	  d <- merge(d, d4, by = intersect(names(d), names(d4)), all = TRUE)
	 
	  r7 <- carobiner::read.excel(f, sheet="6 - Fertilizer amounts ")
	  hdr <- rbind(r7[4, ], r7[3, ])
	  names(r7) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  names(r7) <- make.names(names(r7), unique = TRUE)
	  names(r7) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r7))
	  if (is.null(r7$Date.of.application...dd.mm.yy..5))  r7$Date.of.application...dd.mm.yy..5 <- NA
	  if (is.null(r7$Date.of.application...dd.mm.yy..6)) r7$Date.of.application...dd.mm.yy..6 <- NA
	  d5 <- data.frame(
	    year = as.character(r7$Year),
	    season = r7$Season,
	    trial_id = gsub("0", NA, as.character(r7$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r7$Cropping.System),
	    location = carobiner::fix_name(r7$Node, "title"),
	    #site = as.character(r7$Site.No.),
	    treatment = r7$Tmnt,
	    plot_area = r7$Plot.size..m2.,
	    #fertilizer_type1 = "basal",
	    N_fertilizer1 = r7$Fert.Grade.N,
	    P_fertilizer1 = r7$Fert.Grade.P2O5,
	    K_fertilizer1 = r7$Fert.Grade.K2O,
	    fertilizer_date2 = r7$Date.of.application...dd.mm.yy.,
	    fertilizer_amount1 = r7$Fertilizer.applied.g.plot,
	    fertilizer_price1 = r7$Farm.gate.price.per.kg,
	    #r7$`Product used.1`,
	    N_fertilizer2 = r7$Fert.Grade.N.1,
	    P_fertilizer2 = r7$Fert.Grade.P2O5.1,
	    K_fertilizer2 = r7$Fert.Grade.K2O.1,
	    fertilizer_amount2 = r7$Fertilizer.applied.g.plot.1,
	    fertilizer_price2 = r7$Farm.gate.price.per.kg.1,
	    #fertilizer_type3 = r7$`Product used.2`,
	    N_fertilizer3 = r7$Fert.Grade.N.2,
	    P_fertilizer3 = r7$Fert.Grade.P2O5.2,
	    K_fertilizer3 = r7$Fert.Grade.K2O.2,
	    fertilizer_date3 = r7$Date.of.application...dd.mm.yy..1,
	    fertilizer_amount3 = r7$Fertilizer.applied.g.plot.2,
	    fertilizer_price3 = r7$Farm.gate.price.per.kg.2,
	    #fertilizer_type4 = tolower(r7$`Product used.3`),
	    N_fertilizer4 = r7$Fert.Grade.N.3,
	    P_fertilizer4 = r7$Fert.Grade.P2O5.3,
	    K_fertilizer4 = r7$Fert.Grade.K2O.3,
	    fertilizer_date4 = r7$Date.of.application...dd.mm.yy..2,
	    fertilizer_amount4 = r7$Fertilizer.applied.g.plot.3,
	    fertilizer_price4 = r7$Farm.gate.price.per.kg.3,
	    #fertilizer_type5 = tolower(r7$`Product used.4`),
	    N_fertilizer5 = r7$Fert.Grade.N.4,
	    P_fertilizer5 = r7$Fert.Grade.P2O5.4,
	    K_fertilizer5 = r7$Fert.Grade.K2O.4,
	    fertilizer_date5 = r7$Date.of.application...dd.mm.yy..3,
	    fertilizer_amount5 = r7$Fertilizer.applied.g.plot.4,
	    fertilizer_price5 = r7$Farm.gate.price.per.kg.4,
	    #fertilizer_type6 = r7$`Product used.5`,
	    N_fertilizer6 = r7$Fert.Grade.N.5,
	    P_fertilizer6 = r7$Fert.Grade.P2O5.5,
	    K_fertilizer6 = r7$Fert.Grade.K2O.5,
	    fertilizer_date6 = r7$Date.of.application...dd.mm.yy..4,
	    fertilizer_amount6 = r7$Fertilizer.applied.g.plot.5,
	    fertilizer_price6 = r7$Farm.gate.price.per.kg.5,
	    #fertilizer_type7 = r7$`Product used.6`,
	    N_fertilizer7 = r7$Fert.Grade.N.6,
	    P_fertilizer7 = r7$Fert.Grade.P2O5.6,
	    K_fertilizer7 = r7$Fert.Grade.K2O.6,
	    fertilizer_date7 = r7$Date.of.application...dd.mm.yy..5,
	    fertilizer_amount7 = r7$Fertilizer.applied.g.plot.6,
	    fertilizer_price7 = r7$Farm.gate.price.per.kg.6,
	    N_fertilizer8 = r7$Fert.Grade.N.7,
	    P_fertilizer8 = r7$Fert.Grade.P2O5.7,
	    K_fertilizer8 = r7$Fert.Grade.K2O.7,
	    fertilizer_date8 = r7$Date.of.application...dd.mm.yy..6,
	    fertilizer_amount8 = r7$Fertilizer.applied.g.plot.7,
	    fertilizer_price8 = r7$Farm.gate.price.per.kg.7,
	    N_fertilizer9 = r7$N....kg.ha.,
	    fertilizer_cost = suppressWarnings(as.numeric(r7$Fertilizer.cost..INR.ha.)),
	    P_fertilizer9 = r7$P2O5..kg.ha.,
	    K_fertilizer9 = r7$K2O..kg.ha.,
	    fertilizer_type = "SSP;urea;KCl",
	    farm_nm = r7$Farmer.s.name
	    
	  )
	  
	  d5 <- d5[-c(1:4),]
	  d5$plot_area <- as.numeric(d5$plot_area)
	  d5 <- d5[!is.na(d5$year),]
	  
	  d5$N_fertilizer <- rowSums( apply(d5[, paste0("N_fertilizer", 1:9)],2, as.numeric),na.rm = TRUE)
	  d5$P_fertilizer <- rowSums(apply(d5[, paste0("P_fertilizer", 1:9)], 2, as.numeric),na.rm = TRUE)/2.29
	  d5$K_fertilizer <- rowSums(apply(d5[, paste0("K_fertilizer", 1:9)], 2, as.numeric),na.rm = TRUE)/1.2051
	  d5$fertilizer_amount <- (rowSums( apply(d5[, paste0("fertilizer_amount", 1:8)],2, as.numeric),na.rm = TRUE)/d5$plot_area)*10 # kg/ha
	  d5$fertilizer_price <- rowSums( apply(d5[, paste0("fertilizer_price", 1:8)],2, as.numeric),na.rm = TRUE) 
	  d5$fertilizer_date <- d5$fertilizer_date <- apply(d5[, paste0("fertilizer_date", 2:8)], 1, function(x) {
	      x <- as.numeric(x)
	      x <- as.Date(x, origin = "1899-12-30")
	      paste(na.omit(x), collapse = ";")})
	
	  d5 <- d5[, !names(d5) %in% c(paste0("N_fertilizer", 1:9),paste0("P_fertilizer", 1:9),paste0("K_fertilizer", 1:9), paste0("fertilizer_amount", 1:8), paste0("fertilizer_price", 1:8), paste0("fertilizer_date", 2:8))] 
	  
	  ### merge d and d5
	  d5 <- d5[which(d5$year!=0),]
	  d <- merge(d, d5, by=intersect(names(d), names(d5)), all = TRUE)
	  
	  ###
	  r10 <- carobiner::read.excel(f, sheet="9 - Pesticide applications", na= "")
	  hdr <- rbind(r10[4, ], r10[3, ])
	  names(r10) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  names(r10) <- make.names(names(r10), unique = TRUE)
	  names(r10) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r10))
	  
	 d6 <- data.frame(
	   year = as.character(r10$Year),
	   season = r10$Season,
	   trial_id = gsub("0", NA, as.character(r10$Trial.Code)),
	   cropping_system = gsub("Note:", NA, r10$Cropping.System),
	   location = carobiner::fix_name(r10$Node, "title"),
	   #site = as.character(r10$Site.No.),
	   treatment = r10$Tmnt,
	   plot_area = r10$Plot.size..m2.,
	   pesticide_date1 = r10$Date.of.application..dd.mm.yy.,
	   pesticide_date2 = gsub("8/8/116", NA, r10$Date.of.application..dd.mm.yy..1),
	   herbicide_product = gsub("NA;NA", NA, paste(tolower(r10$Product.applied), tolower(r10$Product.applied.1), sep = ";")),
	   herbicide_implement =  gsub("NA;NA", NA, paste(tolower(r10$Method.of.application), tolower(r10$Method.of.application.1), sep = ";")),
	   pesticide_amount1 = r10$Rate.of.application..g.or.ml.ha.,
	   #r10$`No. of labourers/plot`,
	   #r10$`Time taken/plot (minutes)`,
	   pesticide_price1 = r10$Farm.gate.price.of.product.container,
	   Csize1 = r10$Container.size..g.or.ml. ,
	   pesticide_amount2 = r10$Rate.of.application..g.or.ml.ha..1,
	   pesticide_price2 = r10$Farm.gate.price.of.product.container.1,
	   Csize2 = r10$Container.size..g.or.ml..1,
	   farm_nm = r10$Farmer.s.name
	 )
	  
	 d6 <- d6[-c(1:4),]
	 d6$plot_area <- as.numeric(d6$plot_area)
	 d6 <- d6[!is.na(d6$year),]
	 
	 d6$pesticide_price2 <- (as.numeric(d6$pesticide_price2)/as.numeric(d6$Csize2))*1000 ## price/kg 
	 d6$pesticide_price1 <- (as.numeric(d6$pesticide_price1)/as.numeric(d6$Csize1))*1000 ## price/kg 
	 d6$herbicide_amount <- rowSums(apply(d6[, c("pesticide_amount1", "pesticide_amount2")], 2, as.numeric),na.rm = TRUE)/1000 #kg/ha
	 d6$herbicide_price <- rowSums(apply(d6[, c("pesticide_price1", "pesticide_price2")], 2, as.numeric), na.rm = TRUE)
	 d6$herbicide_dates <- apply(d6[, paste0("pesticide_date", 1:2)], 1, function(x) {
	   x <- as.numeric(x)
	   x <- as.Date(x, origin = "1899-12-30")
	   gsub("NA;NA", NA, paste(x, collapse = ";"))})
	 d6$herbicide_product = gsub("NA;", "", d6$herbicide_product)
	 
	 d6$pesticide_price1 <- d6$pesticide_price2 <- d6$pesticide_amount1 <- d6$pesticide_amount2 <- d6$Csize1 <- d6$Csize2 <- d6$pesticide_date1 <- d6$pesticide_date2 <- NULL

	 ### merge d and d6
	 d6 <- d6[which(d6$year!="0"),]
	 d <- merge(d, d6, by=intersect(names(d), names(d6)), all = TRUE)
	 
	 
	 ####
	 r11 <- carobiner::read.excel(f, sheet = "11 - Harvest operations", skip= 4)
	 names(r11) <- make.names(names(r11), unique = TRUE)
	 names(r11) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r11))
	 names(r11) <- gsub("Trial.Code", "Trial.code", names(r11))
	 names(r11) <- gsub("Total.labor.for.harvest.operation..no.ha.", "Total.labor.for.harvesting.operation..no.ha.", names(r11))
	 
	 d7 <- data.frame(
	   year = as.character(r11$Year),
	   season = r11$Season,
	   trial_id = gsub("0", NA, as.character(r11$Trial.code)),
	   cropping_system = r11$Cropping.System,
	   location = carobiner::fix_name(r11$Node, "title"),
	   farm_nm= r11$Farmer.s.name,
	   treatment = r11$Tmnt,
	   plot_area = as.numeric(r11$Plot.size..m2.),
	   #harvest_labour = r11$Total.labor.for.harvesting.operation..no.ha.,
	   harvest_cost = r11$Operation.cost.thresher..INR.ha.
	 ) 
	 
	 ####
	 ### merge d and d6
	 d7 <- d7[which(d7$year!="0"),]
	 d <- merge(d, d7, by=intersect(names(d), names(d7)), all = TRUE)
	 

	 r14 <- carobiner::read.excel(f, sheet="14 - Grain Harvest ")
	 hdr <- rbind(r14[4, ], r14[3, ])
	 names(r14) <- apply(hdr, 2, function(x) {
	   x <- trimws(as.character(x))
	   x <- x[!is.na(x) & x != ""]
	   if (length(x)) x[1] else NA_character_
	 })
	 names(r14) <- make.names(names(r14), unique = TRUE)
	 names(r14) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r14))
	 names(r14) <- gsub("Grain.yield..t.ha.|Sun.dry.grain.yield..t.ha.|Sundry.grain.yield..t.ha", "grain.yield.t.ha.", names(r14))
	 names(r14) <- gsub("Trial.Code", "Trial.code", names(r14))
	 
	 d8 <- data.frame(
	   year = as.character(r14$Year),
	   season = r14$Season,
	   trial_id = gsub("0", NA, as.character(r14$Trial.code)),
	   cropping_system = gsub("Note:", NA, r14$Cropping.System),
	   location = carobiner::fix_name(r14$Node, "title"),
	   #site = as.character(r14$Site.No.),
	   treatment = r14$Tmnt,
	   plot_area = r14$Plot.size..m2.,
	   yield = suppressWarnings(as.numeric(r14$grain.yield.t.ha.))*1000,
	   fwy_total =  suppressWarnings(as.numeric(r14$Biomass..t.ha.))*1000,
	   harvest_index =  suppressWarnings(as.numeric(r14$HI)),
	   farm_nm = r14$Farmer.s.name
	 )
	  
	 d8 <- d8[-c(1:4),]
	 d8$plot_area <- as.numeric(d8$plot_area)
	 d8 <- d8[!is.na(d8$year),]
	 
	 ### merge d and d8
	 d8 <- d8[which(d8$year!="0"),]
	 d <- merge(d, d8, by=intersect(names(d), names(d8)), all = TRUE)
	 d$season <- tolower(d$season)
	 d$crop_rotation  <- ifelse(grepl("R-L-J", d$cropping_system), "rice;lentil;jute",
	                     ifelse(grepl("R-W-J", d$cropping_system), "rice;wheat;jute", "rice;maize")) 
	 d$year <- d$farm_nm <- d$cropping_system <- NULL
	 
	 
	 d
	}	
	
	d <- lapply(ff, proc)
	d <- do.call(rbind, d)
	
	#### Fixing planting method 
	d$planting_method <- "transplanting"
	d$fertilizer_date[d$fertilizer_date== ""] <- NA
	### Fixing land_prep
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub("Tillage;Puddling|TILLAGE;Puddling", "conventional;puddled", P)
	P <- gsub("Siggle tillage;NA|Single TILLAGE;-", "conventional", P)
	P <- gsub("Puddling;NA", "puddled", P)
	P <- gsub("Dry tillage;Transplanting", "conventional", P)
	d$land_prep_method <- P
	d$land_prep_method <- ifelse(is.na(d$land_prep_method) &grepl("UPTPR", d$treatment), "not puddled",
	                       ifelse(is.na(d$land_prep_method) &grepl("CTTPR", d$treatment), "conventional", d$land_prep_method))
	
	### herbicide implement
	P <- carobiner::fix_name(d$herbicide_implement)
	P <- gsub("spray;spraying|NA;spraying", "manure spreader", P)
	P <- gsub("spray;broadcasting", "broadcast spreader", P)
	P <- gsub("NA;broadcasting", "broadcast spreader", P)
	d$herbicide_implement <- P
	d$herbicide_dates <- gsub("NA;|;NA", "", d$herbicide_dates)
	
	#### Fixing lon and lat coordinate
	
	geo <- data.frame(
	  location = c("Durganagar", "Falimari", "Ghughumari","Mansai", "Patchara", "Folimari"),
	  long = c(88.214, 89.817, 89.3270, 89.723, 87.667, 89.818),
	  lat = c(22.1968, 26.391, 26.333, 26.272, 22.302, 26.391)
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$long[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$long <- d$lat <- NULL
	
	d$on_farm <- TRUE 
	d$is_survey <- FALSE 
	d$yield_part <- "grain" 
	d$country <- "India" 
	d$yield_moisture <- NA_real_
	d$geo_from_source <- FALSE
	d$irrigated <- NA
	d$yield_isfresh <- TRUE
	
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}

