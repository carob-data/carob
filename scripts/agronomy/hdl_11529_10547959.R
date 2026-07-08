# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
1.5-Kharif (summer) rice-all nodes-Long term trial (LT)-Dhanusha-Nepal

Farmers' participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives:   1.	Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation.   2.	Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders.   3.	Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems.
"

	uri <- "hdl:11529/10547959"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=1,
		data_organization = "CIMMYT; NARC",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "crop_rotation;land_prep_method",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-07",
		carob_completion = 80,	
		carob_effort = 7
	)
	

	ff <- ff[grepl("nodes|LT", basename(ff))]
	
	### process
	
	proc <- function(f){
	  if(grepl("2016-LT-Giddha|2016-LT-Phulgama|2016-LT-Ragunathpur|2016-LT-Sinurjoda|2017-LT-Giddha|2017-LT-Phulgama|2017-LT-Sinurjoda", f)){r3 <- carobiner::read.excel(f, sheet="2-Site Information")
	  } else if (grepl("2016-LT-Lalgadh", f)){r3 <- carobiner::read.excel(f, sheet="2.site informations")
	  } else{r3 <- carobiner::read.excel(f, sheet="2 - Site information")}
	  
	  if(grepl("2016-LT-Giddha|2016-LT-Phulgama|2016-LT-Ragunathpur|2017-LT-Giddha|2017-LT-Phulgama|2017-LT-Sinurjoda|2016-LT-Lalgadh|2016-LT-Sinurjoda", f)){hdr <- rbind(r3[4, ], r3[3, ], r3[2, ])} else { hdr <- rbind(r3[5, ], r3[3, ], r3[4, ])}
	  #hdr <- rbind(r3[5, ], r3[4, ], r3[3, ])
	  names(r3) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r3 <- r3[-(1:5),] ### header
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
	    plot_area = as.numeric(r3$Plot.size..m2...each.plot),
	    #land_type = r3$Land.type..land.topography..HL..MHL..MLL.LL.,
	    soil_texture = tolower(r3$Soil.texture..sand..silt..clay.etc.),
	    #r3$`Soil problem, if any (Flood, acidic soils etc.)`,
	    latitude = as.numeric(r3$Latitude.North..Degree.and.decimal.format.only.),
	    longitude = as.numeric(r3$Longitude.East..Degree.and.decimal.format.only.),
	    farm_nm = r3$Farmer.s.name
	  )
	  
	  d1 <- d1[!is.na(d1$year),]
	  ####
	  if(grepl("2014-LT-All|2015-LT", f)){ r4 <- carobiner::read.excel(f, sheet="3-  Land Prep Operations", skip=4)
	  } else if(grepl("2016-LT-Giddha|2016-LT-Phulgama|2016-LT-Ragunathpur|2016-LT-Sinurjoda|2017-LT-Giddha|2017-LT-Phulgama|2017-LT-Sinurjoda", f)){r4 <- carobiner::read.excel(f, sheet="3-Land prep Operations", skip=4)
	  } else if (grepl("2016-LT-Lalgadh", f)) {r4 <- carobiner::read.excel(f, sheet="3.land prep.oparations", skip=4)
	  } else{ r4 <- carobiner::read.excel(f,sheet="3-  Land Preparation Operations", skip=4)}
	 
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
	    land_prep_method = paste(r4$Operation.1.......Name.of.operation,
	                             r4$Operation.2.......Name.of.operation,
	                             r4$Operation.3.......Name.of.operation, sep = ";"),
	    planting_method = paste(r4$Operation.5.......Name.of.operation,
	                            r4$Operation.6.......Name.of.operation,
	                            r4$Operation.8.......Name.of.operation, sep = ";"),
	    equipment_cost = r4$Equipment..cost.for.tillage.ZTSD..Puddling..NRP.ha.,
	    farm_nm = r4$Farmer.s.name
	  )
	  
	  d2 <- d2[!is.na(d2$year),]
	  
	  ## merge d1 and d2 
	  d <- merge(d1, d2, by=intersect(names(d1), names(d2)), all = TRUE)
	  
	  if (grepl("2016-LT-Giddha|2016-LT-Phulgama|2016-LT-Ragunathpur|2016-LT-Sinurjoda|2017-LT-Giddha|2017-LT-Phulgama|2017-LT-Sinurjoda", f)){
	   r5 <- carobiner::read.excel(f, sheet = "4.Stand counts &phenology")
	      if(grepl("2016-LT-Phulgama|2017-LT-Phulgama", f)){hdr <- rbind(r5[2, ], r5[3, ])
	       }else if (grepl("2016-LT-Ragunathpur", f)){ hdr <- rbind(r5[4, ], r5[3, ])}else{hdr <- rbind(r5[2, ], r5[1, ])}
	  }else if (grepl("2016-LT-Lalgadh", f)){r5 <- carobiner::read.excel(f, sheet="4. Stand count & Phenology")
	    hdr <- rbind(r5[4, ], r5[3, ])
	    } else {
	    r5 <- carobiner::read.excel(f, sheet="4- Stand counts & Phenology")
	    hdr <- rbind(r5[4, ], r5[3, ])}
	  names(r5) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r5 <- r5[-c(1:4),] ### header
	  names(r5) <- make.names(names(r5), unique = TRUE)
	  names(r5) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r5))
	  names(r5) <- gsub("Date.of.80..physiological.maturity..dd.mm.yy.", "Date.of.80..physiological.maturity..mm.dd.yy.", names(r5))
	  names(r5) <- gsub("Datw.of.harvest..dd.mm.yy.", "Date.of.harvest..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.50..first.flower..mm.dd.yy.", "Date.of.50..first.flower..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.100..plant.emergence..mm.dd.yy.", "Date.of.100..plant.emergence..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Sedding.date", "Date.of.seeding..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.transplanting..mm.dd.yy.", "Date.of.transplanting..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.50..anthesis..dd.mm.yy.", "Date.of.50..anthesis..mm.dd.yy.", names(r5))
	  names(r5) <- gsub("X90..physiological.maturity..DAS.", "X80..physiological.maturity..DAS.", names(r5))
	  names(r5) <- gsub("Date.of.seeding..mm.dd.yy.", "Date.of.seeding..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Datw.of.harvest..mm.dd.yy.", "Date.of.harvest..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Tmnt..Short.abbreviation.as.mentioned.in.protocol.", "Tmnt", names(r5))

	  if(is.null(r5$Seed.cost..NRP.ha.)) {r5$Seed.cost..NRP.ha. <- NA}
	  if(is.null(r5$Cost.of.seed.per.kg)) {r5$Cost.of.seed.per.kg <- NA}
	  if(is.null(r5$Harvesting..DAS.)) {r5$Harvesting..DAS. <- NA}
	  if(is.null(r5$X80..physiological.maturity..DAS.)) {r5$X80..physiological.maturity..DAS. <- NA}
	  if(is.null(r5$X50..anthesis..DAS.)) {r5$X50..anthesis..DAS. <- NA}
	  if(is.null(r5$X50..first.flowering..DAS.)) {r5$X50..first.flowering..DAS. <- NA}
	  if(is.null(r5$X100..emergence..DAS.)) {r5$X100..emergence..DAS. <- NA}
	  d3 <- data.frame(
	    year = as.character(r5$Year),
	    season = r5$Season,
	    trial_id = gsub("0", NA, as.character(r5$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r5$Cropping.System),
	    location = carobiner::fix_name(r5$Node, "title"),
	    #site = as.character(r5$Site.No.),
	    treatment = r5$Tmnt,
	    plot_area = as.numeric(r5$Plot.size..m2.),
	    variety = r5$Variety,
	    plant_height =  as.numeric(r5$Height.of.rice.stubble.in.plot..cm.),
	    planting_date = r5$Date.of.seeding..dd.mm.yy.,
	    transplanting_date = r5$Date.of.transplanting..dd.mm.yy.,
	    #planting_method = r5$`Row seeded/ transplanted (1) or B'cast/random (2)`,
	    row_spacing = as.numeric(r5$Row.spacing..cm.),
	    seed_rate =  as.numeric(r5$Seedrate..kg.ha.),
	    emergence_date = r5$Date.of.100..plant.emergence..dd.mm.yy.,
	    flowering_date = r5$Date.of.50..anthesis..mm.dd.yy.,
	    maturity_date = r5$Date.of.80..physiological.maturity..mm.dd.yy.,
	    harvest_date = r5$Date.of.harvest..dd.mm.yy.,
	    emergence_days =  as.numeric(r5$X100..emergence..DAS.),
	    flowering_days =  as.numeric(r5$X50..first.flowering..DAS.),
	    anthesis_days =  as.numeric(r5$X50..anthesis..DAS.),
	    maturity_days =  as.numeric(r5$X80..physiological.maturity..DAS.),
	    harvest_days =  as.numeric(r5$Harvesting..DAS.),
	    seed_cost =  as.numeric(r5$Seed.cost..NRP.ha.),
	    seed_price = as.numeric(r5$Cost.of.seed.per.kg),
	    farm_nm = r5$Farmer.s.name
	  )
	  
	  d3 <- d3[!is.na(d3$year),]
	  
	  ### Fixing date 
	  conv <- function(x) {
	    x <- as.numeric(x)
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
	  
	  if(grepl("2016-LT-Giddha|2016-LT-Phulgama|2016-LT-Ragunathpur|2016-LT-Sinurjoda|2017-LT-Giddha|2017-LT-Phulgama|2017-LT-Sinurjoda", f)){
	  r6 <- carobiner::read.excel(f, sheet="5.Crop mgmt operations", skip= 4)
	  } else if(grepl("2016-LT-Lalgadh", f)){r6 <- carobiner::read.excel(f, sheet="5.CRop mgmt.operations", skip= 4)
	  } else{r6 <- carobiner::read.excel(f, sheet="5- Crop Mgmt Operations", skip= 4)}
	  #r6 <- carobiner::read.excel(f, sheet="5- Crop Mgmt Operations", skip= 4)
	  names(r6) <- make.names(names(r6), unique = TRUE)
	  names(r6) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r6))
	  names(r6) <- gsub("Plot.size..m2...each.plot", "Plot.size..m2", names(r6))
	  names(r6) <- gsub("Plot.size..m2...each.plot", "Plot.size..m2", names(r6))
	  names(r6) <- gsub("Tmnt..Short.abbreviation.as.mentioned.in.protocol.", "Tmnt", names(r6))
	  d4 <- data.frame(
	    year = as.character(r6$Year),
	    season = r6$Season,
	    trial_id = gsub("0", NA, as.character(r6$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r6$Cropping.System), 
	    location = carobiner::fix_name(r6$Node, "title"),
	    #site = as.character(r6$Site.No.),
	    treatment = r6$Tmnt,
	    plot_area = as.numeric(r6$Plot.size..m2),
	    weeding_dates = as.character(r6$X0peration.4.........Date..mm.dd.yy.),
	    weeding_done = grepl("WEEDING|weeding", r6$Operation.4.......Name.of.operation),
	    weeding_implement = tolower(r6$X0peration.4...Implement.used),
	    labour = r6$Total.labor..no.ha.,
	    farm_nm = r6$Farmer.s.name
	    
	  )
	  
	  d4 <- d4[!is.na(d4$year),]
	  
	  ### merge d and d4
	  d <- merge(d, d4, by = intersect(names(d), names(d4)), all = TRUE)
	  
	  if(grepl("2016-LT-Giddha|2016-LT-Phulgama|2016-LT-Ragunathpur|2016-LT-Sinurjoda|2017-LT-Giddha|2017-LT-Phulgama|2017-LT-Sinurjoda", f)) {r7 <- carobiner::read.excel(f, sheet="6.fertilizer amount")
	  } else if (grepl("2016-LT-Lalgadh", f)){r7 <- carobiner::read.excel(f, sheet="6.Fertilizer amounts")
	  }else {r7 <- carobiner::read.excel(f, sheet="6 - Fertilizer amounts ")}
	  hdr <- rbind(r7[4, ], r7[3, ])
	  names(r7) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  
	  r7 <- r7[-c(1:4),] ### header
	  names(r7) <- make.names(names(r7), unique = TRUE)
	  names(r7) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r7))
	  names(r7) <- gsub("K2O...kg.ha.|K2O..kg.ha.", "K2O....kg.ha.", names(r7))
	  names(r7) <- gsub("P2O5..kg.ha.", "P2O5....kg.ha.", names(r7))
	  names(r7) <- gsub("N...kg.ha.|N..kg.ha.", "N....kg.ha.", names(r7))
	  if (is.null(r7$Date.of.application...dd.mm.yy..5))  r7$Date.of.application...dd.mm.yy..5 <- NA
	  if (is.null(r7$Date.of.application...dd.mm.yy..6)) r7$Date.of.application...dd.mm.yy..6 <- NA
	  if(is.null(r7$Fertilizer.applied.g.plot.6)){r7$Fertilizer.applied.g.plot.6 <- NA}
	  d5 <- data.frame(
	    year = as.character(r7$Year),
	    season = r7$Season,
	    trial_id = gsub("0", NA, as.character(r7$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r7$Cropping.System),
	    location = carobiner::fix_name(r7$Node, "title"),
	    #site = as.character(r7$Site.No.),
	    treatment = r7$Tmnt,
	    plot_area = as.numeric(r7$Plot.size..m2.),
	    #fertilizer_type1 = "basal",
	    N_fertilizer1 = as.numeric(r7$Fert.Grade.N),
	    P_fertilizer1 = as.numeric(r7$Fert.Grade.P2O5),
	    K_fertilizer1 = as.numeric(r7$Fert.Grade.K2O),
	    fertilizer_date1 = as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy.), origin = "1899-12-30")),
	    fertilizer_amount1 = as.numeric(r7$Fertilizer.applied.g.plot),
	    fertilizer_price1 = as.numeric(r7$Farm.gate.price.per.kg),
	    #r7$`Product used.1`,
	    N_fertilizer2 = as.numeric(r7$Fert.Grade.N.1),
	    P_fertilizer2 = as.numeric(r7$Fert.Grade.P2O5.1),
	    K_fertilizer2 = as.numeric(r7$Fert.Grade.K2O.1),
	    fertilizer_date2 = as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy.), origin = "1899-12-30")),
	    fertilizer_amount2 = as.numeric(r7$Fertilizer.applied.g.plot.1),
	    fertilizer_price2 = as.numeric(r7$Farm.gate.price.per.kg.1),
	    #fertilizer_type3 = r7$`Product used.2`,
	    N_fertilizer3 = as.numeric(r7$Fert.Grade.N.2),
	    P_fertilizer3 = as.numeric(r7$Fert.Grade.P2O5.2),
	    K_fertilizer3 = as.numeric(r7$Fert.Grade.K2O.2),
	    fertilizer_date3 =  as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..1), origin = "1899-12-30")),
	    fertilizer_amount3 = as.numeric(r7$Fertilizer.applied.g.plot.2),
	    fertilizer_price3 = as.numeric(r7$Farm.gate.price.per.kg.2),
	    #fertilizer_type4 = tolower(r7$`Product used.3`),
	    N_fertilizer4 = as.numeric(r7$Fert.Grade.N.3),
	    P_fertilizer4 = as.numeric(r7$Fert.Grade.P2O5.3),
	    K_fertilizer4 = as.numeric(r7$Fert.Grade.K2O.3),
	    fertilizer_date4 =  as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..2), origin = "1899-12-30")),
	    fertilizer_amount4 = as.numeric(r7$Fertilizer.applied.g.plot.3),
	    fertilizer_price4 = as.numeric(r7$Farm.gate.price.per.kg.3),
	    #fertilizer_type5 = tolower(r7$`Product used.4`),
	    N_fertilizer5 = as.numeric(r7$Fert.Grade.N.4),
	    P_fertilizer5 = as.numeric(r7$Fert.Grade.P2O5.4),
	    K_fertilizer5 = as.numeric(r7$Fert.Grade.K2O.4),
	    fertilizer_date5 =  as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..3), origin = "1899-12-30")),
	    fertilizer_amount5 = as.numeric(r7$Fertilizer.applied.g.plot.4),
	    fertilizer_price5 = as.numeric(r7$Farm.gate.price.per.kg.4),
	    #fertilizer_type6 = r7$`Product used.5`,
	    N_fertilizer6 = as.numeric(r7$Fert.Grade.N.5),
	    P_fertilizer6 = as.numeric(r7$Fert.Grade.P2O5.5),
	    K_fertilizer6 = as.numeric(r7$Fert.Grade.K2O.5),
	    fertilizer_date6 =  as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..4), origin = "1899-12-30")),
	    fertilizer_amount6 = as.numeric(r7$Fertilizer.applied.g.plot.5),
	    fertilizer_price6 = as.numeric(r7$Farm.gate.price.per.kg.5),
	    #fertilizer_type7 = r7$`Product used.6`,
	    N_fertilizer7 = as.numeric(r7$Fert.Grade.N.6),
	    P_fertilizer7 = as.numeric(r7$Fert.Grade.P2O5.6),
	    K_fertilizer7 = as.numeric(r7$Fert.Grade.K2O.6),
	    fertilizer_date7 =  as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..5), origin = "1899-12-30")),
	    fertilizer_amount7 = as.numeric(r7$Fertilizer.applied.g.plot.6),
	    fertilizer_price7 = as.numeric(r7$Farm.gate.price.per.kg.6),
	    N_fertilizer8 =  as.numeric(r7$N....kg.ha.),
	    P_fertilizer8 = as.numeric(r7$P2O5....kg.ha.),
	    K_fertilizer8 = as.numeric(r7$K2O....kg.ha.),
	    fertilizer_date8 =  as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..6), origin = "1899-12-30")),
	    fertilizer_cost = as.numeric(r7$Total.fertiliser.cost..NRP.ha.),
	    fertilizer_price8 = NA,
	    fertilizer_amount8 = NA,
	    fertilizer_type = "DAP;urea;KCl",
	    farm_nm = r7$Farmer.s.name
	    
	  )
	  
	  d5 <- d5[!is.na(d5$crop),]
	  
	  ### merge d and d5
	  d5 <- d5[which(d5$year!=0),]
	  d <- merge(d, d5, by=intersect(names(d), names(d5)), all = TRUE)
	  
	  ###
	  
	  if(grepl("2016-LT-Giddha|2016-LT-Phulgama|2016-LT-Ragunathpur|2016-LT-Sinurjoda|2017-LT-Giddha|2017-LT-Phulgama|2017-LT-Sinurjoda", f)){r10 <- carobiner::read.excel(f, sheet="9.Pesticides application", na= "")
	  } else if(grepl("2016-LT-Lalgadh", f)){r10 <- carobiner::read.excel(f, sheet="9.Pesticides applications", na= "")
	  } else{r10 <- carobiner::read.excel(f, sheet="9 - Pesticide applications", na= "")}
	  hdr <- rbind(r10[4, ], r10[3, ])
	  names(r10) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  
	  r10 <- r10[-c(1:4),]
	  r10 <- r10[!is.na(r10$Year),]
	  names(r10) <- make.names(names(r10), unique = TRUE)
	  names(r10) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r10))
	  names(r10) <- gsub("ha.", "Plot.", names(r10))
	  names(r10) <- gsub("plot", "Plot", names(r10))
	  names(r10) <- gsub(".100ml", ".1", names(r10))
	  names(r10) <- gsub(".250ml", ".2", names(r10))
	  names(r10) <- gsub(".250ml", ".2", names(r10))
	  names(r10) <- gsub("ml.L.", "", names(r10))
	  names(r10) <- gsub("container.ml", "container.2", names(r10))
	  names(r10) <- gsub("Farm.gate.price.of.product.container.20g", "Farm.gate.price.of.product.container.3", names(r10))
	  if(is.null(r10$Rate.of.application..g.or.ml.Plot..2)){r10$Rate.of.application..g.or.ml.Plot..2 <- NA}
	  if(is.null(r10$Rate.of.application..g.or.ml.Plot..3)){r10$Rate.of.application..g.or.ml.Plot..3 <- NA}
	  if(is.null(r10$Date.of.application..dd.mm.yy..4)){r10$Date.of.application..dd.mm.yy..4 <- NA}
	  if(is.null(r10$Farm.gate.price.of.product.container.4)){r10$Farm.gate.price.of.product.container.4 <- NA}
	  d6 <- data.frame(
	    year = as.character(r10$Year),
	    season = r10$Season,
	    trial_id = gsub("0", NA, as.character(r10$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r10$Cropping.System),
	    location = carobiner::fix_name(r10$Node, "title"),
	    #site = as.character(r10$Site.No.),
	    treatment = r10$Tmnt,
	    plot_area = as.numeric(r10$Plot.size..m2.),
	    pesticide_date1 = r10$Date.of.application..dd.mm.yy.,
	    pesticide_date2 = gsub("8/8/116", NA, r10$Date.of.application..dd.mm.yy..1),
	    pesticide_date3 = gsub("8/8/116", NA, r10$Date.of.application..dd.mm.yy..2),
	    pesticide_date4 = gsub("8/8/116", NA, r10$Date.of.application..dd.mm.yy..3),
	    pesticide_date5 = gsub("8/8/116", NA, r10$Date.of.application..dd.mm.yy..4),
	    herbicide_product = gsub("NA;NA", NA, paste(tolower(r10$Product.applied), tolower(r10$Product.applied.1), tolower(r10$Product.applied.2),  tolower(r10$Product.applied.3), tolower(r10$Product.applied.4), sep = ";")),
	    herbicide_implement =  gsub("NA;NA", NA, paste(tolower(r10$Method.of.application), tolower(r10$Method.of.application.1), tolower(r10$Method.of.application.2), tolower(r10$Method.of.application.3),tolower(r10$Method.of.application.4), sep = ";")),
	    pesticide_amount1 = r10$Rate.of.application..g.or.ml.Plot.,
	    pesticide_amount2 = r10$Rate.of.application..g.or.ml.Plot.,
	    pesticide_amount3 = r10$Rate.of.application..g.or.ml.Plot..1,
	    pesticide_amount4 = r10$Rate.of.application..g.or.ml.Plot..2,
	    pesticide_amount5 = r10$Rate.of.application..g.or.ml.Plot..3,
	    #r10$`No. of labourers/plot`,
	    #r10$`Time taken/plot (minutes)`,
	    pesticide_price1 = r10$Farm.gate.price.of.product.container,
	    pesticide_price2 = r10$Farm.gate.price.of.product.container.1,
	    pesticide_price3 = r10$Farm.gate.price.of.product.container.2,
	    pesticide_price4 = r10$Farm.gate.price.of.product.container.3,
	    pesticide_price5 = r10$Farm.gate.price.of.product.container.4,
	    Csize1 = r10$Container.size..g.or.ml. ,
	    Csize2 = r10$Container.size..g.or.ml..1 ,
	    Csize3 = r10$Container.size..g.or.ml..2 ,
	    Csize4 = r10$Container.size..g.or.ml..3 ,
	    Csize5 = r10$Container.size..g.or.ml..4 ,
	    farm_nm = r10$Farmer.s.name
	  )
	  
	  d6 <- d6[!is.na(d6$crop),]
	  
	  d6$pesticide_price2 <- (as.numeric(d6$pesticide_price2)/as.numeric(d6$Csize2))*1000 ## price/kg 
	  d6$pesticide_price1 <- (as.numeric(d6$pesticide_price1)/as.numeric(d6$Csize1))*1000 ## price/kg 
	  d6$pesticide_price3 <- (as.numeric(d6$pesticide_price1)/as.numeric(d6$Csize3))*1000 ## price/kg
	  d6$pesticide_price4 <- (as.numeric(d6$pesticide_price1)/as.numeric(d6$Csize4))*1000 ## price/kg
	  d6$pesticide_price5 <- (as.numeric(d6$pesticide_price1)/as.numeric(d6$Csize5))*1000 ## price/kg
	  d6$herbicide_amount <- rowSums(apply(d6[, paste0("pesticide_amount", 1:5)], 2, as.numeric),na.rm = TRUE)/1000 #kg/ha
	  d6$herbicide_price <- rowSums(apply(d6[, paste0("pesticide_price",1:5)], 2, as.numeric), na.rm = TRUE)
	  d6$herbicide_dates <- apply(d6[, paste0("pesticide_date", 1:5)], 1, function(x) {
	    x <- suppressWarnings(as.numeric(x))
	    x <- as.Date(x, origin = "1899-12-30")
	    paste(format(na.omit(x), "%Y-%m-%d"), collapse = ";")
	  })
	  d6$herbicide_product = gsub("NA;", "", d6$herbicide_product)
	  
	  d6 <- d6[, !names(d6) %in% c(paste0("pesticide_amount", 1:5), paste0("pesticide_price", 1:5), paste0("pesticide_date", 1:5), paste0("Csize", 1:5))] 
	  
	  ### merge d and d6
	  d6 <- unique(d6[which(d6$year!="0"),])
	  d <- merge(d, d6, by =intersect(names(d), names(d6)), all = TRUE)
	  
	  
	  ####
	  if(grepl("2016-LT-Giddha|2016-LT-Phulgama|2016-LT-Ragunathpur|2016-LT-Sinurjoda|2017-LT-Giddha|2017-LT-Phulgama|2017-LT-Sinurjoda", f)){r11 <- carobiner::read.excel(f, sheet = "11.Harvest operation", skip= 4) 
	  } else if (grepl("2016-LT-Lalgadh", f)){r11 <- carobiner::read.excel(f, sheet = "11.Harvest operations", skip= 4)
	  } else {r11 <- carobiner::read.excel(f, sheet = "11 - Harvest operations", skip= 4)}
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
	    harvest_cost = r11$Operation.cost.for.thressher..NRP.ha.
	  ) 
	  
	  ####
	  ### merge d and d6
	  d7 <- d7[which(d7$year!="0"),]
	  agg <- aggregate(. ~ year+ season + trial_id + cropping_system + location+ farm_nm+ treatment + plot_area ,d7, function(X) mean(X) )
	  d <- merge(d, agg, by=intersect(names(d), names(agg)), all = TRUE)
	  
	  if (grepl("2016-LT-Giddha|2016-LT-Phulgama|2016-LT-Ragunathpur|2016-LT-Sinurjoda|2017-LT-Giddha|2017-LT-Phulgama|2017-LT-Sinurjoda", f)) {r14 <- carobiner::read.excel(f, sheet="13.Grain harvest")
	  } else if (grepl("2016-LT-Lalgadh", f)){r14 <- carobiner::read.excel(f, sheet="14.Grain harvest")} else {r14 <- carobiner::read.excel(f, sheet="14 - Grain Harvest ")}
	  hdr <- rbind(r14[4, ], r14[3, ])
	  names(r14) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r14 <- r14[-c(1:4),]
	  names(r14) <- make.names(names(r14), unique = TRUE)
	  names(r14) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r14))
	  names(r14) <- gsub("Grain.yield..t.ha.|Sun.dry.grain.yield..t.ha.|Sundry.grain.yield..t.ha", "grain.yield.t.ha.", names(r14))
	  names(r14) <- gsub("Trial.Code", "Trial.code", names(r14))
	  names(r14) <- gsub("Harvest.Index..HI.", "HI", names(r14))
	  names(r14) <- gsub("Biomass.t.ha.", "Biomass..t.ha", names(r14))
	  d8 <- data.frame(
	    year = as.character(r14$Year),
	    season = r14$Season,
	    trial_id = gsub("0", NA, as.character(r14$Trial.code)),
	    cropping_system = gsub("Note:", NA, r14$Cropping.System),
	    location = carobiner::fix_name(r14$Node, "title"),
	    #site = as.character(r14$Site.No.),
	    treatment = r14$Tmnt,
	    plot_area = as.numeric(r14$Plot.size..m2.),
	    yield = as.numeric(r14$grain.yield.t.ha.)*1000,
	    fwy_total =  as.numeric(r14$Biomass..t.ha)*1000,
	    harvest_index =  as.numeric(r14$HI),
	    farm_nm = r14$Farmer.s.name
	  )
	  
	  d8 <- d8[!is.na(d8$year),]
	  
	  ### merge d and d8
	  d8 <- d8[which(d8$year!="0"),]
	  agg <- aggregate(. ~ year+ season + trial_id + cropping_system + location+ farm_nm+ treatment + plot_area ,d8, function(X) mean(X) )
	  d <- merge(d, agg, by= intersect(names(d), names(d8)), all = TRUE)
	  d$season <- tolower(d$season)
	  d$crop_rotation  <- ifelse(grepl("R-L", d$cropping_system), "rice;lentil",
	                             ifelse(grepl("R-W", d$cropping_system), "rice;wheat", "none")) 
	  d$year <- d$farm_nm <- d$cropping_system <- NULL
	  d$loc <- basename(f)
	  
	  d
	}	
	
	d <- lapply(ff, proc)
	d <- do.call(rbind, d)
	
	### Fixing land prep method 
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub("Tilling|Tillage|tilling|First tillage", "tillage", P)
	P <- gsub(";NA|NA;", "", P)
	P <- gsub("^tillage;tillage$|^tillage$", "conventional", P)
	P <- gsub("tillage;tillage;puddling|tillage;Puddling", "conventional;puddled", P)
	P <- gsub("tillage;tillage;puddling|tillage;tillage;Puddling", "conventional;puddled", P)
	P <- gsub("Seeding", NA, P)
	P <- gsub("puddling", "puddled", P)
	P <- gsub("Primary tillage;Secondary tillage", "conventional", P)
	P <- gsub("Primary tillage;Secondary tillage", "conventional", P)
	P <- gsub("tillage;conventional", "conventional", P)
	P <- gsub("Primary tillage;Secondary conventional", "conventional", P)
	d$land_prep_method <- P
	d$land_prep_method <- ifelse(is.na(d$land_prep_method) &grepl("UPTPR", d$treatment), "not puddled",
	                             ifelse(is.na(d$land_prep_method) &grepl("CTTPR", d$treatment), "conventional", 
	                             ifelse( is.na(d$land_prep_method) & grepl("ZT", d$treatment), "none", d$land_prep_method)))
	
	d$soil_texture <- gsub("sandy", "sand", d$soil_texture)
	#### Fixing planting method 
	d$planting_method <- "transplanting"
  d$planting_method <- ifelse(is.na(d$transplanting_date) & grepl("ZTDSR", d$treatment), "direct seeding", d$planting_method)
   
	### herbicide implement
	d$herbicide_implement <- "manual"
	d$herbicide_dates <- gsub("NA;|;NA", "", d$herbicide_dates)
	d$herbicide_dates <- gsub("", NA, d$herbicide_dates)
	d$season <- "kharif"
	d$weeding_implement <- gsub("sprayer", "manual", d$weeding_implement)
  ### fixing herbicide product
	
	P <- carobiner::fix_name(d$herbicide_product)
	P <- gsub("alphamethrin", "none", P) # insecticide alpha-cypermethrin
	P <- gsub("bispyribac;bispyribac", "bispyribac-sodium", P)
	P <- gsub("bispyribac", "bispyribac-sodium", P)
	P <- gsub("chloropyriphus", "none", P) # insecticide
	P <- gsub("glyphoshat;", "glyphosate;", P)
	P <- gsub(";NA|NA;", "", P)
	P <- gsub("bispyribac-sodium-sodium", "bispyribac-sodium", P)
	d$herbicide_product <- P
	d$herbicide_product <- ifelse(is.na(d$herbicide_product), "none", d$herbicide_product)
	
		#### Fixing lon and lat coordinate
	d$location <- gsub("Aman rice 2015-LT-|xlsx", "", ifelse(is.na(d$location)|d$location=="", d$loc, d$location))
	geo <- data.frame(
	  location = c("Fulgama", "Lalgadh", "Raghunathpur","Sinurjoda", "Giddha", "Raghunathpur-Dhanusha.", "Sinurjoda-Dhanusha."),
	  long = c(85.905, 85.9111, 86.1190,  85.917, 86.0657, 86.101, 85.9182),
	  lat = c(26.639, 26.982, 26.822, 26.791, 26.699, 26.824, 26.791)
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$long[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$long <- d$lat <- d$loc <- NULL
	
	d$on_farm <- TRUE 
	d$crop <- "rice"
	d$is_survey <- FALSE 
	d$yield_part <- "grain" 
	d$country <- "Nepal" 
	d$yield_moisture <- NA_real_
	d$geo_from_source <- FALSE
	d$irrigated <- NA
	d$yield_isfresh <- TRUE
	
	d <- unique(d)
	
	#### create a long format for fertilizer
	d$record_id <- as.integer(1: nrow(d))
	i <- grepl("^(N_|P_|K_)|date[1-8]$|amount[1-8]$|price[1-8]$|^record_id$",names(d))
	Nm <- names(d)[i]
	
	fert <- d[, Nm]
	vars <- paste0(rep(gsub("#", "fertilizer", c("N_#", "P_#", "K_#", "#_amount", "#_price")), each=8))
	cols <- paste0(vars, rep(1:8, 5))
	date <- rep(c(paste0("fertilizer_date", 1:8)), 5)
	fert_long <- reshape( fert, varying = list (cols, date), v.names = c("value", "date"), direction = "long")
	fert_long$variable <- vars[fert_long$time]
	fert_long <- fert_long[!is.na(fert_long$value),]
	fert_long$time <- fert_long$id <- NULL
	i <- grepl("^(N_|P_|K_)|date[1-8]$|amount[1-8]$|price[1-8]$",names(d))
	Nm1 <- names(d)[i]
  d <- d[, !names(d) %in% Nm1]

	carobiner::write_files(path, meta, d, long= fert_long)
}


