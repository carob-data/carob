# R script for "carob"
# license: GPL (>=3)

## ISSUES



carob_script <- function(path) {

"
1.1-Kharif (summer) rice-all nodes-Long term trail (LT)-Rajshahi-Bangladesh

Farmers' participatory researchers managed long-term trials aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives:   1.	Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear the risk and undertake technological innovation.   2.	Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for smallholders.   3.	Facilitate the widespread adoption of sustainable, resilient, and more profitable farming systems.
"

	uri <- "hdl:11529/10548071"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = "doi:10.1016/j.jclepro.2019.118982",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "crop_rotation;land_prep_method",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-14",
		carob_completion = 80,	
		carob_effort = 7
	)
	
	
	ff <- ff[grepl("nodes|LT", basename(ff))]
	
	### process
	
	proc <- function(f){
	  r3 <- carobiner::read.excel(f, sheet="2 - Site information")
	  if(grepl("2016-LT-All| 2017-LT-All", f)){hdr <- rbind(r3[4, ], r3[3, ], r3[2, ])} else { hdr <- rbind(r3[5, ], r3[3, ], r3[4, ])}
	  
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
	    soil_texture = r3$Soil.texture..sand..silt..clay.etc..,
	    #r3$`Soil problem, if any (Flood, acidic soils etc.)`,
	    latitude = as.numeric(r3$Latitude.North..Degree.and.decimal.format.only.),
	    longitude = as.numeric(r3$Longitude.East..Degree.and.decimal.format.only.),
	    farm_nm = r3$Farmer.s.name
	  )
	  
	  d1 <- d1[!is.na(d1$year),]
	  ####
	  if(grepl("2017-LT-All|2016-LT-All", f)){ r4 <- carobiner::read.excel(f, sheet="3-  Land Preparation Operations", skip=4)} else{ r4 <- carobiner::read.excel(f, sheet="3-  Land Prep Operations", skip=4)}
	  names(r4) <- make.names(names(r4), unique = TRUE)
	  names(r4) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r4))
	  names(r4) <- gsub("Trial.Code...3", "Trial.Code", names(r4))
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
	  
	  ## merge d1 and d2 
	  d <- merge(d1, d2, by=intersect(names(d1), names(d2)), all = TRUE)
	  
	  r5 <- carobiner::read.excel(f, sheet="4- Stand counts & Phenology")
	  hdr <- r5[4:3, ]
	  names(r5) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r5 <- r5[-c(1:4),] ### header
	  names(r5) <- make.names(names(r5), unique = TRUE)
	  names(r5) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r5))
	  names(r5) <- gsub("Date.of.80..physiological.maturity..dd.mm.yy.", "Date.of.80..physiological.maturity..mm.dd.yy.", names(r5))
	  names(r5) <- gsub("Datw.of.harvest..mm.dd.yy.|Datw.of.harvest..dd.mm.yy.|Date.of.harvest..mm.dd.yy.", "Date.of.harvest..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.50..first.flower..mm.dd.yy.", "Date.of.50..first.flower..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.100..plant.emergence..mm.dd.yy.", "Date.of.100..plant.emergence..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Sedding.date|Date.of.seeding..mm.dd.yy.", "Date.of.seeding..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.transplanting..mm.dd.yy.", "Date.of.transplanting..dd.mm.yy.", names(r5))
	  names(r5) <- gsub("Date.of.50..anthesis..dd.mm.yy.", "Date.of.50..anthesis..mm.dd.yy.", names(r5))
	  names(r5) <- gsub("variety", "Variety", names(r5))
	  
	  d3 <- data.frame(
	    year = as.character(r5$Year),
	    season = r5$Season,
	    trial_id = gsub("0", NA, as.character(r5$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r5$Cropping.System),
	    location = carobiner::fix_name(r5$Node, "title"),
	    country= "India",
	    #site = as.character(r5$Site.No.),
	    treatment = r5$Tmnt,
	    plot_area = as.numeric(r5$Plot.size..m2.),
	    crop = "rice",
	    variety = r5$Variety,
	    plant_height =  as.numeric(r5$Height.of.rice.stubble.in.plot..cm.),
	    planting_date = r5$Date.of.seeding..dd.mm.yy.,
	    transplanting_date = ifelse(nchar(r5$Date.of.transplanting..dd.mm.yy.>5), as.character(as.Date(r5$Date.of.transplanting..dd.mm.yy., "%m-%d-%y")), as.character(as.Date(as.numeric(r5$Date.of.transplanting..dd.mm.yy.), origin = "1899-12-30"))) ,
	    #planting_method = r5$`Row seeded/ transplanted (1) or B'cast/random (2)`,
	    row_spacing = as.numeric(gsub("Random", NA, r5$Row.spacing..cm.)),
	    seed_rate =  as.numeric(r5$Seedrate..kg.ha.),
	    seed_price = as.numeric(r5$Cost.of.seed.per.kg),
	    emergence_date = r5$Date.of.100..plant.emergence..dd.mm.yy.,
	    ## anthesis = flowering
	    flowering_date = r5$Date.of.50..anthesis..mm.dd.yy.,
	    ## first flower ... 
	    ## flowering_date = r5$Date.of.50..first.flower..dd.mm.yy.,
	    maturity_date = r5$Date.of.80..physiological.maturity..mm.dd.yy.,
	    harvest_date = r5$Date.of.harvest..dd.mm.yy.,
	    emergence_days =  as.numeric(r5$X100..emergence..DAS.),
	    flowering_days =  as.numeric(r5$X50..first.flowering..DAS.),
	    anthesis_days =  as.numeric(r5$X50..anthesis..DAS.),
	    maturity_days =  as.numeric(r5$X80..physiological.maturity..DAS.),
	    harvest_days =  as.numeric(r5$Harvesting..DAS.),
	    seed_cost =  as.numeric(r5$Seed.cost..Tk..ha.),
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
	  #d3$transplanting_date <- conv(d3$transplanting_date)
	  d3$emergence_days[which(d3$emergence_days <0)] <- NA
	  d3$anthesis_days[which(d3$anthesis_days <0)] <- NA
	  ### merge d and d3
	  d <- merge(d, d3, by=intersect(names(d), names(d3)), all = TRUE)
	  ####
	  
	  if(grepl("2017-LT-All|2016-LT-All", f)){r6 <- carobiner::read.excel(f, sheet="5- Crop Managment Operations", skip= 4)} else{r6 <- carobiner::read.excel(f, sheet="5- Crop Mgmt Operations", skip= 4)}
	  names(r6) <- make.names(names(r6), unique = TRUE)
	  names(r6) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r6))
	  names(r6) <- gsub("Equipment.cost..TK.ha.", "Equipment.cost..tk.ha", names(r6))
	  names(r6) <- gsub("Total.labor..Pd.ha.", "Total.labor..pd.ha", names(r6))
	  if(is.null(r6$Equipment.cost..tk.ha)) r6$Equipment.cost..tk.ha <- NA
	 
	   d4 <- data.frame(
	    year = as.character(r6$Year),
	    season = r6$Season,
	    trial_id = gsub("0", NA, as.character(r6$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r6$Cropping.System), 
	    location = carobiner::fix_name(r6$Node, "title"),
	    #site = as.character(r6$Site.No.),
	    treatment = r6$Tmnt,
	    plot_area = as.numeric(r6$Plot.size..m2.),
	    weeding_dates = gsub("weeding", NA, r6$X0peration.4.........Date..mm.dd.yy.),
	    weeding_done = grepl("WEEDING|weeding", r6$Operation.4.......Name.of.operation),
	    weeding_implement = tolower(r6$X0peration.4...Implement.used),
	    labour = r6$Total.labor..pd.ha,
	    equipment_cost = r6$Equipment.cost..tk.ha,
	    farm_nm = r6$Farmer.s.name
	    
	  )
	  
	  d4 <- d4[!is.na(d4$year),]
	  d4$weeding_dates = gsub("21-072015", "2015-07-21", d4$weeding_dates)
	  d4$weeding_dates <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", d4$weeding_dates), d4$weeding_dates,
	                      ifelse( nchar(d4$weeding_dates) > 5, as.character(as.Date(d4$weeding_dates, format = "%m-%d-%y")), as.character(as.Date(suppressWarnings(as.numeric(d4$weeding_dates)), origin = "1899-12-30"))))
	  ### merge d and d4
	  d <- merge(d, d4, by = intersect(names(d), names(d4)), all = TRUE)
	  
	  ####################
	  
	  r7 <- carobiner::read.excel(f, sheet="6 - Fertilizer amounts ")
	  hdr <- rbind(r7[4, ], r7[3, ])
	  names(r7) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  
	  r7 <- r7[-c(1:4),] ### header
	  names(r7) <- make.names(names(r7), unique = TRUE)
	  names(r7) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r7))
	  names(r7) <- gsub("Gyp$", "Gypsum..kg.ha.", names(r7))
	  names(r7) <- gsub("^N$", "N..kg.ha.", names(r7))
	  names(r7) <- gsub("^ZnSO4$", "ZnSO4..kg.ha.", names(r7))
	  names(r7) <- gsub("^K2O$", "K2O..kg.ha.", names(r7))
	  names(r7) <- gsub("^P2O5$", "P2O5..kg.ha.", names(r7))
	  names(r7) <- gsub("ZnSO4..kg.ha...kg.ha..1", "ZnSO4..kg.ha.", names(r7))
	  names(r7) <- gsub("Total.cost.for.frtilizer..BDT.ha.", "Total.fertiliser.cost..Tk.ha.", names(r7))
	  names(r7) <- gsub("Boric.acid..kg.ha.", "Boric.Acid..kg.ha.", names(r7))
	  
	  if (is.null(r7$Date.of.application...dd.mm.yy..5))  r7$Date.of.application...dd.mm.yy..5 <- NA
	  if (is.null(r7$Date.of.application...dd.mm.yy..6)) r7$Date.of.application...dd.mm.yy..6 <- NA
	  if (is.null(r7$Date.of.application...dd.mm.yy..4)) r7$Date.of.application...dd.mm.yy..4 <- NA
	  if(is.null(r7$Fert.Grade.N.5)) r7$Fert.Grade.N.5 <-  NA
	  if(is.null(r7$Fert.Grade.N.6)) r7$Fert.Grade.N.6 <-  NA
	  if(is.null(r7$Fertilizer.applied.g.plot.5)) r7$Fertilizer.applied.g.plot.5 <-  NA
	  if(is.null(r7$Fertilizer.applied.g.plot.6)) r7$Fertilizer.applied.g.plot.6 <-  NA
	  if(is.null(r7$Farm.gate.price.per.kg.5)) r7$Farm.gate.price.per.kg.5 <-  NA
	  if(is.null(r7$Farm.gate.price.per.kg.6)) r7$Farm.gate.price.per.kg.6 <-  NA
	  if(is.null(r7$Fert.Grade.P2O5.5)) r7$Fert.Grade.P2O5.5 <-  NA
	  if(is.null(r7$Fert.Grade.P2O5.6)) r7$Fert.Grade.P2O5.6 <-  NA
	  if(is.null(r7$Fert.Grade.K2O.5)) r7$Fert.Grade.K2O.5 <-  NA
	  if(is.null(r7$Fert.Grade.K2O.6)) r7$Fert.Grade.K2O.6 <-  NA
	  if(is.null(r7$Farm.gate.price.per.kg.7)) r7$Farm.gate.price.per.kg.7 <- NA
	  if(is.null(r7$Fertilizer.applied.g.plot.7)) r7$Fertilizer.applied.g.plot.7 <-  NA
	  if(is.null(r7$Boric.Acid..kg.ha.))  r7$Boric.Acid..kg.ha. <- NA  
	  
	  d5 <- data.frame(
	    year = as.character(r7$Year),
	    season = r7$Season,
	    trial_id = gsub("0", NA, as.character(r7$Trial.Code)),
	    cropping_system = gsub("Note:", NA, r7$Cropping.System),
	    location = carobiner::fix_name(r7$Node, "title"),
	    #site = as.character(r7$Site.No.),
	    treatment = r7$Tmnt,
	    plot_area = as.numeric(r7$Plot.size..m2.),
	    ##
	    fertilizer_type1 = r7$Product.used,
	    rep1 = "1",
	    N_fertilizer1 = as.numeric(r7$Fert.Grade.N) *as.numeric(r7$Total.urea..kg.ha.)/100,
	    P_fertilizer1 = as.numeric(r7$Fert.Grade.P2O5) *as.numeric(r7$Total.TSP..kg.ha.)/100,
	    K_fertilizer1 = as.numeric(r7$Fert.Grade.K2O) *as.numeric(r7$Total.MOP..kg.ha.)/100,
	    gypsum1 = 0,
	    B_fertilizer1 = 0,
	    Zn_fertilizer1 = 0,
	    fertilizer_date1 = ifelse(nchar(r7$Date.of.application...dd.mm.yy.>5), as.character(as.Date(r7$Date.of.application...dd.mm.yy., "%m-%d-%y")) , as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy.), origin = "1899-12-30"))),
	    fertilizer_amount1 = (as.numeric(r7$Fertilizer.applied.g.plot)/as.numeric(r7$Plot.size..m2.))*10, #kg/ha,
	    fertilizer_price1 = as.numeric(r7$Farm.gate.price.per.kg),
	    fertilizer_type2 = r7$Product.used.1,
	    rep2 = "2",
	    N_fertilizer2 = as.numeric(r7$Fert.Grade.N.1)*as.numeric(r7$Total.urea..kg.ha.)/100,
	    P_fertilizer2 = as.numeric(r7$Fert.Grade.P2O5.1)*as.numeric(r7$Total.TSP..kg.ha.)/100,
	    K_fertilizer2 = as.numeric(r7$Fert.Grade.K2O.1)*as.numeric(r7$Total.MOP..kg.ha.)/100,
	    gypsum2 = 0,
	    B_fertilizer2 = 0,
	    Zn_fertilizer2 = 0,
	    fertilizer_date2 = ifelse(nchar(r7$Date.of.application...dd.mm.yy.>5), as.character(as.Date(r7$Date.of.application...dd.mm.yy., "%m-%d-%y")) , as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy.), origin = "1899-12-30"))),
	    fertilizer_amount2 = (as.numeric(r7$Fertilizer.applied.g.plot.1)/as.numeric(r7$Plot.size..m2.))*10,
	    fertilizer_price2 = as.numeric(r7$Farm.gate.price.per.kg.1),
	    fertilizer_type3 = r7$Product.used.2,
	    rep3 = "3",
	    N_fertilizer3 = as.numeric(r7$Fert.Grade.N.2)*as.numeric(r7$Total.urea..kg.ha.)/100,
	    P_fertilizer3 = as.numeric(r7$Fert.Grade.P2O5.2)*as.numeric(r7$Total.TSP..kg.ha.)/100,
	    K_fertilizer3 = as.numeric(r7$Fert.Grade.K2O.2)*as.numeric(r7$Total.MOP..kg.ha.)/100,
	    gypsum3 = 0,
	    B_fertilizer3 = 0,
	    Zn_fertilizer3 = 0,
	    fertilizer_date3 =  ifelse(nchar(r7$Date.of.application...dd.mm.yy..1>5), as.character(as.Date(r7$Date.of.application...dd.mm.yy..1, "%m-%d-%y")) , as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..1), origin = "1899-12-30"))),
	    fertilizer_amount3 = (as.numeric(r7$Fertilizer.applied.g.plot.2)/as.numeric(r7$Plot.size..m2.))*10,
	    fertilizer_price3 = as.numeric(r7$Farm.gate.price.per.kg.2),
	    fertilizer_type4 = tolower(r7$Product.used.3),
	    rep4 = "4",
	    N_fertilizer4 = as.numeric(r7$Fert.Grade.N.3)*as.numeric(r7$Total.urea..kg.ha.)/100,
	    P_fertilizer4 = as.numeric(r7$Fert.Grade.P2O5.3)*as.numeric(r7$Total.TSP..kg.ha.)/100,
	    K_fertilizer4 = as.numeric(r7$Fert.Grade.K2O.3)*as.numeric(r7$Total.MOP..kg.ha.)/100,
	    gypsum4 = 0,
	    B_fertilizer4 = 0,
	    Zn_fertilizer4 = 0,
	    fertilizer_date4 =  ifelse(nchar(r7$Date.of.application...dd.mm.yy..2>5), as.character(as.Date(r7$Date.of.application...dd.mm.yy..2, "%m-%d-%y")) , as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..2), origin = "1899-12-30"))),
	    fertilizer_amount4 = (as.numeric(r7$Fertilizer.applied.g.plot.3)/as.numeric(r7$Plot.size..m2.))*10,
	    fertilizer_price4 = as.numeric(r7$Farm.gate.price.per.kg.3),
	    fertilizer_type5 = tolower(r7$Product.used.4),
	    rep5 = "5",
	    N_fertilizer5 = as.numeric(r7$Fert.Grade.N.4)*as.numeric(r7$Total.urea..kg.ha.)/100,
	    P_fertilizer5 = as.numeric(r7$Fert.Grade.P2O5.4)*as.numeric(r7$Total.TSP..kg.ha.)/100,
	    K_fertilizer5 = as.numeric(r7$Fert.Grade.K2O.4)*as.numeric(r7$Total.MOP..kg.ha.)/100,
	    gypsum5 = 0,
	    B_fertilizer5 = 0,
	    Zn_fertilizer5 = 0,
	    fertilizer_date5 = ifelse(nchar(r7$Date.of.application...dd.mm.yy..3>5), as.character(as.Date(r7$Date.of.application...dd.mm.yy..3, "%m-%d-%y")) , as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..3), origin = "1899-12-30"))),
	    fertilizer_amount5 = (as.numeric(r7$Fertilizer.applied.g.plot.4)/as.numeric(r7$Plot.size..m2.))*10,
	    fertilizer_price5 = as.numeric(r7$Farm.gate.price.per.kg.4),
	    #
	    fertilizer_type6 = "gypsum",
	    rep6 = "6",
	    N_fertilizer6 = as.numeric(r7$Fert.Grade.N.5)*as.numeric(r7$Total.urea..kg.ha.)/100,
	    P_fertilizer6 = as.numeric(r7$Fert.Grade.P2O5.5)*as.numeric(r7$Total.TSP..kg.ha.)/100,
	    K_fertilizer6 = as.numeric(r7$Fert.Grade.K2O.5)*as.numeric(r7$Total.MOP..kg.ha.)/100,
	    gypsum6 = as.numeric(r7$Gypsum..kg.ha.),
	    B_fertilizer6 = 0,
	    Zn_fertilizer6 = 0,
	    fertilizer_date6 =  ifelse(nchar(r7$Date.of.application...dd.mm.yy..4 >5), as.character(as.Date(r7$Date.of.application...dd.mm.yy..4, "%m-%d-%y")) , as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..4), origin = "1899-12-30"))),
	    fertilizer_amount6 = (as.numeric(r7$Fertilizer.applied.g.plot.5)/as.numeric(r7$Plot.size..m2.))*10,
	    fertilizer_price6 = as.numeric(r7$Farm.gate.price.per.kg.5),
	    
	    fertilizer_type7 = "ZnSO4",
	    rep7 = "7",
	    N_fertilizer7 = as.numeric(r7$Fert.Grade.N.6)*as.numeric(r7$Total.urea..kg.ha.)/100,
	    P_fertilizer7 = as.numeric(r7$Fert.Grade.P2O5.6)*as.numeric(r7$Total.TSP..kg.ha.)/100,
	    K_fertilizer7 = as.numeric(r7$Fert.Grade.K2O.6)*as.numeric(r7$Total.MOP..kg.ha.)/100,
	    Zn_fertilizer7 =  as.numeric(r7$ZnSO4..kg.ha.),
	    gypsum7 = 0,
	    B_fertilizer7 = 0,
	    fertilizer_date7 =  ifelse(nchar(r7$Date.of.application...dd.mm.yy..5>5), as.character(as.Date(r7$Date.of.application...dd.mm.yy..5, "%m-%d-%y")) , as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..5), origin = "1899-12-30"))),
	    fertilizer_amount7 = (as.numeric(r7$Fertilizer.applied.g.plot.6)/as.numeric(r7$Plot.size..m2.))*10,
	    fertilizer_price7 = as.numeric(r7$Farm.gate.price.per.kg.6),
	    
	    fertilizer_type8 = "borax",
	    rep8 = "8",
	    N_fertilizer8 = 0,
	    P_fertilizer8 = 0,
	    K_fertilizer8 = 0,
	    B_fertilizer8 = as.numeric(r7$Boric.Acid..kg.ha.),
	    Zn_fertilizer8 = 0,
	    gypsum8 = 0,
	    fertilizer_price8 = as.numeric(r7$Farm.gate.price.per.kg.7),
	    fertilizer_amount8 = as.numeric(r7$Fertilizer.applied.g.plot.7),
	    fertilizer_date8 = ifelse(nchar(r7$Date.of.application...dd.mm.yy..6>5), as.character(as.Date(r7$Date.of.application...dd.mm.yy..6, "%m-%d-%y")) , as.character(as.Date(as.numeric(r7$Date.of.application...dd.mm.yy..6), origin = "1899-12-30"))),
	    
	    fertilizer_cost = as.numeric(r7$Total.fertiliser.cost..Tk.ha.),
	    farm_nm = r7$Farmer.s.name	    
	  )
	  
	  d5 <- d5[!is.na(d5$year),]
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
	  
	  r10 <- r10[-c(1:4),]
	  names(r10) <- make.names(names(r10), unique = TRUE)
	  names(r10) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r10))
	  names(r10) <- gsub("Container.size..g.or...", "Container.size..g.or.ml..", names(r10))
	  names(r10) <- gsub("Rate.of.application..g.or..ha..", "Rate.of.application..g.or.ml.ha..", names(r10))
	  names(r10) <- gsub("Rate.of.application..g.or..ha.$", "Rate.of.application..g.or.ml.ha.", names(r10))
	  names(r10) <- gsub("Container.size..g.or..$", "Container.size..g.or.ml.", names(r10))
	  names(r10) <- gsub("^Container.size..g.or.ml...$", "Container.size..g.or.ml.", names(r10))
	  names(r10) <- gsub("Container.size...ml..1", "Container.size..g.or.ml....1", names(r10))
	  names(r10) <- gsub("Rate.of.application...ml.ha.", "Rate.of.application..g.or.ml.ha.", names(r10))
	  names(r10) <- gsub("Rate.of.application..ml.ha.", "Rate.of.application..g.or.ml.ha..1", names(r10))
	  
	  if(is.null(r10$Pesticide.cost..Tk.ha.)) r10$Pesticide.cost..Tk.ha. <- NA
	  if(is.null(r10$Container.size..g.or.ml....1)) r10$Container.size..g.or.ml....1 <- NA
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
	    herbicide_product = gsub("NA;NA", NA, paste(tolower(r10$Product.applied), tolower(r10$Product.applied.1), sep = ";")),
	    herbicide_implement =  gsub("NA;NA", NA, paste(tolower(r10$Method.of.application), tolower(r10$Method.of.application.1), sep = ";")),
	    pesticide_amount1 = r10$Rate.of.application..g.or.ml.ha.,
	    #r10$`No. of labourers/plot`,
	    #r10$`Time taken/plot (minutes)`,
	    pesticide_price1 = r10$Farm.gate.price.of.product.container,
	    Csize1 = r10$Container.size..g.or.ml. ,
	    pesticide_amount2 = r10$Rate.of.application..g.or.ml.ha..1,
	    pesticide_price2 = r10$Farm.gate.price.of.product.container.1,
	    Csize2 = r10$Container.size..g.or.ml....1,
	    farm_nm = r10$Farmer.s.name,
	    herbicide_cost = as.numeric(r10$Pesticide.cost..Tk.ha.)
	  )
	
	  d6 <- d6[!is.na(d6$year),]
	  
	  d6$pesticide_price2 <- (as.numeric(d6$pesticide_price2)/as.numeric(d6$Csize2))*1000 ## price/kg 
	  d6$pesticide_price1 <- (as.numeric(d6$pesticide_price1)/as.numeric(d6$Csize1))*1000 ## price/kg 
	  d6$herbicide_amount <- rowSums(apply(d6[, c("pesticide_amount1", "pesticide_amount2")], 2, as.numeric),na.rm = TRUE)/1000 #kg/ha
	  d6$herbicide_price <- rowSums(apply(d6[, c("pesticide_price1", "pesticide_price2")], 2, as.numeric), na.rm = TRUE)
	  d6$herbicide_dates <- apply(d6[, paste0("pesticide_date", 1:2)],1,function(x) {
	        x <- sapply(x, function(z) {
	        if (is.na(z) || z == "") return(NA_character_)
	        if (nchar(z) > 5) {
	          as.character(as.Date(z, format = "%m-%d-%y"))
	        } else {
	          as.character(as.Date(as.numeric(z), origin = "1899-12-30"))
	        }
	      })
	      x <- na.omit(x)
	      if (length(x) == 0) NA_character_ else paste(x, collapse = ";")})
	  d6$herbicide_product = gsub("NA;", "", d6$herbicide_product)
	  
	  d6$pesticide_price1 <- d6$pesticide_price2 <- d6$pesticide_amount1 <- d6$pesticide_amount2 <- d6$Csize1 <- d6$Csize2 <- d6$pesticide_date1 <- d6$pesticide_date2 <- NULL
	  
	  ### merge d and d6
	  d6 <- d6[which(d6$year!="0"),]
	  d <- merge(d, d6, by=intersect(names(d), names(d6)), all = TRUE)
	  
	  
	  ####
	  r11 <- carobiner::read.excel(f, sheet = "11 - Harvest operations", skip= 4)
	  names(r11) <- make.names(names(r11), unique = TRUE)
	  names(r11) <- gsub("Cropping.systems|Cropping.system|Croppign.System", "Cropping.System", names(r11))
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
	    harvest_cost = r11$Operation.cost.for.thressher..Tk.ha.
	  ) 
	  
	  ####
	  ### merge d and d6
	  d7 <- d7[which(d7$year!="0"),]
	  d <- merge(d, d7, by=intersect(names(d), names(d7)), all = TRUE)
	  
	  #####
	  r14 <- carobiner::read.excel(f, sheet="14 - Grain Harvest ")
	  hdr <- r14[3:4, ]
	  names(r14) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r14 <- r14[-c(1:4),]
	  names(r14) <- make.names(names(r14), unique = TRUE)
	  names(r14) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r14))
	  names(r14) <- gsub("calculation|Sraw.yield..t.ha.|Grain.yield..t.ha.|Sun.dry.grain.yield..t.ha.|Sundry.grain.yield..t.ha|Sun.dry.grain.wt..t.ha.", "grain.yield.t.ha.", names(r14))
	  names(r14) <- gsub("Trial.Code", "Trial.code", names(r14))
	  
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
	    fwy_total =  as.numeric(r14$Biomass..t.ha.)*1000,
	    seed_weight = as.numeric(r14$X1000.grain.weight..g.),
	    harvest_index =  as.numeric(r14$HI),
	    farm_nm = r14$Farmer.s.name
	  )
	  
	  d8 <- d8[!is.na(d8$year),]
	  
	  ### merge d and d8
	  d8 <- d8[which(d8$year!="0"),]
	  d <- merge(d, d8, by=intersect(names(d), names(d8)), all = TRUE)
	  
	  ##################################################
	  
	  if(grepl("2016-LT|2015-LT-Baduria-Rajshahi", f)) {r15 <- carobiner::read.excel(f, sheet = "16-output farm gate price")} else {r15 <- carobiner::read.excel(f, sheet = "15-output farm gate price")}
	  hdr <- r15[3:4, ]
	  names(r15) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r15 <- r15[-c(1:4),]
	  names(r15) <- make.names(names(r15), unique = TRUE)
	  names(r15) <- gsub("Cropping.systems|Cropping.system", "Cropping.System", names(r15))
	  names(r15) <- gsub("Trial.Code", "Trial.code", names(r15))
	  
	  d9 <- data.frame(
	    year = r15$Year,
	    season = r15$Season,
	    trial_id = r15$Trial.code,
	    cropping_system = r15$Cropping.System,
	    location = r15$Node,
	    farm_nm = r15$Farmer.s.name,
	    treatment = r15$Tmnt,
	    plot_area = r15$Plot.size..m2.,
	    crop_price = as.numeric(r15$Market.sale.price.per.kg.in.local..currency)
	  )
	  
	  ### merge d and d9
	  d <- merge(d, d9, by=intersect(names(d), names(d9)), all = TRUE)
	  
	  d$season <- tolower(d$season)
	  d$crop_rotation  <- ifelse(grepl("R-W", d$cropping_system), "rice;wheat",
	                             ifelse(grepl("R-W-MB", d$cropping_system), "rice;wheat;mung bean",
	                             ifelse(grepl("R-R|Aman rice",d$cropping_system), "rice;rice", "rice;maize")))
	  d$loc <- gsub("Aman rice 2017-LT-|.xlsx|Aman rice 2016-LT-", "", basename(f))
	  d <- d[!is.na(d$rep1),]
	  d$year <- d$farm_nm <- d$cropping_system <- NULL
	  
	  
	  d
	}	
	
	d <- lapply(ff, proc)
	d <- do.call(rbind, d)
	
	#### Fixing planting method 
	d$planting_method <- "transplanting"
	d$planting_method = ifelse(grepl("DSR", d$treatment), "direct seeding", d$planting_method)
  d$season <- gsub("kahrif", "kharif", tolower(d$season)) 
	### Fixing land_prep
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub("2 tillage;tillage|3 tillage;Leveling", "conventional", P)
	P <- gsub("Strip tillage;NA", "strip tillage", P)
	P <- gsub("tillage;tillage \\& leveling", "conventional", P)
	P <- gsub("NA;NA|^\\;NA", NA, P)
	d$land_prep_method <- P
	d$land_prep_method[d$land_prep_method=="\\;NA"] <- NA
	d$land_prep_method <- ifelse(is.na(d$land_prep_method) &grepl("UPTPR", d$treatment), "not puddled",
	                             ifelse(is.na(d$land_prep_method) &grepl("CTTPR", d$treatment), "conventional", d$land_prep_method))
	d$land_prep_method[is.na(d$land_prep_method)] <- "none"
	
	### herbicide implement
	P <- carobiner::fix_name(d$herbicide_implement)
	P <- gsub("spray;spray|NA;spray", "manure spreader", P)
	P <- gsub("hand|spray", "manure spreader", P)
	d$herbicide_implement <- P
	d$herbicide_dates <- gsub("NA;|;NA", "", d$herbicide_dates)
	d$weeding_implement <- gsub("hand", "manual", d$weeding_implement)
	d$weeding_implement <- gsub("spray", "manure spreader", d$weeding_implement)
	
	P <- carobiner::fix_name(d$herbicide_product)
	P <- gsub("round up", "glyphosate", P)
	P <- gsub("rifit", "pretilachlor", P)
	P <- gsub(";karate|;virtako", "", P) # insecticide
	d$herbicide_product <- P
	
	### soil texture 
	d$soil_texture <- tolower(d$soil_texture)
	
	#### Fixing Lon and lat coordinate
	
	d$location[d$location==""] <- NA
	d$location <- ifelse(is.na(d$location), d$loc, d$location)
	d$loc <- NULL
	geo <- data.frame(
	  location = c("Dharampur", "Baduria", "Laxmipur", "Nabinagar", "Bijoy Nagar", "Dharampur-Rajshahi"),
	  long = c(88.220, 88.717, 90.830, 90.965, 91.228, 88.599),
	  lat = c(24.955, 24.343, 22.9451, 23.889, 24.015, 24.378)
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$long[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$long <- d$lat <- NULL
	
	d$on_farm <- TRUE 
	d$currency <- "BDT"
	d$is_survey <- FALSE 
	d$yield_part <- "grain" 
	d$country <- "Bangladesh" 
	d$yield_moisture <- NA_real_
	d$geo_from_source <- FALSE
	d$irrigated <- NA
	d$yield_isfresh <- TRUE
	
	d <- unique(d)
	#### create a long format for fertilizer
	d$record_id <- as.integer(1: nrow(d))
	i <- grepl("^(N_|P_|K_|Zn_|B_)|date[1-8]$|amount[1-8]$|price[1-8]$|^record_id$|^rep|gypsum|type",names(d))
	Nm <- names(d)[i]
	
	fert <- d[, Nm]
	vars <- paste0(rep(gsub("#", "fertilizer", c("N_#", "P_#", "K_#","Zn_#","B_#" ,"#_amount", "#_price", "gypsum", "#_type")),  each=8))
	cols <- paste0(vars, rep(1:8, 8))
	date <- rep(paste0("fertilizer_date", 1:8), 9)
	reps <- rep(paste0("rep", 1:8), 9)
	fert_long <- reshape( fert, varying = list (cols, date, reps), v.names = c("value", "date", "rep"), direction = "long")
	fert_long$variable <- vars[fert_long$time]
	fert_long <- fert_long[!(is.na(fert_long$value) & is.na(fert_long$date)),]
	fert_long$time <- fert_long$id <- NULL
	names(fert_long)[names(fert_long) == "rep"] <- "order"

	fw <- reshape(fert_long, timevar="variable", idvar=c("record_id", "order", "date"), direction="wide")
	names(fw) <- gsub("^value\\.", "", names(fw))
	fw <- fw[order(fw$record_id, fw$order), ]
	fw$fertilizer_type <- gsub("Muriate", "KCl", fw$fertilizer_type)
	fw$fertilizer_type <- gsub("Urea", "urea", fw$fertilizer_type)
	### type 
	cols <- c("N_fertilizer", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "B_fertilizer", "gypsum", "fertilizer_amount", "fertilizer_price")
	fw[cols] <- lapply(fw[cols], as.numeric)
	
	i <- grepl("^(N_|P_|K_|Zn_|B_)|date[1-8]$|amount[1-8]$|price[1-8]$|^rep|gypsum|type[1-8]",names(d))
	Nm1 <- names(d)[i]
	d <- d[, !names(d) %in% Nm1]
	
	carobiner::write_files(path, meta, d, long=fw)
}

