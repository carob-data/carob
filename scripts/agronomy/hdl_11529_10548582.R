# R script for "carob"


carob_script <- function(path) {
  
"The experiment was initiated in 2008 and concluded in 2018 to evaluate the performance of durum wheat (Triticum durum L.) under conventionally tilled (CTB) and permanent beds (PB) under two sowing irrigation practices and five nitrogen (N) fertilization treatments in northwestern Mexico. It was located at the Norman E. Borlaug Experiment Station (CENEB) near Ciudad Obregón, Sonora, Mexico (lat. 27°22010″N, long. 109°55051″E, 38 masl) and had a randomized complete block design for four environments (ENV) that combined tillage and sowing irrigation practice: CTB with wet and dry sowing and PB with wet and dry sowing. The PB treatments had been under conservation agriculture for over ten years previously to the experiment. Plots were defined by N fertilizer management, with three replicates. Plots were 3 m wide (4 beds of 0.75 m width) and 10 m long, a space of 30 m2. The CTB were tilled after each crop with a disk harrow to 20 cm depth and new beds were formed. The PB were only reshaped every year in the furrow without disturbing the soil on the bed. In wet sowing, 100-120 mm irrigation was applied two-to-three weeks before sowing; in dry sowing, the field was irrigated one or two days after sowing, which provided higher soil moisture content during germination than wet sowing. Four auxiliary irrigations of 80-100 mm were applied to all plots each cycle. The N fertilizer treatments consisted of a control treatment with no N fertilizer and five treatments with different doses and divisions between first and second fertilization applied as urea. The basal N application was done on the same day as the pre-sowing irrigation, applying the fertilizer in the furrow and incorporating it through irrigation. The N application at first node was completed immediately prior to the first auxiliary irrigation. Nitrogen was applied either once (basal) or split between pre-sowing and first node (split). The data set contains daily weather data for the weather station closest to the experimental site for 2008-2018 (reference evapotranspiration, precipitation, minimum and maximum temperature), yield data (grain yield, biomass yield and straw yield for durum wheat), grain quality data (test weight and thousand kernel weight), and plant physiological data (plant stand, days from flowering to maturity, NDVI) for 2009-2018, grain and straw N data for three years, soil temperature for two years and soil moisture for one year. (2021-07-09)

Randomized complete block design for four environments (ENV) that combined tillage and sowing irrigation practice: CTB with wet and dry sowing and PB with wet and dry sowing. The PB treatments had been under conservation agriculture for over ten years previously to the experiment. Plots were defined by N fertilizer management, with three replicates.
"
  
	uri <- "hdl:11529/10548582"
	group <- "agronomy"
	ff	 <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		publication="doi:10.1016/j.fcr.2021.108310",
		data_organization = "CIMMYT;ZALF;CAU",
		carob_contributor="Effie Ochieng'",
		carob_date="2021-07-26",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "land_prep_method;N_fertilizer",
		project=NA
	)
	
	
	f <- ff[basename(ff) == "DAT-PUB-214DrySow.xlsx"]
	
	d <- carobiner::read.excel(f, sheet = "Wheat")
	d$country <- "Mexico"
	d$adm1 <- "Sonora"
	d$adm2 <- "Cajeme"
	d$trial_id <- "1"
	d$latitude <- 27.369444
	d$longitude <- -109.930833
	# EGB: Adding planting_date
	d$planting_date <- NA
	d$planting_date[d$Year == 2009] <- "2008-12-03"
	d$planting_date[d$Year == 2010] <- "2009-12-01"
	d$planting_date[d$Year == 2011] <- "2010-12-08"
	d$planting_date[d$Year == 2012] <- "2011-12-15"
	d$planting_date[d$Year == 2013] <- "2012-12-10"
	d$planting_date[d$Year == 2014] <- "2013-12-11"
	d$planting_date[d$Year == 2015] <- "2014-11-26"
	d$planting_date[d$Year == 2016] <- "2015-11-23"
	d$planting_date[d$Year == 2017] <- "2016-11-28"
	d$planting_date[d$Year == 2018] <- "2017-11-27"
	d$harvest_date <- as.character(d$Year + 1)
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$rep <- as.integer(d$REP)
	d$crop <- "wheat"
	d$yield_part <- "grain"
	
	d$BIOMASS[d$BIOMASS == "."] <- NA 
	d$dmy_total <- as.numeric(d$BIOMASS)
	d$`YIELD 12%`[d$`YIELD 12%` == "."] <- NA
	d$yield <- as.numeric(d$`YIELD 12%`)
	d$STRAW[d$STRAW == "."] <- NA
	d$fwy_residue <- as.numeric(d$STRAW)
	d$TKW[d$TKW == "."] <- NA
	d$seed_weight <- as.numeric(d$TKW)
	d$fertilizer_type <- ifelse(d$FERT == 1 , "none", "urea")
	d$N_fertilizer <- as.integer(
				ifelse(d$FERT == 1 , 0,
				ifelse(d$FERT == 2 , 120,
				ifelse(d$FERT == 3 | d$FERT == 4, 180, 240))))
	#	d$N_splits <- ifelse(d$FERT == 1 , "0 | 0",
	#			ifelse(d$FERT == 2 , "36 | 84",
	#			ifelse(d$FERT == 3, "54 | 126",
	#			ifelse(d$FERT == 4, "180 | 0",
	#			ifelse(d$FERT == 5, "72 | 168", "240 | 0")))))
	
	d$N_splits <- NA
	d$N_splits[d$FERT %in% c(2, 3, 5)] <- 2L
	d$N_splits[d$FERT %in% c(4, 6)] <- 1L
	d$P_fertilizer <- 46/2.29 # convert P2O5 to P
	d$K_fertilizer <- 0
	
	
	## RH: this is the seeding method
	## d$irrigated <- ifelse(d$IRR == 1 , "Dry seeding", "Wet seeding")
	d$irrigated <- TRUE
	d$land_prep_method <- ifelse(d$TIL == 1 , "Permanent beds", "Conventionally tilled beds")
	d$plant_density <- d$`PLANTS/m² Emerg`*10000 # Conversion m2 to ha
	
	# process file(s)
	d <- d[,c("country", "adm1", "adm2", "trial_id",
		"latitude", "longitude", "planting_date", "harvest_date",
		"on_farm", "is_survey", "rep", "crop", "dmy_total", "yield",
		"fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer",
		"fwy_residue", "seed_weight", "irrigated", "land_prep_method", "plant_density")]
	
	
	d$yield_part <- "grain"
	d$geo_from_source <- TRUE
	d <- d[!is.na(d$yield), ]
	carobiner::write_files(meta, d, path=path)
	
}

