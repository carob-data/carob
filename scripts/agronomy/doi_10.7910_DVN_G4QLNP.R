# R script for "carob"

## ISSUES
#need to find information on planting and harvesting dates from author


carob_script <- function(path) {

"This dataset was generated from the research conducted to evaluate the impact of intercropping on crop productivity and drought resistance in terms of crop yield, whole-system yield based on nutritional yields and Land Equivalent Ratio, and yield loss due to drought. The experiment was conducted at a previously established ICRAF field trial in Manyusi village, Kongwa District, Dodoma, Tanzania. The drought was imposed in the field using rainout shelters on five cropping systems with and without fertilizer application in the 2019 cropping season. The treatment factors and levels were as follows: 
cropping system (sole maize, sole pigeonpea, maize-pigeonpea, maize-gliricidia, and maize-pigeonpea-gliricidia, fertilization (fertilized, unfertilized), and water (ambient rainfall, drought). The experiment layout was a randomized split-split-plot with the main plot in a randomized complete block design with three replications. The drought was imposed beginning at maize anthesis and continued past harvests of maize and pigeonpea. Data were collected in the field on-site rainfall, air temperature and relative humidity, and photosynthetically active radiation, and in the laboratory on crop grain biomass, gravimetric water content, and soil properties using crop and soil samples from the field. Crop grain yields were calculated from laboratory measurements and used to calculate nutritional yield, land equivalent ratio, and drought resistance, vapor pressure deficit was calculated from air temperature and relative humidity, and the fraction of transmitted  photosynthetically active radiation was calculated from field-measured data."


	uri <- "doi:10.7910/DVN/G4QLNP"
	group <- "agronomy"
	
	ff  <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "ICRAF;UCD",
		publication= "doi:10.3389/fsufs.2020.562663",
		project="AfricaRISING",
		data_type= "experiment",
		carob_contributor= "Mitchelle Njukuya",
		carob_date="2024-04-09",
		response_vars= "yield",
		treatment_vars="intercrops"
	)

	#use grep because the filename has a space on linux	
	f1 <- ff[grep("006_siDom_droughtResistance.csv", basename(ff))]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "007_siteCharacterization_droughtResistance.csv"]
	r2 <- colMeans(read.csv(f2))
	rownames(r2) <- NULL
	
	d <- data.frame(
		rep=r1$Blk,
		planting_date=as.character(r1$Year),
		yield=r1$Maize.Grain.Production..t.ha. * 1000,
		pyield=r1$Pigeonpea.Grain.Production..t.ha. * 1000,
		pwyield=r1$PPWood..t.ha. * 1000,
		gyield=r1$GSWood..t.ha. * 1000,
		soil_pH = r2[["pH"]],
		soil_CEC = r2[["CEC"]],
		soil_SOC = r2[["OC"]],
		soil_P = r2[["Avl_P"]],
		soil_sand = r2[["Sand"]],
		soil_silt = r2[["Silt"]],
		soil_clay = r2[["Clay"]]
	)
  
	#excel sheet for abbreviations indicated the following system names:
	tcode <- c("M", "MP", "P", "MG", "MGP")
	tname <- c("maize","maize;pigeon pea", "pigeon pea", "maize;gliricidia", "maize;pigeon pea;gliricidia")	
	d$treatment <- paste(tname[match(r1$CrpSystm, tcode)])

	d$on_farm <- TRUE
	d$is_survey <- FALSE

  # Location information was accessed from published paper
	d$country <- "Tanzania"
	d$adm1 <- "Dodoma"
	d$adm2 <- "Kogwa"
	d$location <- "Manyusi"
	d$longitude <- 36.296
	d$latitude <- -5.5656
	d$elevation <- 1206
   d$geo_from_source <- FALSE
	d$N_fertilizer <- d$P_fertilizer <- 0
	d$N_splits[r1$Fertilizer=="Fert"] <- 2L
	d$N_fertilizer[r1$Fertilizer=="Fert"] <- 43
	d$P_fertilizer[r1$Fertilizer=="Fert"] <- 15
	d$fertilizer_type <- "none"
	d$fertilizer_type[r1$Fertilizer=="Fert"] <- "DAP;urea"

	d$plant_density[d$treatment=="maize"] <- 44444
	d$plant_density[d$treatment=="pigeon pea"] <- 44444
	d$plant_density[d$treatment=="maize;pigeon pea"] <- 88889
	d$plant_density[d$treatment=="maize;gliricidia"] <- 88889
	d$plant_density[d$treatment=="maize;pigeon pea;gliricidia"] <- 88889

	d$OM_used <- grepl("gliricidia", d$treatment)
	d$OM_amount <- 0
	d$OM_amount[d$treatment=="maize;gliricidia"] <- 2700
	d$OM_amount[d$treatment=="maize;pigeon pea;gliricidia"] <- 1600
	d$OM_type <- ifelse(d$OM_used, "foliage", "none")

	#dataset and publication has no information on planting and harvesting dates
	#d$planting_date <- as.character(as.Date(   ))
	#d$harvest_date  <- as.character(as.Date(    ))

	# variety names are from the published paper
	
	mze <- d[grepl("maize", d$treatment), ]
	mze$crop <- "maize"
	mze$yield_part <- "grain"
	mze$variety <- "Staha"
	mze$intercrops <- gsub("maize;|maize", "", mze$treatment)
	
	pea <- d[grepl("pigeon", d$treatment), ]
	pea$crop <- "pigeon pea"
	pea$yield_part <- "seed"
	pea$variety <- "ICEAP 0040"
	pea$intercrops <- gsub("pigeon pea;|;pigeon pea|pigeon pea", "", pea$treatment )
	pea$yield <- pea$pyield

	wpea <- pea
	wpea$yield_part <- "wood"
	wpea$yield <- wpea$pwyield
	wpea <- wpea[!is.na(wpea$yield), ]
	wpea <- wpea[wpea$yield > 0, ]
	
	gli <- d[grepl("gliricidia", d$treatment), ]
	gli$crop <- "gliricidia"
	gli$yield_part <- "wood"
	gli$intercrops <- gsub(";gliricidia", "", gli$treatment )
	gli$yield <- gli$gyield
	
	d <- carobiner::bindr(mze, pea, wpea, gli)	
	d$pyield <- d$pwyield <- d$gyield <- NULL
	d$treatment <- paste0(d$treatment, "_", d$fertilizer_type)
	d$intercrops[d$intercrops == ""] <- "none"
	d$trial_id <- "1"
	
	d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- FALSE
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)

}
