# R script for "carob"


# should also process nematodes, macrofauna, weed biomass over time.
# Code Date D1, D2 ... are not well defined

carob_script <- function(path) {
   
"Dataset recording the observation of different variables related to rice growth, weeds, nitrogen content in rice biomass and grains, rice yield, macrofauna and grub countings, and nematodes under 3 different rotations (one with rice followed by groundnut, one with rice followed by a cereal-legume mixture, one with rice followed by a legume mixture) and a rice monocropping during 4 years.in Malagasy highlands Climatic data (monthly) for the 4 years of the trial are also included (rainfall, temperature)."
   
	uri <-  "doi:10.18167/DVN1/XYOHRP"
	group <- "agronomy" 

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		publication= "doi:10.1016/j.agee.2021.107576",
		data_organization = "CIRAD;FOFIFA;IRD",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-10-15",
		data_type="experiment",
		project=NA,
		response_vars = "yield",
		treatment_vars = "crop_rotation;planting_date;N_organic;P_organic;K_organic;Ca_organic;Mg_organic",
		completion = 50,
		notes = "not processed nematodes and macrofauna"
	)

	f1 <- ff[basename(ff) == "DonneesDATAVERSE_F1.xlsx"]
   
   r1a <- carobiner::read.excel(f1, sheet="DataSol", fix=TRUE)
	r1b <- carobiner::read.excel(f1, sheet="DataFertilization", fix=TRUE)
	r1c <- carobiner::read.excel(f1, sheet="DataBiomassYieldN", fix=TRUE)
	r1d <- carobiner::read.excel(f1, sheet="DataResidus")
	#r1e <- carobiner::read.excel(f1, sheet="Nminsoil")
	#r1f <- carobiner::read.excel(f1, sheet="DataComposantes")
	r1g <- carobiner::read.excel(f1, sheet="DataClimat", fix=TRUE)
	#r1h <- carobiner::read.excel(f1, sheet="Nematodes")
	#r1i <- carobiner::read.excel(f1, sheet="Macrofaune")
	#r1j <- carobiner::read.excel(f1, sheet="VB")
	#r1k <- carobiner::read.excel(f1, sheet="Attaques")

	d1 <- data.frame(
		crop = "rice",
		variety = "NERICA 4",  # As indicated in publication
		country = "Madagascar",
		season = r1c$Season, #"rainy", # As indicated in publication --- as-is needed for merge
		crop_rotation = r1c$Rotation, 
		yield = 1000 * r1c$Yield.14pct.moisture.content,
		yield_moisture = 14,
		weed_biomass = 1000 * r1c$TotalWeedBiomass,
		dmy_total = 1000 * r1c$RiceBiomassD5,
		rep = as.integer(as.factor(r1c$Block))
	)
   
     
	qom <- r1b$Quantity.of.applied.manure.t.ha.1.of.DM  * 10
	d2 <- data.frame(
		season = r1b$Season,
		OM_amount = qom / r1b$pct.DM,
		N_organic = r1b$N.pct * qom, 
		P_organic = r1b$P.pct * qom,
		K_organic = r1b$K.pct * qom,
		Ca_organic = r1b$Ca.pct * qom,
		Mg_organic = r1b$Mg.pct * qom
	)
	
	d <- merge(d1, d2, by="season")
	
	### process residue
	d3 <- data.frame(
	   dmy_residue = 1000 *r1d$Residus,
	   season = r1d$Season,
	   rep = as.integer(as.factor(r1d$Block)),
	   crop_rotation = r1d$Rotation 
	)
 
	d3 <- d3[!duplicated(d3[, c("season", "rep", "crop_rotation")]),] 
	d <- merge(d, d3, by=c("season", "rep", "crop_rotation"), all.x = TRUE)
	
	d$crop_rotation[d$crop_rotation=="RG"] <- "rice;groundnut"
	d$crop_rotation[d$crop_rotation=="RR"] <- "rice;rice"
	d$crop_rotation[d$crop_rotation=="RVC"] <- "rice;cereal;legume"
	d$crop_rotation[d$crop_rotation=="RSC"] <- "rice;cereal"

 
	# according to the publication
	d$planting_date[d$season == 1516] <- "2015-12-02"  
	d$planting_date[d$season == 1617] <- "2016-11-23"
	d$planting_date[d$season == 1718] <- "2017-11-24"
	d$planting_date[d$season == 1819] <- "2018-11-27"
	d$harvest_date[d$season == 1516] <- as.character(as.Date("2015-12-02") + (16*7))
	d$harvest_date[d$season == 1617] <- as.character(as.Date("2016-11-23") + (17*7))
	d$harvest_date[d$season == 1718] <- as.character(as.Date("2017-11-24") + (17*7))
	d$harvest_date[d$season == 1819] <- as.character(as.Date("2018-11-27") + (17*7))
	   
	d$location <- "Ivory station" # As indicated in publication
	d$adm1 <- "Vakinankaratra"
	d$adm2 <- "Mandoto"
	d$longitude <- 46.415 # As indicated in publication
	d$latitude <- -19.555 # As indicated in publication
	d$elevation <- 930 # As indicated in publication
	d$geo_from_source <- FALSE
	d$trial_id <- as.character(d$season)
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$irrigated <- FALSE
	d$is_survey <- FALSE

	
	### Process soil data
	ds <- data.frame(
	   soil_P_method = "Olsen",
	   soil_depth = as.numeric(gsub("-", "", substr(gsub(" ", "", r1a$Soil.Layer.cm), 3, 5))),
	   soil_pH = as.numeric(stringr::str_extract(r1a$pH.H2O, "\\d+\\.?\\d*")),
	   soil_P = as.numeric(stringr::str_extract(r1a$Olsen.P.mg.kg.1, "\\d+\\.?\\d*")),
	   soil_CEC = as.numeric(stringr::str_extract(r1a$CEC.cmol.kg.1, "\\d+\\.?\\d*")),
	   soil_C_total = as.numeric(stringr::str_extract(r1a$Total.C.pct, "\\d+\\.?\\d*")),
	   soil_N = as.numeric(stringr::str_extract(r1a$N, "\\d+\\.?\\d*"))*1000,
	   #soil_Mo = as.numeric(stringr::str_extract(r1a$MO.pct, "\\d+\\.?\\d*"))*10000,
	   soil_clay = as.numeric(stringr::str_extract(r1a$Clay.pct, "\\d+\\.?\\d*")),
	   soil_silt = as.numeric(stringr::str_extract(r1a$Silt.pct, "\\d+\\.?\\d*")),
	   soil_sand = as.numeric(stringr::str_extract(r1a$Sand.pct, "\\d+\\.?\\d*"))
	)
	agg <- aggregate(. ~ soil_P_method , ds, function(x) mean(x))
	
	d <- cbind(d, agg)
	### process climate data 
	
	dw <- data.frame(
	   date = ifelse(nchar(r1g$Month) < 2, paste(r1g$Year, "-0", r1g$Month, sep = ""), paste(r1g$Year, r1g$Month, sep = "-")),
	   prec = r1g$Monthly.rainfall.mm,
	   tmax = r1g$Average.Monthly.Maximal.temperature.C,
	   tmin = r1g$Average.Montlhy.Minimal.temperature.C,
	   country = "Madagascar",
	   location = "Ivory station", # As indicated in publication
	   adm1 = "Vakinankaratra",
	   adm2 = "Mandoto",
	   longitude = 46.415, # As indicated in publication
	   latitude = -19.555, # As indicated in publication
	   elevation =  930, # As indicated in publication
	   geo_from_source = FALSE
	)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	d$season <- "wet" #rainy according to publication
	
	carobiner::write_files(path, meta, d, wth = dw)
   
}
