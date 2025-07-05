# R script for "carob"

carob_script <- function(path) {
   
"Dataset recording the observation of different variables related to rice growth, weeds, nitrogen content in rice biomass and grains, rice yield, macrofauna and grub countings, and nematodes under 3 different rotations (one with rice followed by groundnut, one with rice followed by a cereal-legume mixture, one with rice followed by a legume mixture) and a rice monocropping during 4 years.in Malagasy highlands Climatic data (monthly) for the 4 years of the trial are also included (rainfall, temperature)."
   
	uri <-  "doi:10.18167/DVN1/XYOHRP"
	group <- "agronomy" 

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		publication= NA,
		data_organization = "CIRAD;FOFIFA;IRD",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-10-15",
		data_type="experiment",
		project=NA,
		response_vars = "yield",
		treatment_vars = "crop_rotation"		
	)
   
	r1 <- carobiner::read.excel(ff[basename(ff)=="DonneesDATAVERSE_F1.xlsx"], sheet="DataBiomassYieldN")  
	d1 <- data.frame(
		crop = "rice", 
		country = "Madagascar",
		season = as.character(r1$Season),
		crop_rotation = r1$Rotation, 
		yield = 1000 * r1$`Yield (14% moisture content)`,
		weed_biomass = 1000 * r1$TotalWeedBiomass,
		dmy_total = 1000 * r1$RiceBiomassD5,
		rep = as.integer(as.factor(r1$Block))
	)
   
	r2 <- carobiner::read.excel(ff[basename(ff)=="DonneesDATAVERSE_F1.xlsx"], sheet = "DataFertilization", fix=TRUE) 
      
	qom <- r2$Quantity.of.applied.manure.t.ha.1.of.DM  * 10
	d2 <- data.frame(
		season = r2$Season,
		OM_amount = qom / r2$pct.DM,
		N_organic = r2$N.pct * qom, 
		P_organic = r2$P.pct * qom,
		K_organic = r2$K.pct * qom,
		Ca_organic = r2$Ca.pct * qom,
		Mg_organic = r2$Mg.pct * qom
	)
	
	d <- merge(d1, d2, by="season")

	d$crop_rotation[d$crop_rotation=="RG"] <- "rice;groundnut"
	d$crop_rotation[d$crop_rotation=="RR"] <- "rice;rice"
	d$crop_rotation[d$crop_rotation=="RVC"] <- "rice;cereal;legume"
	d$crop_rotation[d$crop_rotation=="RSC"] <- "rice;cereal"

 
	d$planting_date <- paste0("20", substr(d$season, 1, 2))
	d$harvest_date <- paste0("20", substr(d$season, 3, 4))
	   
	d$location <- "Vakinankaratra"
	d$longitude <- 46.836
	d$latitude <- -19.711
	d$geo_from_source <- FALSE
	d$trial_id <- "1"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$irrigated <- FALSE
	d$is_survey <- FALSE

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

message("should also process soil and weather data")

	
	carobiner::write_files(path, meta, d)
   
}

