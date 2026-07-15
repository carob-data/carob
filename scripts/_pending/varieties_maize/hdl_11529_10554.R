# R script for "carob"

## ISSUES
# Reading excel files needs to be refined a lot 
# need to skip rows in begging and end 

carob_script <- function(path) {
  
"
Summary results and individual trial results from the International Late White Hybrid - ILWH,
(Elite Tropical Late White Normal and QPM Hybrid Trial - CHTTW) conducted in 2006.
"

	uri <- "hdl:11529/10554"
	group <- "varieties_maize"
	ff	<- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication= NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Mitchelle Njukuya",
		carob_date="2024-02-22"
	)
	
	get_data <- function(fname, id, country, longitude, latitude, elevation) {
		f <- ff[basename(ff) == fname]
		r <- carobiner::read.excel(f) 
		data.frame( 
			trial_id = id,
			crop = "maize",
				on_farm = TRUE,
			striga_trial = FALSE, 
			striga_infected = FALSE,
			borer_trial = FALSE,
			yield_part = "grain",
			variety=r$BreedersPedigree1,
			yield=as.numeric(r$GrainYieldTons_FieldWt)*1000,
			asi=as.numeric(r$ASI),
			plant_height=as.numeric(r$PlantHeightCm),
			e_ht = as.numeric(r$EarHeightCm),
			rlper = as.numeric(r$RootLodgingPer),
			slper = as.numeric(r$StemLodgingPer),
			husk = as.numeric(r$BadHuskCoverPer),
			e_rot = as.numeric(r$EarRotTotalPer),
			moist = as.numeric (r$GrainMoisturePer),
			plant_density = as.numeric(r$PlantStand_NumPerPlot),
			e_asp = as.numeric(r$EarAspect1_5),
			p_asp = as.numeric(r$PlantAspect1_5),
			country=country,
			longitude=longitude,
			latitude=latitude,
			elevation = elevation
		)
	}
		
	d0 <- get_data("06CHTTW1-1.xls", 1, "Mexico", -105.7, 18.9167, 1340)	
	d0$location <- "Usmajac, Jalisco"
	d0$planting_date <- "2006-06-22"
	d0$harvest_date	<- "2007-02-22"
	
	d1 <- get_data("06CHTTW8-1.xls", 2, "Ghana", -1.3667, 7.3833, 232)	
	d1$location <- "Ejura"
	d1$planting_date <- "2007-05-08"
	d1$harvest_date	<- "2007-09-12"
	
	d2 <- get_data("06CHTTW9-1.xls", 3, "Ghana", -1.5833, 6.75, 270)	
	d2$location <- "Fumesua"
	d2$planting_date <- "2007-07-04"
	d2$harvest_date	<- "2007-08-16"
	
	d3 <- get_data("06CHTTW28-1.xls", 4, "Mexico", -96.6667, 19.35, 15)	
	d3$location <- "Cotaxtla, Veracruz"
	d3$planting_date <- "2006-09-27"
	d3$harvest_date	<- "2007-01-30"
	
	d <- carobiner::bindr(d0, d1, d2, d3)
	
	carobiner::write_files(meta, d, path=path)
}
