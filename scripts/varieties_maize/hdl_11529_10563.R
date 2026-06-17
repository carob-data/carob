# R script for "carob"


carob_script <- function(path) {

"Summary results and individual trial results from the International Late Yellow Variety - ILYV, (Tropical Late Yellow Normal and QPM Synthetic Variety Trial - EVT13S) conducted in 2006"

  uri <- "hdl:11529/10563"
  group <- "varieties_maize"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
    data_organization = "CIMMYT",
    publication= NA,
    project=NA,
    data_type= "experiment",
	response_vars = "yield",
	treatment_vars = "variety;longitude;latitude",
    carob_contributor= "Mitchelle Njukuya",
    carob_effort = NA,
    carob_date="2024-03-12"
  )
  
  ##### PROCESS data records
  
  get_data <- function(fname, id,country,longitude,latitude,elevation) {
    f <- ff[basename(ff) == fname]
    r <- carobiner::read.excel(f) 
    r <-r[22:38,2:42]
    
    d <- data.frame( 
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
      ear_height = as.numeric(r$EarHeightCm),
      rlper = as.numeric(r$RootLodgingPer),
      slper = as.numeric(r$StemLodgingPer),
      husk = as.numeric(r$BadHuskCoverPer),
      e_rot = as.numeric(r$EarRotTotalPer),
      moist = as.numeric (r$GrainMoisturePer),
#      plant_density = as.numeric(r$PlantStand_NumPerPlot),
      e_asp = as.numeric(r$EarAspect1_5),
      p_asp = as.numeric(r$PlantAspect1_5),
      gls = r$GrayLeafSpot1_5,
      rust = r$CommonRust1_5,
      blight = r$LeafBlightTurcicum1_5,
      country=country,
      longitude=longitude,
      latitude=latitude,
      elevation = elevation)}
  
  d0 <- get_data("06CHTTEY10-1.xls",1,"Bolivia", -63.15, 17.7667, 398)  
  d0$location <- "El Vallecito"
  d0$planting_date <- "2006-12-06"
  d0$harvest_date  <- "2007-03-18"
  
  d1 <- get_data("06CHTTEY21-1.xls",2,"Mexico", -100.6833, 20.5333, 1771)  
  d1$location <- "Apasco el Gde"
  d1$planting_date <- "2006-05-10"
  d1$harvest_date  <- "2006-11-30"
  
  d2 <- get_data("06CHTTEY34-1.xls",3,"Mexico", -96.6667, 19.3333, 15)  
  d2$location <- "Cotaxtla Veracruz"
  d2$planting_date <- "2006-09-27"
  d2$harvest_date  <- "2007-01-30"
  
	d <- carobiner::bindr(d0, d1, d2 )
  	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

  
  carobiner::write_files(meta, d, path=path)
}





