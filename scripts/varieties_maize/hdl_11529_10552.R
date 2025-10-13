# R script for "carob"


carob_script <- function(path) {
  
  "Summary results and individual trial results from the International Early White Variety - IEWV, (Tropical Early White Variety Trial - EVT14B) conducted in 2006."
  
  uri <- "hdl:11529/10552"
  group <- "varieties_maize"
  
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                  data_organization = "CIMMYT",
                                  publication = NA,
                                  project = "Global Maize Program",
                                  carob_date = "2025-10-13",
                                  design = NA,
                                  data_type = "experiment",
                                  treatment_vars = "variety;longitude;latitude",
                                  response_vars = "yield", 
                                  carob_contributor = "Mitchelle Njukuya",
                                  completion = 100,	
                                  notes = NA
                                  )
  
  get_data <- function(fname, id, longitude, latitude, elevation, plot_area) {
    
    f <- ff[basename(ff) == fname]
    r <- carobiner::read.excel(f)
    r <- r[22:33, ]
    
    d <- data.frame( 
      trial_id = as.character(id),
      yield_part = "grain",
      variety = r$BreedersPedigree1,
      yield = as.numeric(r$GrainYieldTons_FieldWt) * 1000,
      asi = as.numeric(r$ASI),
      plant_height = as.numeric(r$PlantHeightCm),
      ear_height = as.numeric(r$EarHeightCm),
      rlper = as.numeric(r$RootLodgingPer),
      slper = as.numeric(r$StemLodgingPer),
      husk = as.numeric(r$BadHuskCoverPer),
      e_rot = as.numeric(r$EarRotTotalPer),
      moist = as.numeric(r$GrainMoisturePer),
      
      #Flexible plant density calculation
      plant_density = as.numeric(r$PlantStand_NumPerPlot) / plot_area * 10000,
      
      e_asp = as.numeric(r$EarAspect1_5),
      p_asp = as.numeric(r$PlantAspect1_5),
      planting_date = r$PlantingDate,
      harvest_date = r$HarvestDate,
      plot_area = plot_area,      # record which plot size was used
      country = r$Country,
      location = r$NameOfLocation,
      longitude = longitude,
      latitude = latitude,
      elevation = elevation
    )
  }
  
  d0 <- get_data("06EVT14B7-1.xls", 1, -83.5333, 9.0833, 615, 7.50)
  d1 <- get_data("06EVT14B10-1.xls", 2, -95.5667, 18.1, 65, 7.88)
  d2 <- get_data("06EVT14B11-1.xls", 3, -93.6667, 17.7, 25, 8.40)
  d3 <- get_data("06EVT14B13-1.xls", 4, -63.1333, -17.7, 398, 7.80)
  d4 <- get_data("06EVT14B14-1.xls", 5, -63.1333, -17.7, 398, 7.80)
  d5 <- get_data("06EVT14B27-1.xls", 6, -96.6667, 19.35, 15, 8.32)
  
  d0$harvest_date <- "2007-01-31"
  d1$harvest_date <- "2006-11-07"
  d2$harvest_date <- "2006-11-20"
  d3$harvest_date <- "2007-04-12"
  d4$harvest_date <- "2007-03-18"
  d5$harvest_date <- "2007-01-30"
  
  d <- carobiner::bindr(d0, d1, d2, d3, d4, d5)
  
  d$crop <- "maize"
  d$on_farm <- TRUE
  d$striga_trial <- FALSE
  d$striga_infected <- FALSE
  d$borer_trial <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$geo_from_source <- FALSE
  d$yield_moisture <- as.numeric(NA)
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  d$planting_date <- as.character(d$planting_date)
  d$harvest_date <- as.character(d$harvest_date)
  d$location <- tolower(d$location)
  #meta$design <- as.character(meta$design)
  #meta$notes <- as.character(meta$notes)
  
  carobiner::write_files(meta, d, path=path)
}


