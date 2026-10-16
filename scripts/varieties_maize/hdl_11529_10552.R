# R script for "carob"


carob_script <- function(path) {
  
"Summary results and individual trial results from the International Early White Variety - IEWV, (Tropical Early White Variety Trial - EVT14B) conducted in 2006."
  
  uri <- "hdl:11529/10552"
  group <- "varieties_maize"
  
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		carob_effort = NA,
		carob_date = "2025-10-13",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_completion = 100,	
		notes = NA
	)
  
	get_data <- function(fname, id, longitude, latitude, elevation, plot_area, harvestdate) {
    
		f <- ff[basename(ff) == fname]
		r <- carobiner::read.excel(f)
		r <- r[22:33, ]
		
		data.frame( 
			country = r$Country,
			trial_id = as.character(id),
			yield_part = "grain",
			variety = r$BreedersPedigree1,
			yield = as.numeric(r$GrainYieldTons_FieldWt) * 1000,
			asi = as.numeric(r$ASI),
			plant_height = as.numeric(r$PlantHeightCm),
			ear_height = as.numeric(r$EarHeightCm),
			root_lodging = ifelse(is.na(as.numeric(r$RootLodgingPer)), NA_character_, paste0(as.numeric(r$RootLodgingPer), "%")),

			stalk_lodging = ifelse(is.na(as.numeric(r$StemLodgingPer)), NA_character_, paste0(as.numeric(r$StemLodgingPer), "%")),

			husk = as.character(as.numeric(r$BadHuskCoverPer)),
			e_rot = r$EarRotTotalPer,
			yield_moisture = as.numeric(r$GrainMoisturePer),
			
			#Flexible plant density calculation
			plant_density = as.numeric(r$PlantStand_NumPerPlot) / plot_area * 10000,
			
			ear_aspect = as.numeric(r$EarAspect1_5),
			aspect = as.numeric(r$PlantAspect1_5),
			gls = r$GrayLeafSpot1_5,
			rust = r$CommonRust1_5,
			blight = r$LeafBlightTurcicum1_5,
			helm = r$LeafBlightMaydis1_5,
			planting_date = as.character(r$PlantingDate),
			# harvest_date = r$HarvestDate # different formats and language
			harvest_date = harvestdate,
			plot_area = plot_area,			# record which plot size was used
			location = carobiner::fix_name(r$NameOfLocation, "title"),
			longitude = longitude,
			latitude = latitude,
			elevation = elevation
		)
	}
  
	d0 <- get_data("06EVT14B7-1.xls", 1, -83.5333, 9.0833, 615, 7.50, "2007-01-31")
	d1 <- get_data("06EVT14B10-1.xls", 2, -95.5667, 18.1, 65, 7.88, "2006-11-07")
	d2 <- get_data("06EVT14B11-1.xls", 3, -93.6667, 17.7, 25, 8.40, "2006-11-20")
	d3 <- get_data("06EVT14B13-1.xls", 4, -63.1333, -17.7, 398, 7.80, "2007-04-12")
	d4 <- get_data("06EVT14B14-1.xls", 5, -63.1333, -17.7, 398, 7.80, "2007-03-18")
	d5 <- get_data("06EVT14B27-1.xls", 6, -96.6667, 19.35, 15, 8.32, "2007-01-30")
	
	d <- carobiner::bindr(d0, d1, d2, d3, d4, d5)
	
	
	d$crop <- "maize"
	d$on_farm <- TRUE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	d$borer_trial <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- as.logical(NA)
	d$geo_from_source <- FALSE
	d$yield_isfresh <- as.logical(NA)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$ear_aspect[d$ear_aspect < 1 | d$ear_aspect > 9] <- NA
	d <- d[!is.na(d$yield), ]

	d$record_id <- as.integer(seq_len(nrow(d)))
	long <- carobiner::get_function("mz_pd_long", path, group)(d)
	d[intersect(c("e_rot", "blight", "curv", "gls", "rust", "streak", "borer", "helm",
		"str_co1", "str_co2", "str_co3", "str_rat1", "str_rat2"), names(d))] <- NULL
	carobiner::write_files(meta, d, path=path, long=long)
}


