# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"International Intermediate White Hybrid Trial - IIWH0730 Summary results and individual trial results from the International Intermediate White Hybrid - IIWH, (Elite Subtropical Late White Normal and QPM Hybrid Trial, Basically Single Crosses Trial - CHTSW) conducted in 2007."

	uri <- "hdl:11529/10542"
	group <- "varieties_maize"

	ff  <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International intermediate white hybrid trials",
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_effort = NA,
		carob_date = "2024-10-09",
		design = NA
	)

	f <- ff[basename(ff) == "07CHTSW-Locations.xls"]
	rlocs <- carobiner::read.excel.hdr(f, skip=9, hdr=2) 
	rlocs <- rlocs[-1, ]
	names(rlocs) <- c("ID", names(rlocs)[-ncol(rlocs)])
	
	locs <- data.frame(
		trial_id = as.character(rlocs$ID),
		latitude = as.numeric(gsub("o", "", rlocs$Latitude_Latitud)) + 
						as.numeric(gsub("'", "", rlocs$X)) / 60, 
		longitude = (as.numeric(gsub("o", "", rlocs$Longitude_Longitud)) + 
						as.numeric(gsub("'", "", rlocs$X.2)) / 60 ) * 
						ifelse(rlocs$`X.3` == "W", -1, 1),
		country = rlocs$Country_País,
		location = rlocs$Location_Localidad,
		elevation = rlocs$Masl_Altitud_msnm,
		planting_date = as.character(rlocs$Date_Fecha.de_Siembra),
		harvest_date = as.character(rlocs$Date_Fecha.de_Cosecha)
	)
	locs$country <- gsub("México", "Mexico", locs$country)
	s <- sapply(strsplit(locs$location, ", "), \(i) i[1:2]) |> t()
	locs$location <- s[,1]
	locs$adm1 <- s[,2]
	locs$adm1 <- gsub("Jal.", "Jalisco", s[,2])
	locs$geo_from_source <- TRUE
	i <- locs$location == "Usmajac"
	locs$latitude[i] <- 19.875
	locs$longitude[i] <- -103.5987
	locs$geo_from_source[i] <- FALSE
	
	get_data <- function(fname, id, cols=2:35) {
		f <- ff[basename(ff) == fname]
		r <- carobiner::read.excel(f) 
		r <-r[22:33, cols]

		x <- data.frame( 
		  trial_id = as.character(id),
		  variety_code = as.character(r$Name),
		  variety_pedigree=r$BreedersPedigree1,
		  asi=as.numeric(r$ASI),
		  plant_height=as.numeric(r$PlantHeightCm),
		  ear_height = as.numeric(r$EarHeightCm),
		  rlper = as.numeric(r$RootLodgingPer),
		  slper = as.numeric(r$StemLodgingPer),
		  husk = as.numeric(r$BadHuskCoverPer),
		  e_rot = as.numeric(r$EarRotTotalPer),
		  moist = as.numeric (r$GrainMoisturePer),
#		  plant_density = as.numeric(r$PlantStand_NumPerPlot),
		  e_asp = as.numeric(r$EarAspect1_5),
		  p_asp = as.numeric(r$PlantAspect1_5),
		  gls = r$GrayLeafSpot1_5,
		  rust = r$CommonRust1_5,
		  blight = r$LeafBlightTurcicum1_5
		)
		
		x$yield <- if (fname == "07CHTSW11-1.xls") {
			as.numeric(r$GrainYieldTons_GrainWt) * 1000
		} else {
			as.numeric(r$GrainYieldTons_FieldWt) * 1000
		}
		x
	}
	
	d1 <- get_data("07CHTSW1-1.xls", 1)
	d2 <- get_data("07CHTSW9-1.xls", 2) 
	d3 <- get_data("07CHTSW10-1.xls",3)
	d4 <- get_data("07CHTSW11-1.xls", 4, 2:46)  
	d5 <- get_data("07CHTSW24-1.xls", 5)  
	d6 <- get_data("07CHTSW26-1.xls", 6)

	dd <- rbind(d1, d2, d3, d4, d5, d6)

	d <- merge(dd, locs, by="trial_id")

	d$crop = "maize"
	d$on_farm = TRUE
	d$striga_trial = FALSE 
	d$striga_infected = FALSE
	d$borer_trial = FALSE
	d$yield_part = "grain"
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$N_fertilizer <- d$P_fertilizer  <- d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}

