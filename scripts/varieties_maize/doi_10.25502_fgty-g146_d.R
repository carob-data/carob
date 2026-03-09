# R script for "carob"
# license: GPL v3

carob_script <- function(path) {

"Maize (Zea mays L.), an important source of calories and nutrients in sub-Saharan Africa, is threatened by northern corn leaf blight (NCLB) caused by Exserohilum turcicum. This study examined combining ability and heterotic patterns of earlymaturing maize inbreds, gene action conditioning NCLB resistance, and performance of derived hybrids across environments and identified testers. Fifteen each of white and yellow inbreds were intercrossed using North Carolina Design II to obtain 75 hybrids per endosperm color. Hybrids plus six checks were inoculated with a virulent isolate of E. turcicum 4 wk after planting in six inoculated and three non-inoculated environments in Nigeria in 2018 and 2019. Inbreds were assigned to heterotic groups using general combining ability (GCA) of multiple traits method. Specific combining ability (SCA), GCA, and genotype × environment (G × E) interactions were significant for grain yield (GYLD), disease severity, and other traits. Northern corn leaf blight caused 46% GYLD reduction. The effects of GCA were preponderant over SCA for GYLD and NCLB severity across environments, indicating that hybridization and recurrent selection would enhance genetic gains and hybrid performance. White and yellow inbreds were placed in two and three heterotic groups, respectively. High-yielding NCLB-resistant testers identified could be used to classify other inbreds yet to be field-tested into heterotic groups. Genotypes TZEI 32 × TZEI 5 and TZEI 124 × TZEI 134 were identified as single-cross testers. Genotypes TZEI 32 × TZEI 5 and TZEI 5 × TZEI 75 (white) and TZEI 124 × TZEI 134 and TZEI 124 × TZEI 11 (yellow)were identified for on-farmtesting and possible commercialization."

	uri <- "doi:10.25502/fgty-g146/d"
	group <- "varieties_maize"


	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1002/agj2.20746",
		project = "Stress Tolerant Maize for Africa",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-09-23",
		notes = "dataset has no planting dates, harvest dates and fertilizer rates"
	)
	
	process_data <- function(file_name, id) {
	  
	  f <- ff[tolower(basename(ff)) == tolower(file_name)]
	  r <- read.csv(f) 
	  
	  d <- data.frame (
	    trial_id = as.character(id),
	    rep = r$Rep,
	    variety = r$Pedigree,
	    location = r$Loc,
	    treatment = r$Treatment,
	    yield = r$Grain_yield_ha_Kgha,
	    anthesis_days = r$Days_to_anthesis,
	    silking_days = r$DY_SK,
	    asi = r$ASI,
	    plant_height = r$PLHT,
	    ear_height = r$E_HT,
	    rlper = r$RL_PERC,
	    slper = r$SL_PERC,
	    husk = r$HC,
	    p_asp = r$PL_ASP,
	    e_asp = r$E_ASP,
	    e_rot = r$E_rot,
	    disease_severity = as.character(r$TURC6WAF),
	    e_harv = r$E_HARV,
	    p_harv = r$PHARV,
	    moist = r$MOIST,
	    curv = r$CURV
	     )
	  
	}
	
	d0 <- process_data("Ikenne_Infected.csv", 1)
	d1 <- process_data("Ikenne_Uninfected.csv", 2)
	d2 <- process_data("Ile_Ife_Infected.csv", 3)
	d3 <- process_data("Ile_Ife_Uninfected.csv", 4)
	d4 <- process_data("Zaria_Infected.csv", 5)
	d5 <- process_data("Zaria_Uninfected.csv", 6)
	
	d <- rbind(d0, d1, d2, d3, d4, d5)
	
	#adding geo data
	
	d$country <- "Nigeria"
	d$geo_from_source <- FALSE
	d$longitude[d$location=="Ikenne"] <- 3.6758
	d$latitude[d$location=="Ikenne"] <- 6.9189
	d$longitude[d$location=="Ile-Ife"] <- 4.5603
	d$latitude[d$location=="Ile-Ife"] <- 7.4822
	d$longitude[d$location=="Zaria"] <- 7.7159
	d$latitude[d$location=="Zaria"] <- 11.0819
	d$elevation[d$location=="Ikenne"] <- 235.2
	d$elevation[d$location=="Ile-ife"] <- 275
	d$elevation[d$location=="Zaria"] <- 643.65
	  
	  
	d$crop <- "maize"
	d$yield_part <- "grain"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$striga_trial <- FALSE
	d$borer_trial <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	
	d$inoculated <- ifelse(d$treatment=="Infected",TRUE,FALSE)
	d$inoculant <- ifelse(d$inoculated==TRUE,"Exserohilum turcicum",NA)
	
	#dates and fertilizer rates obtained in correspondence with Ortega-Beltran Alejando
	
	d$planting_date[d$location=="Ikenne"] <- "2019-06-13"
	d$planting_date[d$location=="Ile-Ife"] <- "2019-07-04"
	d$planting_date[d$location=="Zaria"] <- "2019-07-19"
	  
	d$harvest_date[d$location=="Ikenne"] <-"2019-09-26"
	d$harvest_date[d$location=="Ile-Ife"] <-"2019-10-16"
	d$harvest_date[d$location=="Zaria"] <-"2019-10-27"
	
	d$fertilizer_used <- TRUE
	d$fertilizer_type <- "NPK;urea"
	d$N_fertilizer <- 90
	d$N_splits <- as.integer(2)
	d$P_fertilizer <- 60
	d$K_fertilizer <- 60
 
	d$yield[d$yield < 0 ] <- NA
	d$anthesis_days[d$anthesis_days < 0] <- NA
	d$silking_days[d$silking_days < 0] <- NA

	d$asi[d$asi > 100] <- NA
	d$plant_height[d$plant_height==1] <- NA
	#d$plant_height[d$plant_height > 350] <- NA

	carobiner::write_files(path, meta, d)
}


