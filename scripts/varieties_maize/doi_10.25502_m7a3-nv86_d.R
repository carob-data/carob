# R script for "carob"


# need to capture the variation in irrigation and in striga treatment

carob_script <- function(path) {

"The GGE biplot tool has potential for determining combining ability effects, identifying distinct heterotic groups and efficient testers in a line × tester study. However, its use for such analysis has not been adequately explored. The objectives of this study were to (i) assess combining ability of extra-early maturing lines (80–85 days to physiological maturity) and testers for grain yield (ii) classify lines into heterotic groups and (iii) identify most efficient testers using GGE biplot. Sixty-three lines crossed to four testers were evaluated under Strga-infested, drought and nonstress environments for 2 years in Nigeria. Results of GGE biplot analyses of combining ability and heterotic patterns of yield of lines, grouping and identification of testers were close to those of the conventional line × tester method. Testers TZEEI 13, TZEEI 21 and TZEEI 29 were highly efficient in grouping lines under stress environments while testers TZEEI 21 and TZEEI 29 were best under nonstress environments. The GGE biplot identified tester TZEEI 13, TZEEI 21 and TZEEI 29 as most efficient across stress environments and TZEEI 21 and TZEEI 29 across nonstress environments."


	uri <- "doi:10.25502/m7a3-nv86/d"
	group <- "varieties_maize"
	ff  <- carobiner::get_data(uri, path, group)
 
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1556/0806.47.2019.25",
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-10-29",
		notes = NA, 
		design = "16×9 lattice"
	)
	
	sort_data <- function(fname, treatment, nrows=-1, skip=0) {
	  
	  f <- ff[basename(ff) == fname]
      r <- read.csv(f, nrows=nrows, skip=skip)
	  colnames(r) <- gsub("RLLNPERCT|RLHNPERCT","RLPERCT",colnames(r))
	  colnames(r) <- gsub("SLLNPERCT|SLHNPERCT","SLPERCT",colnames(r))
	  colnames(r) <- gsub("HUSKLN|HUSKHN","HC",colnames(r))
	  colnames(r) <- gsub("LN$|HN$","", colnames(r))
	  
	  data.frame(
		treatment = as.character(treatment),
		planting_date = r$YEAR,
		location = r$LOC,
		rep = as.integer(r$Rep),
		variety = r$Pedigree,
		anthesis_days = as.numeric(r$POLLEN),
		silking_days = as.numeric(r$DYSK),
		asi = as.numeric(r$ASI),
		plant_height = as.numeric(r$PLHT),
		ear_height = as.numeric(r$EHT),
		rlper = r$RLPERCT,
		slper = r$SLPERCT,
		husk = r$HC,
		p_asp = r$PASP,
		e_asp = r$EASP,
		e_rot = r$EROT,
		p_harv = r$PHARV,
		e_harv = r$EHARV,
		moist = r$MOIST,
		yield = as.numeric(r$YIELD)
	  )
	}

	d1 <- sort_data(tolower("Drought.csv"), "drought_stress")
	d2 <- sort_data(tolower("Low-N.csv"), "low_N")
	d3 <- sort_data(tolower("Opt.csv"), "high_N", nrows=1536)
	d4 <- sort_data(tolower("Opt.csv"), "high_N", skip=1538)  

	d <- rbind(d1, d2, d3, d4)
	
	d <- d[d$location != "LOC", ]
	d <- d[d$location != "", ]
	
	#site data (see publication)
	d$location <- gsub("IK|IKDS","Ikenne",d$location)
	d$location <- gsub("BG","Bagauda",d$location)
	d$location <- gsub("MO","Mokwa",d$location)
	d$location <- gsub("IFE","Ile-Ife",d$location)
	
	d$country <- "Nigeria"
	
	d$longitude[d$location=="Ikenne"] <- 3.1167
	d$latitude[d$location=="Ikenne"] <- 7.45
	d$longitude[d$location=="Bagauda"] <- 8.3667
	d$latitude[d$location=="Bagauda"] <- 12
	d$longitude[d$location=="Mokwa"] <- 7.3333
	d$latitude[d$location=="Mokwa"] <- 9.2667
	d$longitude[d$location=="Ile-Ife"] <- 4.55
	d$latitude[d$location=="Ile-Ife"] <- 7.4667
	d$geo_from_source <- FALSE
	
	d$elevation[d$location=="Ikenne"] <- 1500
	d$elevation[d$location=="Bagauda"] <- 580
	d$elevation[d$location=="Mokwa"] <- 300
	d$elevation[d$location=="Ile-Ife"] <- 244

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$borer_trial <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	
	#fertilizer regimes (see publication)
	d$fertilizer_type <- "NPK;urea"
	d$N_fertilizer <-ifelse(d$treatment=="high_N", 120, 30)
	d$P_fertilizer <-ifelse(d$treatment=="high_N", 60, 30)
	d$K_fertilizer <-ifelse(d$treatment=="high_N", 60, 30)
	
	#weed management (see publication)
	d$herbicide_used <- ifelse(d$treatment=="high_N", TRUE, FALSE)
	d$herbicide_product <- ifelse(d$treatment=="high_N", "primextra;paraquat dichloride", NA)
	d$herbicide_amount <- ifelse(d$treatment=="high_N", 5, 0)
	
	d$harvest_date <- as.character(d$planting_date + 1)
	d$planting_date <- as.character(d$planting_date)
	 
	d$crop <- "maize"
	d$yield_part <- "grain"
	
	d$asi[d$asi < -7] <- NA
	d$asi[d$asi > 30] <- NA
	d$silking_days[d$silking_days < 30] <- NA
	d$silking_days[d$silking_days > 125] <- NA
	d$ear_height[d$ear_height < 10] <- NA
	d$plant_height[d$plant_height < 10] <- NA
	
	
	carobiner::write_files(path, meta, d)
}


