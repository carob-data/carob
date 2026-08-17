# R script for "carob"
# license: GPL (>=3)

## ISSUES
# "Todanga" and "Neulia" villages each occur in two different Bhadrak blocks -
# disambiguated coordinate lookup by location+block/GP (carob-data/carob#749).
# GP "Ramchandra Pur" mapped to block "Bhandari Pokhari" per official GP->
# village list (village Neulia listed there, not under any Bonth-block GP);
# small residual risk from a third-party source, max ~19 km off either way.
# Todanga/Neulia block coords from OSM (block PHC/CD centroid, GP office) - see
# geo_source column.
# 78 dry-season rows with GP/Village both NA kept, not dropped - geolocated to
# adm2 centroid via carobiner::adm_pointRadius; adm3/adm4/location left NA.

carob_script <- function(path) {

 
"Data on mechanized crop establishment methods (Direct seeding by seed drill and transplanting by machine) and rice-fallow areas suitable for short duration pulses in Odisha
  
Two types of experiments conducted in multi-location on-farm trials to evaluate the mechanized crop establishment methods (Drill-Direct Seeded Rice and Mechanical Transplanted Rice) alternative to traditional crop establishment methods (Manual transplanted rice and broadcasting followed by beushening) in three districts of Odisha over three years (2017 to 2019). Two types of experiments were also conducted to evaluated the performance of short duration pulses or oilseeds in the rice-fallow areas for increasing the cropping intensity and system productivity. The yield data were collected manually from different treatments under each experiment over three years. We also combined multi-temporal Earth Observation (EO) data from Landsat-8 Operational Land Imager (OLI) and Sentinel-1 satellite sensors from 2018 to 2021 to identify rice-fallow areas and suitable rice-fallow areas for cultivation of short duration pulses and oilseeds. (2023-08-12)"

	uri <- "hdl:11529/10548942"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI;CIMMYT;CU",
		publication = "doi:10.1016/j.fcr.2023.109078",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method;planting_method",
		response_vars = "yield", 
		carob_completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_effort = NA,
		carob_date = "2025-10-09",
		notes = NA, 
		design = NA
	)

	f <- ff[basename(ff) == "CSISA_IND_OD_Rice_Fallow_2017-19.xlsx"]
	r1 <- carobiner::read.excel(f, na="na")
	r2 <- carobiner::read.excel(f, sheet ="Dry_season_crop")

	wet <- data.frame(
		country = "India",
		adm1="Odisha",
		adm2=carobiner::fix_name(r1$District, "title"),
		adm3=carobiner::fix_name(r1$Blck, "title"),
		adm4 = NA,
		location=carobiner::fix_name(r1$Village, "title"),
		planting_date=as.character(r1$Sow_date_ymd),
		variety=r1$Var,
		treatment=r1$Treat_Desc,
		seed_rate=r1$Seed_rate,
		harvest_date=as.character(r1$Harv_date_ymd),
		yield=r1$GrYld_Tha*1000,
		crop_rotation=NA
	)
		
	dry <- data.frame(
	  country = "India",
	  adm1="Odisha",
	  adm2=carobiner::fix_name(r2$District, "title"),
	  adm3 = NA,
	  adm4=carobiner::fix_name(r2$GP, "title"), #Gram Panchayat
	  location=carobiner::fix_name(r2$Village, "title"),
	  planting_date=r2$Year,
	  variety=r2$Var,
	  treatment=r2$Tret_detail,
	  seed_rate=NA,
	  harvest_date=NA,
	  yield=r2$GrYld_Tha*1000,
	  crop_rotation=tolower(r2$Crop_Sys)
	)

	d <- rbind(wet, dry)
	d$crop <- "rice"
	d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- FALSE
	
	fixes <- c("Khirosahi"= "Khorasahi",
	           "Renugaon"="Renugan",
	           "Adia"="Adiapada",
	           "Paggad"="Pagadabili",
	           "Pagad"="Pagadabili",
	           "Sankerka"="Sankerko",
	           "Khirasahi"="Khorasahi",
	           "Bada Brahmanamora"="Badbrahmanmara",
	           "Sulgadia"="Salugadia",
	           "Athanagaon"="Athangaon",
	           "Napang"="Napanga",
	           "Jagannathi"="Jaganathpur",
	           "Bankisole"="Bankisul")
	
	d$location <- ifelse(d$location %in% names(fixes),fixes[d$location],d$location)

	# "Blck" (adm3) spelling variants for the same block, seen in the raw data
	blck_fixes <- c("Bont"="Bonth", "Bhandaripokhari"="Bhandari Pokhari",
	                "Naschintakoili"="Nischinta Koili")
	d$adm3 <- ifelse(d$adm3 %in% names(blck_fixes), blck_fixes[d$adm3], d$adm3)

	# Todanga/Neulia block-disambiguated, see ## ISSUES
	ambiguous <- d$location %in% c("Todanga", "Neulia")
	# dry-season sheet has no Blck, only GP; map Ramchandra Pur to Bhandari Pokhari
	grp <- ifelse(!is.na(d$adm3), d$adm3, d$adm4)
	grp[grp == "Ramchandra Pur"] <- "Bhandari Pokhari"
	d$location_key <- d$location
	d$location_key[ambiguous] <- paste(d$location[ambiguous], grp[ambiguous])

	loc <- data.frame(
		location_key = c("Narayan Pur", "Khorasahi", "Chhuruni", "Bishnupur", "Renugan", "Belpal", "Chilbasa", "Kandagadia", "Adiapada", "Odang", "Pagadabili", "Chandigaon", "Bahudarada", "Haridapal", "Sikarghati", "Badbrahmanmara", "Palli", "Sankilo", "Kansapal", "Telibila", "Dhanpur", "Sankerko", "Salugadia", "Athangaon", "Tikarpada", "Amdubi", "Napanga", "Jaganathpur", "Nandoor", "Bankisul", "Pc Pur", "Gundihudi",
				# block/GP-disambiguated entries, see comment above
				"Todanga Bonth", "Todanga Bhadrak", "Neulia Bhandari Pokhari", "Neulia Bhadrak"),
		longitude = c( 83.702, 86.614, 86.675, 86.828, 86.834, 86.588, 86.861, 85.326, 85.313, 86.421, 83.781, 86.579, 86.429, 86.151, 86.82, 86.801, 86.207, 86.224, 86.682, 86.686, 86.673, 86.681, 86.693, 83.526, 84.791, 86.68, 85.986, 85.685, 83.007, 86.772, 86.902, 86.654,
               # block/GP-level approximations, see ## ISSUES
               86.325, 86.498, 86.339, 86.498),
		latitude = c( 20.835, 21.084, 21.709, 21.268, 21.735, 21.625, 21.802, 19.916, 19.887, 20.413, 19.062, 21.093, 21.007, 21.163, 21.924, 21.977, 20.201, 20.469, 20.428, 21.929, 21.919, 21.842, 21.969, 20.653, 20.61, 21.949, 20.515, 19.803, 19.931, 21.994, 21.779, 21.899,
	             21.12, 21.067, 20.949, 21.067),
		# NA = undocumented legacy coords; uncertainty below ~ sqrt(area/n/pi) over
		# Bhadrak's blocks/GPs
		geo_uncertainty = c(rep(as.numeric(NA), 32), 10700, 10700, 1900, 10700),
		geo_source = c(rep(as.character(NA), 32),
	              "Bonth block, OSM PHC point 'Bonth(N)' (block-level approx.)",
	              "Bhadrak Rural block, OSM boundary centroid (block-level approx.)",
	              "Ramachandrapur GP office, OSM node 8037423899 (GP-level approx.)",
	              "Bhadrak Rural block, OSM boundary centroid (block-level approx.)")
	)

	d <- merge(d,loc, by="location_key", all.x=TRUE)
	d$location_key <- NULL

	# district-centroid fallback for unmatched rows, see ## ISSUES
	# hardcoded snapshot (3 districts used) from carobiner::adm_pointRadius("India", 2) (GADM 4.1 adm2)
	adm2_loc <- data.frame(
	    adm1 = c("Odisha", "Odisha", "Odisha"),
	    adm2 = c("Bhadrak", "Cuttack", "Mayurbhanj"),
	    longitude = c(86.6167, 85.6985, 86.4059),
	    latitude = c(20.9786, 20.4436, 21.8907),
	    geo_uncertainty = c(46171, 89463, 83588),
	    geo_source = c("GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2")
	)
	names(adm2_loc)[3:6] <- c("longitude2","latitude2","geo_uncertainty2","geo_source2")
	d <- merge(d, adm2_loc, by=c("adm1","adm2"), all.x=TRUE)
	fill <- is.na(d$longitude)
	d$longitude[fill] <- d$longitude2[fill]
	d$latitude[fill] <- d$latitude2[fill]
	d$geo_uncertainty[fill] <- d$geo_uncertainty2[fill]
	d$geo_source[fill] <- d$geo_source2[fill]
	d$longitude2 <- d$latitude2 <- d$geo_uncertainty2 <- d$geo_source2 <- NULL

	#Publication data
	d$P_fertilizer <- 40
  d$K_fertilizer <- 40
  d$N_fertilizer <- 80
  d$fertilizer_type <- "DAP;KCl;ZnSO4;urea"
  d$S_fertilizer <-  d$lime <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- 14
  d$crop_rotation <- gsub("-",";",d$crop_rotation)
  d$crop_rotation <- gsub("greengram","mung bean", d$crop_rotation)
  d$crop_rotation <- gsub("blackgram","black gram",d$crop_rotation)
  d$crop_rotation <- gsub("toria","mustard",d$crop_rotation)
  
  #renaming values in planting_method
  trt <- c("Beushening",
           "Mechanical puddled transplanted Rice (PTR-M)",
           "Dry-direct seeded rice (DSR)",
           "Manual random puddled transplanted rice (PTR-R)",
           "Manual line puddled transplanted rice (PTR-L)")
  
  plant_mthd <- c("direct seeding", # beushening is direct seeding with post-emergence tillage
           "transplanted",
           "direct seeding",
           "transplanted",
           "transplanted")

  land_mthd <- c("post-emergence tillage",
           "mechanical puddling",
           "none",
           "manual puddling",
           "line puddling")

  it <- match(d$treatment, trt)
  d$planting_method <- NA
  d$land_prep_method <- NA
  d$planting_method[!is.na(it)] <- plant_mthd[it[!is.na(it)]]  
  d$land_prep_method[!is.na(it)] <- land_mthd[it[!is.na(it)]]  

  d$harvest_date[d$harvest_date == "2018-01-12"] <- "2018-12-12"
  d$harvest_date[d$harvest_date == "2018-07-12"] <- "2018-12-07"

   d <- unique(d)  
	carobiner::write_files(path, meta, d)
}
