# R script for "carob"
# license: GPL (>=3)

#ISSUES
#1.(coordinates not on land: India) All coordinates figures were obtained from google maps at https://www.google.com/maps.

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
		completion = 100,
		carob_contributor = "Blessing Dzuda",
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
	           "Bada Brahmanamora"="Bradbrahmanmara",
	           "Sulgadia"="Salugadia",
	           "Athanagaon"="Athangaon",
	           "Napang"="Napanga",
	           "Jagannathi"="Jaganathpur",
	           "Bankisole"="Bankisul")
	
	d$location <- ifelse(d$location %in% names(fixes),fixes[d$location],d$location)
	d$location <- ifelse(is.na(d$location), d$adm2, d$location)
	
	loc <- data.frame(location = c("Narayan Pur","Todanga","Khorasahi","Chhuruni","Bishnupur","Renugan","Belpal","Chilbasa","Neulia",
	                               "Kandagadia","Adiapada","Odang","Pagadabili","Chandigaon","Bahudarada","Haridapal","Sikarghati",
	                               "Bradbrahmanmara","Palli","Sankilo","Kansapal","Telibila","Dhanpur","Sankerko","Salugadia",
	                               "Athangaon","Tikarpada","Amdubi","Napanga","Jaganathpur","Nandoor","Bankisul","Pc Pur","Gundihudi","Mayurbhanj","Cuttack","Bhadrak"),
  longitude =c(84.091093, 86.4046, 86.6119,86.6755, 87.315126, 86.834300, 86.696562,86.860901,86.25600,85.32591,85.31330,86.42149,
               86.420890,83.780747,86.6119,86.428885,86.820108,86.820344,86.799611,83.780704,86.225399,86.555185,
               86.685020,86.673398,86.681786,86.692975,86.562231,84.790848,87.235684,86.417520,85.912955,76.883175,86.90286,86.654452,86.4144,85.8801,86.5019),
	latitude=c(25.471668, 21.0021,21.0849,21.7094,23.071866,21.735784,20.634567,21.800809,20.328670,19.916783,19.886237,20.413621,
	           19.061579,21.094248,21.006898,21.163765,21.924362,21.924043,21.978779,19.062147,20.469538,22.191481,
	           21.929303,21.919038,21.842180,21.969508,21.562412,20.612054,23.904446,20.248977,20.329299,17.259292,21.779575,21.900282,21.9224,20.4711,21.0580))         
	       
	d <- merge(d,loc, by="location", all.x=TRUE)
	#d$location <- ifelse(is.na(d$location), d$adm2, d$location)
    
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
  d$planting_method <- plant_mthd[it]  
  d$land_prep_method <- land_mthd[it]  

  d$harvest_date[d$harvest_date == "2018-01-12"] <- "2018-12-12"
  d$harvest_date[d$harvest_date == "2018-07-12"] <- "2018-12-07"

  d <- unique(d)  
	carobiner::write_files(path, meta, d)
}
	 