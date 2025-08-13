# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Dataset for: Assessment of at least 200 lines from LBHT x LTVR population in two locations under heat stress

225 genotypes were evaluated under a augmented block design (unreplicated treatments) in the two locations (Gode and Melkasa) in Ethiopia. Five controls were used: Shangi, Gudene, Belete, Wechecha and CIP301056.54 repeated five times each. The planting date was in February and the harvest was in July, 2018. The plots were constituted of 5 plants, allocated 0.3m between plants and 0.75m between rows. In both experiments the yield potential was evaluated and  66 clones were selected for yield performance under heat stress.
"

	uri <- "doi:10.21223/X56PDN"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
   	data_organization = "CIP",
		publication =NA,
		project = NA,
		carob_date = "2025-08-13",
		design ="Augmented block design",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
	f1 <- ff[basename(ff) == "12537_PTYield022018_Gode.xlsx"]
	f2 <- ff[basename(ff) == "12537_PTYield022018_Melkasa.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="Fieldbook")
	r11 <- carobiner::read.excel(f1, sheet="Minimal")
	r12 <- carobiner::read.excel(f1, sheet="Weather_data")
	
	r2 <- carobiner::read.excel(f2, sheet="Fieldbook")
	r22 <- carobiner::read.excel(f2, sheet="Minimal")
	r21 <- carobiner::read.excel(f2, sheet="Weather_data")

	### Process
	
	## location: Gode
	rr <- data.frame(rbind(r11$Value))
	names(rr) <- r11$Factor
	d1 <- data.frame(
	  plot_id= as.character(r1$Plot),
	  rep= as.integer(r1$REP),
	  variety= r1$INSTN,
	  yield= r1$TTYNA*1000,
	  yield_marketable= r1$MTYNA*1000,
	  plant_height=  r1$PLAHE_AV,
	  trial_id= rr$Trial_name,
	  crop= rr$Crop,
	  planting_date= "2018-02",
	  harvest_date= "2018-07",
	  country= rr$Country,
	  adm1= rr$Admin1,
	  adm2= rr$Admin2,
	  adm3= rr$Admin3,
	  location= rr$Locality,
	  elevation= as.numeric(rr$Elevation),
	  latitude= as.numeric(rr$Latitude),
	  longitude= as.numeric(rr$Longitude),
	  plot_area= 2.25/10000, # ha
	  row_spacing= 75,
	  plant_spacing= 30,
	  plant_density= (5/2.25)*10000
	)

	## location: Melkasa
	rr <- data.frame(rbind(r22$Value))
	names(rr) <- r22$Factor
	d2 <- data.frame(
	   plot_id= as.character(r2$Plot),
	   rep= as.integer(r2$REP),
	   variety= r2$INSTN,
	   yield= r2$TTYNA*1000,
	   yield_marketable= r2$MTYNA*1000,
	   plant_height=  r2$PLAHE_AV,
	   trial_id= rr$Trial_name,
	   crop= rr$Crop,
	   planting_date= "2018-02",
	   harvest_date= "2018-07",
	   country= rr$Country,
	   adm1= rr$Admin1,
	   adm2= rr$Admin2,
	   adm3= rr$Admin3,
	   location= rr$Locality,
	   elevation= as.numeric(rr$Elevation),
	   latitude= as.numeric(rr$Latitude),
	   longitude= as.numeric(rr$Longitude),
	   plot_area= 2.25/10000, # ha
	   row_spacing= 75,
	   plant_spacing= 30,
	   plant_density= (5/2.25)*10000
	)
	
	d <- carobiner::bindr(d1, d2)
	
	d$geo_from_source <- TRUE
	d$yield_part <- "tubers"
	d$stress <- "heat"
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$irrigated <- NA
	d$yield_moisture <- as.numeric(NA)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	### Process weather data 
	
	   # location: Gode
	rr <- data.frame(rbind(r11$Value))
	names(rr) <- r11$Factor
	   w1 <- data.frame(
	      date= as.character(as.Date(r12$DATE, format = "%d/%m/%Y")),
	      temp= r12$TMEAN,
	      tmax= r12$TMAX,
	      tmin= r12$TMIN,
	      country= rr$Country,
	      adm1= rr$Admin1,
	      adm2= rr$Admin2,
	      adm3= rr$Admin3,
	      location= rr$Locality,
	      elevation= as.numeric(rr$Elevation),
	      latitude= as.numeric(rr$Latitude),
	      longitude= as.numeric(rr$Longitude),
	      geo_from_source= TRUE
	   )
	   
	   # location: Melkasa
	   rr <- data.frame(rbind(r22$Value))
	   names(rr) <- r22$Factor
	   w2 <- data.frame(
	      date= as.character(as.Date(r21$DATE, format = "%d/%m/%Y")),
	      temp= r21$TMEAN,
	      tmax= r21$TMAX,
	      tmin= r21$TMIN,
	      country= rr$Country,
	      adm1= rr$Admin1,
	      adm2= rr$Admin2,
	      adm3= rr$Admin3,
	      location= rr$Locality,
	      elevation= as.numeric(rr$Elevation),
	      latitude= as.numeric(rr$Latitude),
	      longitude= as.numeric(rr$Longitude),
	      geo_from_source= TRUE
	   )
  wth <- rbind(w1, w2)	   
  	   
	
carobiner::write_files(path, meta, d, wth = wth)

}


