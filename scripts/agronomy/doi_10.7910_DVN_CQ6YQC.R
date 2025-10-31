# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Replication Data for: Participatory Upland Rice Seed Rate Determination for Row Method of Sowing

Participatory upland rice seed determination for row method of sowing
"

   uri <- "doi:10.7910/DVN/CQ6YQC"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "EIAR",
		publication = "doi:10.5897/AJAR2019.14343",
		project = NA,
		carob_date = "2025-10-31",
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_rate",
		response_vars = "yield;fwy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "Seed rate for upland rice.xls"]

	r1 <- carobiner::read.excel(f, sheet="FRG SAS in put werer")
	r2 <- carobiner::read.excel(f, sheet="FRG  SAS in put Beduloale")
	
	
	### process 
	   ## werer location
	d1 <- data.frame(
	   plot_id = as.character(r1$Plot),
	   seed_rate = r1$Treat,
	   rep = as.integer(r1$Rep),
	   plant_height = r1$PH,
	   fwy_total = r1$`BY t/ha`*1000,
	   yield = r1$`GY t/ha`*1000,
	   location = "werer" ,
	   country = "Ethiopia",
	   crop = "rice",
	   planting_date = "2013",
	   longitude = 40.177 ,
	   latitude = 9.335  ,
	   geo_from_source = FALSE,
	   on_farm = FALSE,
	   is_survey = FALSE,
	   trial_id = "1",
	   yield_part = "grain",
	   yield_moisture = as.numeric(NA),
	   irrigated = NA
	)

	d1$seed_rate <- rep(c("50(drilling)", "60(drilling)", "70(drilling)", "80(drilling)","60(broadcasting)"), time=3)[d1$seed_rate]
	
	
	### Beduloale
	d2 <- data.frame(
	   plot_id = as.character(r2$Plot),
	   seed_rate = r2$Treat,
	   rep = as.integer(r2$Rep),
	   plant_height = r2$PH,
	   fwy_total = r2$BYTH*1000,
	   yield = r2$GYTH*1000,
	   location = "Beduloale" ,
	   country = "Ethiopia",
	   crop = "rice",
	   planting_date = "2013",
	   longitude = 40.0569 ,
	   latitude = 11.1029, 
	   geo_from_source = FALSE,
	   on_farm = TRUE,
	   is_survey = FALSE,
	   trial_id = "2",
	   yield_part = "grain",
	   yield_moisture = as.numeric(NA),
	   irrigated = NA
	)
	
	d2$seed_rate <- rep(c("50(drilling)", "60(drilling)", "70(drilling)", "80(drilling)","60(broadcasting)"), time =3)[d2$seed_rate]
	
	### joint d1 and d1 
	d <- carobiner::bindr(d1, d2)
	
	d$planting_method <- ifelse(grepl("drilling", d$seed_rate), "mechanized", "broadcasting")
   d$seed_rate <- as.numeric(gsub("\\(drilling\\)|\\(broadcasting\\)", "",  d$seed_rate))	

   
   #### Adding soil data from publication 
   
   soil <- data.frame(
      seed_rate = rep(c(50, 60, 70, 80, 60), time = 2),
      planting_method = c(rep("mechanized", 4),"broadcasting", rep("mechanized", 4), "broadcasting"),
      location = c(rep("Beduloale", 5), rep("werer", 5)),
      soil_pH = c(8.3, 8.3,8.4, 8.4, 8.4, 8.3, 8.4, 8.4, 8.5, 8.4),
      soil_EC = c(0.402, 0.505, 0.394, 0.408, 0.457, 0.463, 0.409, 0.439, 0.512, 0.502)*10,
      soil_SOC = c(0.397, 0.494, 0.462, 0.631, 0.585, 0.254, 0.306, 0.273, 0.137, 0.338),
      soil_N_total = c(0.034, 0.043, 0.040, 0.054, 0.05, 0.022, 0.026, 0.024, 0.012, 0.029)*10000,
      soil_SOM = c( 0.684, 0.852, 0.796, 1.087, 1.009, 0.437, 0.527, 0.471, 0.235, 0.583),
      soil_P = c(8.280, 9.655, 8.155, 10.405, 8.280, 17.279, 14.655, 14.030, 21.841, 18.311)
   )
   
  d <- merge(d, soil, by= c("seed_rate", "location", "planting_method"), all.x = TRUE) 
 
  d$N_fertilizer <- d$P_fertilizer <-  d$K_fertilizer <- as.numeric(NA)
    
carobiner::write_files(path, meta, d)

}

