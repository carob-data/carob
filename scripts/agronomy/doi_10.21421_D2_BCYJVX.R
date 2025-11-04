# R script for "carob"


carob_script <- function(path) {

"Data on sequential observations of agronomically important traits like Leaf biomass, Stem biomass, Total biomass, Leaf area, and Grain yield during cropping season for the year 2016 in Kharif for Sorghum at ICRISAT Patancheru campus research fields.Data on multi factorial treatments in 2016-17 Kharif Sorghum at Patancheru"
	
	uri <- "doi:10.21421/D2/BCYJVX"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;plant_density;irrigated",
		response_vars = "fwy_leaves, fwy_stems, fwy_total, yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-08-21",
		notes = NA
	)
	
	f1 <- ff[basename(ff) == "2.Experiment.xlsx"]
	r1 <- carobiner::read.excel(f1)

	d1 <- data.frame(
		plot_id= r1$Plot,
		code= r1$`Experiment-code`,
		plot_Nr= r1$`Plant-no`,
		variety_code=r1$Genotype, 
		planting_date=as.character(as.Date(r1$`Sowing-date`, "%d/%m/%Y")),
		emergence_date=as.character(as.Date(r1$`Emergence-date`, "%d/%m/%Y")),
		flowering_date=as.character(as.Date(r1$`Flowering-date`, "%d/%m/%Y")),
		maturity_date=as.character(as.Date(r1$`Maturity-date`, "%d/%m/%Y")),
		row_spacing=r1$`Row-spacing`,
		soil_type=r1$`Soil-type`,
		irrigated=TRUE,
		irrigation_number= as.integer(3) ,
		irrigation_amount= r1$`Irri1-amt`+ r1$`Irri2-amt`+ r1$`Irri3-amt`,
		## DAP: 18% N 20.1% P; urea: 46% N
		N_fertilizer= (r1$`Fert1-amt`)*0.18+ (r1$`Fert2-amt`)*0.46 + (r1$`Fert3-amt`)*0.46,
		P_fertilizer= (r1$`Fert1-amt`)*0.201,
		fertilizer_date= paste(as.Date(r1$`Fert1-date`,"%d/%m/%Y") , as.Date(r1$`Fert2-date`, "%d/%m/%Y"), as.Date(r1$`Fert3-date`, "%d/%m/%Y"), sep = ";"),
		irrigation_dates= paste(as.Date(r1$`Irri1-date`,"%d/%m/%Y"), as.Date(r1$`Irri2-date`, "%d/%m/%Y"), as.Date(r1$`Irri3-date`,"%d/%m/%Y"), sep = ";"),
		K_fertilizer = 0, 
		fertilizer_type="urea;DAP"
	)
		
	f2 <- ff[basename(ff) == "1.Growth and development.xlsx"]
	r2 <- suppressWarnings(carobiner::read.excel(f2,na="NA"))
		
	d2 <- data.frame(
	  variety_code=r2$`Cultivar-short-name`,
	  plot_id= r2$`Plot-id`,
	  code= r2$`Location-code`,
	  plot_Nr= r2$`Plant-no`,
	  treatment=r2$`Treatment-combinations`,
	  fwy_leaves=r2$`TC-leaf-biom-ha`,
	  fwy_stems=as.numeric(r2$`TC-stem-biom-ha`),
	  fwy_residue=as.numeric(r2$`TC-stover-wt-ha`),
	  fwy_total=as.numeric(r2$`TC-tot-biom-ha`),
	  plant_density=r2$`TC-density`*10000,
	  LAI=as.numeric(r2$`TC-lai`),
	  yield=r2$`TC-grain-wt-ha`
	  
	)
	d2$treatment[d2$treatment=="WW_LD_HN"] <- "well watered_low density_high nitrogen"
	
	d <- merge(d1, d2, by=c("plot_id", "variety_code", "code", "plot_Nr"), all.x = TRUE)

	d$plot_Nr <- d$plot_id <- d$code <- NULL

	d$country <- "India"
	d$adm1 <- "Telanyana"
	d$adm2 <- "Sangareddy"
	d$location <- "Patancheru"
	d$site <- "ICRISAT Patancheru campus"
	d$longitude <- 78.276 
	d$latitude <- 17.51
	d$geo_from_source <- FALSE

	d$crop <- "sorghum"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d <- d[!is.na(d$yield), ]
	d$trial_id <- "1"
	
## processing weather data 
  
	f3 <- ff[basename(ff) == "7.Weather.xlsx"]
	r3 <- carobiner::read.excel(f3)
	
	dw <- data.frame(
		country= "India",
		adm1 = "Telanyana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		site = "ICRISAT Patancheru campus",
		longitude = 78.276, 
		latitude = 17.51,
		geo_from_source = FALSE,
		station_name= r3$`MetStation-Name`,
		date= as.character(as.Date(r3$Date,"%d/%m/%Y")),
		prec= r3$Rain,
		srad= r3$Radn,
		tmax= r3$MaxT,
		tmin= r3$MinT,
		rhmx= r3$MaxRH,
		rhmn= r3$MinRH
	)
	

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d, wt=dw)
}



