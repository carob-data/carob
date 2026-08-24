# R script for "carob"
# license: GPL (>=3)

## ISSUES
## weather data is missing 


carob_script <- function(path) {

"
Dataset for: Potato NPT Trials Amidst Prolonged Rains in Kenyan Regions - April 2009

The Potato NPT Trials Amidst Prolonged Rains in Kenyan Regions took place in April of 2009 in various locations including Kibirichia, Kisima, Molo, Naraok, Oljoro, Tigoni, Baraka, and Limuru. The study was conducted under a randomized complete block design (RCBD) with eight potato clones and four replications, aiming to assess the performance of these clones in challenging weather conditions characterized by extended periods of rainfall.
"

	uri <- "doi:10.21223/ENR3AN"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield_marketable", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-24",
		carob_completion = 100,	
		carob_effort = 2
	)
	
	#f1 <- ff[basename(ff) == "08_Data_dictionary.xlsx"]
	#r1 <- carobiner::read.excel(f1)
	ff1 <- ff[grepl("PTYL", basename(ff))]
	
	
	
#### process 
	proc <- function(f){
	  r1 <- carobiner::read.excel(f, sheet="Minimal")
	  rr <- as.data.frame(t(r1$Value))
	  names(rr) <- r1$Factor
	  r2 <- carobiner::read.excel(f, sheet="Crop_management")[, -1]
	  Phd <- as.data.frame(t(r2$Date))
	  names(Phd) <- r2$`Intervention type`
	  r <- carobiner::read.excel(f, sheet="Fieldbook")
	  data.frame(
	    plot_id = as.character(r$PLOT),
	    rep = as.integer(r$REP),
	    variety = r$INSTN,
	    country = rr$Country,
	    crop = rr$Crop,
	    adm1 = carobiner::fix_name(rr$Admin1, "title"),
	    adm2 = carobiner::fix_name(rr$Admin2, "title"),
	    location = carobiner::fix_name(rr$Locality, "title"),
	    longitude = as.numeric(rr$Longitude),
	    latitude = as.numeric(rr$Latitude),
	    planting_date = Phd$Planting,
	    harvest_date = Phd$Harvest,
	    #seed_density = r$NTP, ## need to be divide by plot area
	   # plant_density = r$NPE, ## need to be divide by plot area
	    yield_marketable = r$MTYNA*1000,
	    trial_id = gsub(".xls", "", basename(f))
	  )
	  
	}
	
	d <- lapply(ff1, proc)
	d <- do.call(rbind, d)
	

	d$is_survey <- FALSE 
	d$on_farm <- TRUE
	d$yield_moisture <- NA_real_
	d$yield_part <- "tubers"
	d$geo_from_source <- TRUE
	d$irrigated <- NA
	d$yield_isfresh <- NA
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)

	
	
	carobiner::write_files(path, meta, d)
}


