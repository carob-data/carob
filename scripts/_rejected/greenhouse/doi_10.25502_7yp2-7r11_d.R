# R script for "carob"
# license: GPL (>=3)

## ISSUES
# greenhouse experiment
# no location data


carob_script <- function(path) {

"
Yam tuber production in the aeroponics - 2019

Yam Improvement for Income and Food Security in West Africa
"

	uri <- "doi:10.25502/7yp2-7r11/d"
	group <- "draft"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		treatment_vars = "",
		
		response_vars = "", 
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-08-24",
		carob_completion = 0,	
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "2019_aerotuberharvestdataset-withvarieties.csv"]
	f2 <- ff[basename(ff) == "aerotuberharvestdataset_metadata.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	d1 <- data.frame(
		harvest_date = r1[["Harvest_Date"]],
		location = r1[["Site"]],
		variety = r1[["Variety"]]
	)


	d2 <- data.frame(
		country = r2[["coverage.country"]]
	)

	d$trial_id <- as.character(as.integer(as.factor( )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
	d$longitude <- 
	d$latitude <- 
	  
	d$geo_from_source <- TRUE/FALSE


	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100


	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}


