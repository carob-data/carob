# R script for "carob"


carob_script <- function(path) {

"Productivity and water use efficiency of Sorghum [Sorghum bi color (L.) Moench] grown under different nitrogen applications in Sudan Savanna Zone, Nigeria. To review the nutrient needs, especially N of some of selected sorghum varieties and their water use efficiency on marginal land."


	uri <- "doi:10.21421/D2/NRCQKU"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-08-06",
		notes = NA
	)
	

	f <- ff[basename(ff) == "Data file of Sorghum grown under different nitrogen application in sudan savanna zone.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		country = "Nigeria",
		location = r$Location,
		crop= "sorghum",
		yield_part = "grain",
		variety = r$Sorghum,
		planting_date = as.character(r$Year),
		rep = as.integer(r$`Replication number`),
		N_fertilizer = r$Nitrogen,
		LAI = r$LAI_9WAP,
		plant_height = r$PH_M_cm,
		flowering_days = r$Flo_c_day,
		maturity_days = r$Mat_c_day,
		yield = r$GStdYld_c_Kgha,
		dmy_residue = r$`Stalk yield`,
		seed_weight = r$GW_1000gmM_g
	)
	
	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	#co-ordinates obtained through carobiner::geocode function
	d$longitude[d$location=="BUK"] <- 8.427
	d$latitude[d$location=="BUK"] <- 11.9703
	d$longitude[d$location=="Minjibir"] <- 8.6142
	d$latitude[d$location=="Minjibir"] <- 12.1936
	d$geo_from_source <- FALSE
	
	d$location <- gsub("BUK","Bayero University Kano",d$location)
	
	d$trial_id <- as.character(as.integer(as.factor(1)))
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE


## Database stated the project started 2014-06-14 and ended 2015-12-25, there are no specific planting and harvesting dates
	#d$planting_date <- as.character(as.Date(   ))
	#d$harvest_date  <- as.character(as.Date(    ))

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}


