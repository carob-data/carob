# R script for "carob"
# license: GPL (>=3)

## ISSUES
# coordinate data is in decimal minutes, and some of the lat/lon values are swapped. they are converted to decimal degrees and swapped where necessary. 
# some of the coordinates are still outside of the expected range for Borno, Nigeria, so they are replaced with the coordinates for the local government area (lgs) that the survey was taken in.


carob_script <- function(path) {

"
Household Survey Data on climate resilient cowpea variety

N2AFRICA is a large scale, science-based “research-in-development” project focused on putting nitrogen fixation to work for smallholder farmers growing legume crops in Africa.
"

	uri <- "doi:10.25502/x9gm-b461/d"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "N2AFRICA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield", 
		notes = "Yield was collected by the provider, no fertiliser data was provided, the survey was taken in 3 local government areas (Biu, Hawul, Kwaya Kusar) in Borno Nigeria",
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-10-21",
		carob_completion = 70,	
		carob_effort = 6
	)
	
	f <- ff[basename(ff) == "borno-project-cowpea-data.csv"]
	r <- read.csv(f, stringsAsFactors = FALSE)

	d <- data.frame(
		hhid = as.character(r$ID),
		country = "Nigeria",
		adm1 = "Borno",
		adm2 = trimws(r$local_govt),
		location = trimws(r$village),
		hh_size = r$hholdsize,
		age = r$hhh_age, # head of household age
		sex = ifelse(r$hhh_gender == 1, "Male", "Female"), # head of household sex
		cooperative_member = ifelse(r$mem_assoc == 1, TRUE, FALSE),
		credit_access = ifelse(r$access_credit == 1, TRUE, FALSE),
		yield = r$cowpea_output_ha,
		pesticide_cost = r$pesticide_cost_ha,
		tmax = r$mean_max_temp,
		tmin = r$mean_min_temp,
		rain = r$mean_rainfall,
		irad = r$mean_srad,
		elevation = r$gps_alt
	)

	d$crop <- "cowpea"
	d$yield_part <- "seed"
	d$variety_type <- ifelse(r$adopt_improved_cowpea == 1, "Improved", "Local")
	
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- NA # no irrigation data provided, mention of mean rainfall

	# coverting decimal minutes to decimal degrees
	dm2dd <- function(x) {
		deg <- x %/% 100
		min <- x %% 100
		ifelse(min >= 60, NA, deg + min / 60)
	}

	# sorting the longitude and latitude values out by size as most of the vals are swapped.
	# latitude is < 1100 (according to carobiner geodata)
	a <- r$gps_lon
	b <- r$gps_lat
	a_is_lat <- !is.na(a) & !is.na(b) & a < 1100 & b >= 1100 
	b_is_lat <- !is.na(a) & !is.na(b) & b < 1100 & a >= 1100 
	lat_raw <- ifelse(a_is_lat, a, ifelse(b_is_lat, b, NA)) 
	lon_raw <- ifelse(a_is_lat, b, ifelse(b_is_lat, a, NA))
	d$latitude <- dm2dd(lat_raw)
	d$longitude <- dm2dd(lon_raw)

	bad_coords <- is.na(d$longitude) | is.na(d$latitude) | d$longitude < 11.7 | d$longitude > 12.35 | d$latitude < 10.2 | d$latitude > 10.8
	d$geo_from_source <- !bad_coords

	lga <- data.frame(
		adm2 = c("Biu", "Hawul", "Kwaya Kusar"),
		lon = c(12.1849, 12.3720, 11.9808),
		lat = c(10.7919, 10.4571, 10.4407)
	)
	i <- match(d$adm2, lga$adm2)

	d$longitude[bad_coords] <- lga$lon[i][bad_coords]
	d$latitude[bad_coords] <- lga$lat[i][bad_coords]
   
	carobiner::write_files(path, meta, d)
}


