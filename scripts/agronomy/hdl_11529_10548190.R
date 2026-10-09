# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Replication Data for: Nonlinear heat effects on African maize as evidenced by historical yield trials

This dataset provides supplementary files for the trials and sites described in the 2011 paper in https://www.nature.com/nclimate/ available at https://dx.doi.org/10.1038/NCLIMATE1043
"

	uri <- "hdl:11529/10548190"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "BAU; CIMMYT",
		publication = "doi:10.1038/nclimate1043",
		project = NA,
		carob_date = "2026-10-09",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_type",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		completion = 100,	
		notes = NA,
	  carob_completion = 100,	
		carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "EIL_site_latlon.csv"]
	f2 <- ff[basename(ff) == "maizedata.lobell.sep2011.csv"]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	
	d1 <- data.frame(
	  country = r1$Country,
	  location = r1$Location,
	  longitude = r1$Longitude,
	  latitude = r1$Latitude,
	  elevation = r1$ElevM,
	  sitecode = r1$LocationID
	
	  )
	
	d1$country <- carobiner::fix_name(d1$country, "title")
	d1$country[d1$country == "South Africa Rep."] <- "South Africa"
	d1$country[d1$country == "Swaziland"] <- "Eswatini"
	d1$country[d1$country %in% c("Zaire","Congo") ] <- "Democratic Republic of the Congo"
	#d1$country[d1$country == "Congo"] <- "Republic of the Congo"  # see ISSUES note
	
	d1$location <- carobiner::fix_name(d1$location, "title")
	
	d2 <- data.frame(
	  crop = "maize",
	  yield_part = "grain",
	  yield         = exp(r2$logYield) * 1000,
	  treatment     = r2$Management,
	  planting_date = as.character(as.Date(as.character(r2$PlantingDate), "%Y%m%d")),
	  anthesis_days = r2$AnthesisDate,
	  silking_days  = r2$DaysToSilk,
	  asi           = r2$ASI,
	  variety_type  = ifelse(grepl("HY$", r2$vargroup), "hybrid", "OPV"),
	  variety_traits    = ifelse(grepl("^E", r2$vargroup), "early-intermediate", "intermediate-late"),
	  harvest_date  = r2$yrcode,
	  sitecode      = r2$sitecode
	)
	
	d <- merge(d2, d1, by = "sitecode", all.x = TRUE)
	
	d$geo_from_source <- TRUE
	d$trial_id <- paste(d$sitecode, d$harvest_year, sep = "_")
	d$sitecode <- NULL
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$fertilizer_type <- d$yield_isfresh <- d$yield_moisture <- NA
	
	#fixing geo locations
	d$longitude[d$location == "Alupe"] <- 34.13   # https://www.geonames.org/search.html?q=Alupe&country=KE
	d$longitude[d$location == "Maseru"] <- 27.48   # https://www.geonames.org/search.html?q=Maseru&country=LS
	d$latitude[d$location == "Maseru"] <- -29.31   # https://www.geonames.org/search.html?q=Maseru&country=LS
	d$longitude[d$location == "Lambo"] <- NA
	d$latitude[d$location == "Lambo"] <- NA       #unable to find correct coordinates for Lambo 
	
	d$asi[d$asi > 15] <- NA   
	d$harvest_date <- as.character(d$harvest_date)
	
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)

}


