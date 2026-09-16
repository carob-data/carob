# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them
## added new variables 
#vegetation_type,field_type,soil_vWC = volumetric moisture content %,soil_temp = %, methane emmisions = soil_CH4,  which are part of the treatment levels

carob_script <- function(path) {

"
Smallholder farms in eastern African tropical highlands have low soil greenhouse gas fluxes

The data was used to estimate annual greenhouse gas fluxes from typical smallholder agricultural systems in western Kenya. We had attempted to classify some of the production systems by remote sensing data and farming systems (ie. landuse), and we include some of the ancillary data as well in order to help explain some of the patterns in the emissions. Therefore we are including the greenhouse gas fluxes from smallholder farming systems in western Kenya; weather data (daily precipitation, daily max and minimum temperatures, soil volumetric water content [20 cm depth], soil temperature [20 cm depth] and soil temperature [5 cm depth]); Coordinates of farms where sampling occurred.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.7910/DVN/LVRFMT"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ILRI;ICRAF",
		publication = "doi.org/10.5194/bg-14-187-2017",
		project = NA,
		design = NA,
		data_type = "survey",### this study included both survey and experiment
		treatment_vars = "land_use;field_type;vegetation_type",
		response_vars = "soil_N2O;soil_CH4", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-08-18",
		carob_completion = 80,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "GHG and IN.xlsx"]

	#r1a <- carobiner::read.excel(f1, sheet="metadata")
	r1b <- carobiner::read.excel(f1, sheet="Aug_to_July fluxes")
	r1c <- carobiner::read.excel(f1, sheet="weather station data")
	r1d <- carobiner::read.excel(f1, sheet="farm coordinates")
	
	
	
	d1 <- data.frame(
	  trial_id = r1b$Site,
	  fertilizer_used = TRUE,
	  land_use = r1b$class_num2,
	  vegetation_type = r1b$crop,
	  date = format(as.Date(r1b$Date), "%Y-%m-%d"),
	  crop = r1b$crop,
	  #treatment = r1b$Site,### seems treatment is a combination of site, field type and vegetation type
	  field_type = r1b$Field_Type,
	  soil_N2O = r1b$N2O,
	  soil_CH4 = r1b$CH4,
	  emission_CO2 = r1b$CO2
	  #soil_temp = r1b$Soil_temp
	)
	
	d1$crop[d1$crop == "napier"] <- "napier grass" 
	d1$crop[d1$crop == "grass/grazing"] <- "unknown" #### highlighted as grass/grazing the name of the grass was not identified 
	d1$crop[d1$crop == "fallow"] <- "none"
	d1$crop[d1$crop == "trees/shrubs"] <- "unknown" ## indicated that there were trees or shrubs but not identified by species
	d1$crop[d1$crop == "annuals"] <- "unknown"
	
	##### vegetation type refers to vegetation cover
	d1$vegetation_type[d1$vegetation_type == "napier"] <- "napier grass" 
	d1$vegetation_type[d1$vegetation_type == "grass/grazing"] <- "grass" 
	d1$vegetation_type[d1$vegetation_type == "fallow"] <- "none" #### assumed fallow land is the same as bare soil
	d1$vegetation_type[d1$vegetation_type == "trees/shrubs"] <- "trees" 
	d1$vegetation_type[d1$vegetation_type == "annuals"] <- "crop"
	
	
	d1$field_type[d1$field_type == "1"] <- "most intensively managed" #(more fertilizer/manure additions)
	d1$field_type[d1$field_type == "2"] <- "moderately managed"
	d1$field_type[d1$field_type == "3"] <- "least managed" #none to very low fertilizer additions, degraded, low soil C)
	

	d1$land_use[d1$land_use == "1"] <- "lowland subsistence farms with degradation signs" 
	d1$land_use[d1$land_use == "2"] <- "lower slopes moderate-sized mixed farms"
	d1$land_use[d1$land_use == "3"] <- "mid-slopes moderate-sized primarily grazing/shrubland"
	d1$land_use[d1$land_use == "4"] <- "upper slopes/highland plateau mixed farms"
	d1$land_use[d1$land_use == "5"] <- "mid-slopes moderate-sized mixed farms"
	
	d1$fertilizer_amount <- 25 # indicated in publication but only applied by two farmers 
	
	d1$record_id <- match(paste(d1$trial_id, d1$date), unique(paste(d1$trial_id, d1$date)))
	d1$hhid <- sub("-.*", "", d1$trial_id)
	d1$record_id <- NULL
	
	d1_long <- rbind(
	  transform(d1, date = r1b$Date, depth = 5, soil_NO3 = r1b$NO3_top, soil_NH4 = r1b$NH4_top),
	  transform(d1, date = r1b$Date, depth = 20, soil_NO3 = r1b$NO3_bottom, soil_NH4 = r1b$NH4_bottom)
	)
	
	
	d1_long$date <- format(as.Date(d1_long$date), "%Y-%m-%d")
	
	
	wth <- data.frame(
	  date = format(as.Date(r1c$Date), "%Y-%m-%d"),
	  tmin = r1c$`min.temp`,
	  tmax = r1c$`max.temp`,
	  prec = r1c$`Precip.mm` 
	)
 
	
	#### added variable volumetric water content in soil
	d2_long <- rbind(
	  data.frame(date = r1c$Date, depth = 5,
	             soil_temp = r1c$soil.temp.5cm, soil_vWC = NA),
	  data.frame(date = r1c$Date, depth = 20,
	             soil_temp = r1c$soil.temp.20cm, soil_vWC = r1c$VWC.20cm)
	)
	
	d2_long$date <- format(as.Date(r1c$Date), "%Y-%m-%d")
	
	
	d_long <- merge(d1_long,d2_long,by=c("date","depth"), all.x = TRUE)

	d3 <- data.frame(
	  country = "Kenya",
	  location = "Nyando",
	  hh_key = r1d$HH_ID,
	  latitude = r1d$LAT,
	  longitude = r1d$LON,
	  geo_from_source = TRUE
	)
	
	d3$hhid <- gsub("-", "", sub("^KE01-", "", d3$hh_key))
	d3$hh_key <- NULL
	
	d <- merge(d_long,d3,by="hhid",all.x = TRUE)
	d <- unique(d)
	
	 
	wrong_coord <- d$longitude == 30.05 & d$latitude == -0.34	#### removing wrongly placed coordinates represented were for Uganda instead of Kenya
	d$latitude[wrong_coord] <- NA
	d$longitude[wrong_coord] <- NA 
	d <- d[!(is.na(d$latitude) & is.na(d$longitude)), ]
	
	
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- NA
	d$harvest_date  <- NA
	d$on_farm <- FALSE
	d$is_survey <- TRUE 
	d$irrigated <- NA
	
	
	d$N_fertilizer <- 25#### to specify the fertilizer rates per location
	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
  d$fertilizer_type <- NA 

  d$yield <- NA
  d$yield_part <- "none"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- NA
  
  
	carobiner::write_files(path, meta, d, wth = wth)
}


