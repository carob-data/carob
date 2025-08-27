# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {


"Grain yield, biomass, and nutritional values of various cropping systems tested on-station with/without fertilizer application on two soils in Zimbabwe between 2020 and 2023
  
This database contains data on grain, biomass, protein, and calorie yields from on-station trials in Zimbabwe, conducted at the Domboshava Training Centre (DTC) (17.62°S, 31.17°E) and the University of Zimbabwe farm (UZ) (17.73°S, 31.020°E), characterized by different soil types.

The trials tested maize monocropping, maize-legume rotations, and intercropping with various layouts (traditional intercropping and double-row strip cropping), with and without fertilizer application. Two contrasting legumes, cowpea and pigeon pea, were included in the cropping systems. The experiment was established during the 2019/2020 growing season, but data were collected from the 2020/21 to 2022/23 seasons to allow the rotations to develop. It was carried out under Conservation Agriculture and rainfed conditions.

The dataset is divided into three parts:
1) Field measurement, including treatments, grain yields, and crop biomass;
2) Calculations at the cropping system level (total biomass, calories, and proteins);
3) Processed data used to evaluate the stability of each cropping system, as shown in a radar plot presented in the related paper."


## Identifiers
	uri <- "doi:10.71682/10549322"
	group <- "agronomy"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
# change the major and minor versions if you see a warning
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;UZIM",
		publication = NA,
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "crop_rotation;fertilizer_used",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-08-26",
		notes = NA,
		design = NA
	)
	
## read data 

	f <- ff[basename(ff) == "Masamba et al_contrasting legumes and layouts_data.xlsx"]
	r1 <- read_excel(f, sheet="yield_data")
	#r2 <- read_excel(f, sheet="system_data")

## select the variables of interest and assign them to the correct name
	d <- data.frame(
	  country="Zimbabwe",
	  planting_date="2019",
	  harvest_date=as.character(r1$year),
	  location=r1$site,
	  rep=r1$rep,
	  plot_id=r1$plot,
	  crop=r1$crop,
	  crop_rotation=r1$cropping.system,
	  fertilizer_used=r1$fertilizer,
	  dmy_total=r1$biomass.kg.ha,
	  yield=r1$grain.kg.ha,
	  yield_part="grain"
)

	d$longitude <- ifelse(d$location == "DTC", 31.13, 31.05)   
	d$latitude  <- ifelse(d$location == "DTC", -17.60, -17.83) 
	d$rep <- as.integer(sub(".*?(\\d+)$", "\\1", d$rep))
	d$location <- gsub("UZ","University of Zimbabwe",d$location)
	d$location <- gsub("DTC","Domboshawa Training Centre",d$location)
	d$crop_rotation <- gsub("MZ-CP","maize;cowpea",d$crop_rotation)
	d$crop_rotation <- gsub("MZ-PP","maize;legume",d$crop_rotation)
	d$crop_rotation <- gsub("MZCP2","maize_cowpea",d$crop_rotation)
	d$crop_rotation <- gsub("MZPP1|MZPP2","maize_legume",d$crop_rotation)
	d$crop_rotation <- gsub("MZ","maize",d$crop_rotation)
	d$crop <- gsub("pigeonpea","legume",d$crop)
	d$trial_id <- as.character(paste0(d$location, "_", d$planting_date))
	d$fertilizer_used <- ifelse(d$fertilizer_used=="Fert-",FALSE,TRUE)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)
	d$yield_moisture <- 12.5
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}
