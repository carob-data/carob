# R script for "carob"

## ISSUES
# lon/lat is missing for the fields. Needs to be requested. Without that of much less use.



carob_script <- function(path) {

"The data represent 804 cropping events of maize planting in Córdoba - Colombia from 2013 to 2016. Each cropping event has information on climate, soil, and agronomic management. The data was collected with the aim of determining the factors that 
affect the variation in the yield of maize. The dataset was created through a collaboration project among International Center for Tropical Agriculture CIAT, the Colombian National Cereals, and Legumes Federation (FENALCE) and the Colombian Ministry of Agriculture and Rural Development (MADR)."


	uri <- "doi:10.7910/DVN/LSDQ5C"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIAT;FENALCE",
		publication= "doi:10.1016/j.gfs.2019.08.004", 
		project=NA,
		data_type= "experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-04-02",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer"
	)
	
	f <- ff[basename(ff) == "Dataset_Maize_Cordoba.csv"]
	r <- read.csv(f)

	d <- data.frame(
		crop="maize",  
	    variety=r$Cultivar, 
		previous_crop=r$Former_Crop, 
		variety_type=r$Cultivar_Type, 
	    soil_texture=r$Soil_Texture, 
		yield=r$Yield*1000, 
	    N_fertilizer=r$Total_N*1000, 
		P_fertilizer=r$Total_P*1000, 
		K_fertilizer=r$Total_K*1000, 
		herbicide_times=as.integer(r$Chemical_Treat_Weeds),
		insecticide_times=as.integer(r$Chemical_Treat_Pests),
		fungicide_times=as.integer(r$Chemical_Treat_Disease),
		plant_density=r$Sowing_Seeds_Number,
		soil_depth=r$Efective_Depth,
		planting_method=r$Sowing_Method,
	    soil_pH=r$pH
	)

	d$planting_method <- carobiner::replace_values(d$planting_method, 
		c("Mecanizado", "Manual"), c("mechanized", "manual"))
	
	d$trial_id <- as.character(1:nrow(d))
	d$previous_crop <- carobiner::replace_values(d$previous_crop, 
			c("Algodon", "Frijol",      "Maiz",  "Pastos", "Yuca"),
			c("cotton",  "common bean", "maize", "pasture", "cassava"))

	d$on_farm <- TRUE

	d$country <- "Colombia"
	d$adm1 <- "Córdoba"
	d$longitude <- -75.8040791
	d$latitude <-  8.295031 
	d$geo_from_source <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$row_spacing <- 85 #cm
	d$plant_spacing <- 18.5 # cm

	d$planting_date <-	as.character(as.Date(r$Planting_Date, format = '%m/%d/%Y'))
	d$harvest_date  <-	as.character(as.Date(r$Harvest_Date, format = '%m/%d/%Y'))
	d$planting_date[is.na(d$planting_date)] <- "2015"
	d$yield_part <- "grain"
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}


