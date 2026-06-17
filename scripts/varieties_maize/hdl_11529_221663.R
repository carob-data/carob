# R script for "carob"

## ISSUES
# no specific location data. Since all trials are in Kano, used that to georeference one location based on metadata


carob_script <- function(path) {

"This dataset contains information from on-farm validation trials conducted across 30 farmer fields in Local government areas of the sudan savanna AEZ in Kano. In each farmer field 10 plots (30 meter square each) were planted with a different combination of early maturing maize (10 varieties) under 3 different sowing densitie. Each plot is a different combination of variety and sowing data. Data was collected on tops weight at anthesis, cob yield and stover yield. (2017-12-10)"

	uri <- "hdl:11529/221663"
	group <- "varieties_maize"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "variety;plant_density", 
		carob_contributor = "Shumirai Manzvera",
		carob_effort = NA,
		carob_date = "2024-07-09"
	)
	
	f <- ff[basename(ff) == "NG_VT_Validation_SS_2016.xlsx"]
	r <- carobiner::read.excel(f, sheet = "DATA")

	d <- data.frame(
		variety=r$Variety,
		plant_density=r$Density,
		yield=r$GY_ha,
		trial_id = as.character(r$`Plot No`)
	)

	d$country <- "Nigeria"
	d$adm1 <-  "Kano"
#Kura:  Latitude:11.800813 Longitude: 8.526093  Latitude: 11.78191  Longitude: 8.1296         
#Bunkure:   Latitude: 11.7246 Longitude: 8.53585  Latitude: 11.65997  Longitude: 8.55155       
#Garun mallam:Latitude: 11.66361   Longitude: 8.37957   Latitude: 11.65863      Longitude: 8.37519            
	d$longitude <- 8.4
	d$latitude <- 11.73
	d$geo_from_source <- FALSE

	d$planting_date <- "2015-06"
	d$harvest_date  <- "2015-11"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$striga_trial <- FALSE
	d$borer_trial <- FALSE
	d$striga_infected <- FALSE

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- NA

	d <- d[!is.na(d$yield), ]
	carobiner::write_files(path, meta, d)
}

