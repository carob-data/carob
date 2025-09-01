# R script for "carob"


carob_script <- function(path) {

"This data was collected to develop baselines for three Land Degradation Neutrality (LDN) indicators: land use and land cover change (LUC) for the period 2001-2017, soil organic carbon (SOC) stocks for 2017 and bush density for 2017 as a baseline for bush encroachment in Omusati region, Namibia. "

	uri <- "doi:10.7910/DVN/XZIRK0" 
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIAT",
		publication= "doi:10.3390/su10051610",
		project="Baselines for land degradation neutrality indicators in the Omusati region, Namibia",
		data_type= "survey",
		treatment_vars = "none",
		response_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_date="2024-07-29"
	)
  
	f1 <- ff[basename(ff) == "04. Soil carbon and bulk density sampling points.csv"]
	r1 <- read.csv(f1)
	
	# Rearrange data for soc1 and soc2 into one variable soc
	# soc1 from the Codebooks.xls is soc at 0-30cm
	# soc2 from the Coodebooks.xls is soc at 30-100cm
	# bd1 from the Codebooks.xls is bulk density at 15 cm
#	r1[1:4,]
	r1a <- r1[,-5]
	r1b <- r1[,-4]
	r1b$bd1 <- NA
	colnames(r1a) <- colnames(r1b)

	r1a$depth_top <- 0
	r1a$depth_bottom <- 30
	r1b$depth_top <- 30
	r1b$depth_bottom <- 100

	r <- rbind(r1a, r1b)

	d <- data.frame(
		country = "Namibia",
		location =  "Omusati",
		longitude = r$lon,
		latitude = r$lat,
		geo_from_source= TRUE,
		#trial_id = r$Waypoint_No,
		depth_top = r$depth_top,
		depth_bottom = r$depth_bottom,
		soil_SOC = r$soc2,
		soil_bd = r$bd1
	)

	carobiner::write_files(path, meta, d)
}


