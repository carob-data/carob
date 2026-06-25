# R script for "carob"


carob_script <- function(path) {
  
"Replicated crop-cuts and related agronomy data for maize from 142 farmers' fields in Southern, Eastern and Northern Zones in 2014/2015 season. The soils data from these fields can be found here. (2017-12-17)"
  
	uri <- "hdl:11529/10548143"
	group <- "survey"
	
	ff	<- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication =NA,
		project = NA,
		data_type="crop-cuts",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-06-04",
		carob_effort = NA
	)
	
	f <- ff[basename(ff) == "TAMASA_TZ_CC_Yield_2015.xlsx"]
	r <- suppressWarnings(carobiner::read.excel(f, sheet = "Data"))
	r <- r[1:425, ]

	d <- data.frame(
		country=r$Country,
		adm1=r$Zone,
		adm2=r$District,
		adm3=r$Village,
		adm4=r$Ward,
		latitude=r$Latitude,
		longitude=r$Longitude,
		elevation=r$Altitude,
		crop="maize",
		plant_density=r$`Plant Stands`,
		yield=r$`Grain yield (kg/ha@12.5%)`
	)
	
# Planting date 2014 because the cropping season was 2014/15.
	d$planting_date="2014"

	d$trial_id= as.character(1:nrow(d))
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$yield_part <- "grain"
	d$plant_density <- d$plant_density*400
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}


