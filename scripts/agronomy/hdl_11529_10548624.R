# R script for "carob"

carob_script <- function(path) {

"Improvements to local maize production systems need to be developed and validated together with the local farmers in order to generate a lasting impact. Therefore, CIMMYT coordinated, as part of the work in its innovation hub in Chiapas, the installation of side-by-side comparisons of improved practices (innovation modules) as well as field demonstrations of these practices in farmers’ fields (extension areas). In all these fields, agronomic data were captured by the farm advisors and captured into the Bitacora Electronica MasAgro (BEM) electronic logbook. This dataset contains a subset of variables from the BEM database for Chiapas in the years 2012-2018. Agronomic data including yield, cultivar, tillage and fertilization were extracted from BEM. Additionally, slope, texture, soil organic matter concentration, cation exchange capacity and pH were extracted from INEGI. Weather data in 10-day intervals were extracted from DAYMET. The database contains 4585 cropping events collected over seven years in the state of Chiapas, Mexico. (2021-10-26)"


	uri <- "hdl:11529/10548624"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIMMYT;UIUC;CIAT;ICESI",
		publication = NA,
		project = NA,
		data_type ="on-farm experiment",
		treatment_vars = "land_prep_method",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-08-06",
		notes= NA
	)
	
	f <- ff[basename(ff) == "Chiapas Field Data 2012-2018.xlsx"]
	r <- carobiner::read.excel(f, sheet ="Data")

	d <- data.frame(
		country = "Mexico",
		crop="maize",
		yield=r$YIELD*1000,
		planting_date=r$Year,
		latitude=r$Lat,
		longitude=r$Long,
		elevation=r$Elev,
		variety=r$Cultivar,
		land_prep_method=tolower(r$Tillage),
		N_fertilizer=r$Nitrogen,
		P_fertilizer=r$Phosphorus/2.29,
		K_fertilizer=r$Potassium/1.2051,
		soil_clay=r$Clay,
		soil_CEC=r$CEC,
		soil_SOM=r$SOM,
		soil_pH=r$PH,
		yield_part="grain"
	)
		
	d$trial_id <- as.character(as.integer(as.factor(paste(d$planting_date, d$longitude, d$latitude))))
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <-FALSE
	d$geo_from_source <-TRUE
  
	d$land_prep_method <- gsub("reduced", "reduced tillage", d$land_prep_method)
	d$land_prep_method <- gsub("no-till", "none", d$land_prep_method)
	d$planting_date <- as.character(d$planting_date)
	

# all scripts must end like this
	carobiner::write_files(path, meta, d)
}
