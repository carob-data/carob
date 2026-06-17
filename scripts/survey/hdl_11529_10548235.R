# R script for "carob"

carob_script <- function(path) {

"CIMMYT and IITA. 2017. TAMASA Tanzania Agronomic Panel Survey for 2016. Version 1. Taking Maize Agronomy to Scale Project, CIMMYT and IITA. Tabular dataset (2016)"

	uri <- "hdl:11529/10548235"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		project="TAMASA",
		publication=NA,
		data_organization = "CIMMYT;IITA",
		carob_contributor="Effie Ochieng'",
		carob_date="2021-09-17",
		carob_effort = NA,
		data_type="crop-cuts",
		response_vars = "none",
		treatment_vars = "none"
	)

	f <- ff[basename(ff) == "TZ_TAMASA_APS_2017_Yield_MetaData.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Corrected-Raw-Data", n_max = 1835)

	d <- data.frame(yield = r$`Grain yield (kg/ha@12.5%)`)
	d$country <- "Tanzania"
	d$adm1 <- r$Zone
	d$adm2 <- r$Region 
	d$adm3 <- r$District
	d$adm4 <- r$Ward 
	
	d$location <- r$Village
	d$site <- r$Hamlet
	d$trial_id <- paste0(r$HHID, "-", r$QID)
	d$latitude <- r$Latitude
	d$longitude <- r$Longitude
	d$elevation <- r$Altitude
	
	d$planting_date <- "2016-05-01"
	d$harvest_date <- "2016-12-01"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- as.logical(NA)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	d$crop <- "maize"
	d$yield_part <- "grain"

	d <- d[!is.na(d$yield), ]
	
	carobiner::write_files(meta, d, path=path)
}
