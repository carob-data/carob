# R script for "carob"

carob_script <- function(path) {

"TAMASA Agronomy Panel Survey 2016/17 Season. This file contains the maize grain yield from approximately 578 maize fields in the Southern Highlands, Northern and Eastern Zones of Tanzania in collected May-August 2017. Maize grain yield data can be linked to associated maize yield and soil by the common HHID."

	uri <- "hdl:11529/10548242"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		project="TAMASA",
		publication=NA,
		data_organization = "CIMMYT",
		carob_contributor="Eduardo Garcia Bendito",
		carob_date="2021-09-17",
		carob_effort = NA,
		data_type="crop-cuts",
		response_vars = "none",
		treatment_vars = "none"
	)

	f <- ff[basename(ff) == "TAMASA_TZ_APS_2017_CC_MaizeYield.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Raw data", n_max = 1738)

## lon/lat reversed. but lat makes no sense.		
	d <- data.frame(
		yield = r$`Grain yield (kg/ha@12.5%)`, 
		latitude = r$`QRcode Cobs`,
		longitude = r$Latitude,
		geo_from_source = TRUE
	)

	d$country <- "Tanzania"
	d$trial_id <- paste0(d$HHID, "-", d$QID)
	d$planting_date <- "2016-05-01"
	d$harvest_date <- "2016-12-01"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$irrigated <- as.logical(NA)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	

   # d <- d[!is.na(d$longitude) & !is.na(d$latitude),]
    d <- d[!is.na(d$yield),]
	carobiner::write_files(meta, d, path=path)
}
