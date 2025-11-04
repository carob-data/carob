# R script for "carob"

# phenology from metadata:
#V4-V5	6/22/2016
#V7-V8	6/28/2016
#V9-V10	7/1/2016
#V11	7/6/2016
#V-12	7/14/2016


carob_script <- function(path) {

"This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Chihuahua. (2022-07-06)"

	uri <- "hdl:11529/10548722" 
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield;NDVI", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-09-03",
		notes = NA
	)
	
	f <- ff[basename(ff) == "GreenSeeker Chihuahua 2016.xlsx"]
   r <- carobiner::read.excel.hdr(f, sheet = "Data-Buenaventura", hdr=1, skip=3)

	d <- data.frame(
		country = "Mexico",
		crop= "maize", 
		rep=as.integer(r$Rep_),
		treatment = as.character(r$TRT_),
		plot_area = r$Plot.size_m.2,
		yield=as.numeric(r$Yield.at.14pct.hum_kg.ha),
		N_fertilizer = as.numeric(gsub("KgN", "", r$N.at.Planting_kg.ha))
	)
	d$record_id <- 1:nrow(d)

	nv <- grep("NDVI", names(r), value=TRUE)
	ndvi <- r[, nv]
	ndvi$record_id <- 1:nrow(ndvi)
	dates <- as.numeric(gsub("NDVI_", "", nv)) + as.Date("1900-01-01") - 2
	ndvi <- reshape(ndvi, direction="long", varying=nv, v.names="NDVI", times = as.character(dates))
  ndvi$id <- NULL
  
  d$trial_id <- "1"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	
	d$longitude <- -106.069099
	d$latitude <-  28.632996
	d$geo_from_source <- FALSE

	d$planting_date <- "2016-05-12"
	d$irrigation_dates <- "2016-06-21"    
	d$land_prep_method <- "conventional"
	d$yield_part <- "grain"

	# from metadata: 104 kg/ha MAP
	d$N_fertilizer <- (104*0.11) + d$N_fertilizer
	d$P_fertilizer <- (104*0.52) / 2.29
	d$K_fertilizer <- 0
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d, long=ndvi)
}
