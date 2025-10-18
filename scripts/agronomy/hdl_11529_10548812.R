# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"Wheat experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Oaxaca
  
This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Oaxaca. (2022-10-17)"

	uri <- "hdl:11529/10548812"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		completion = 99,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-10-17",
		notes = NA, 
		design = NA
	)
	

	f <- ff[basename(ff) == "GreenSeeker Oaxaca 2016-2019.xlsx"]
 
#Function to standardize other sheets with same structure
  make_standard_df <- function(file, sheet_name) {
    r <- carobiner::read.excel(file, sheet = sheet_name, skip = 1)
    
    # identify the last column (yield)
    yield_col <- r[, ncol(r)]
    
    data.frame(
      country = "Mexico",
      adm1 = r$Municipality,
      location = r$Locality,
      latitude = r$Latitude,
      longitude = r$Longitude,
      planting_date = as.character(r$`Planting Date`),
      land_prep_method = tolower(r$Tillage),
      variety = r$Hibrid,
      treatment= as.character(r$`Rate N\r\n(kg/ha)`),
      #planting_method = r$`Planting method`, i think this one also falls under land_prep_method, no publication for clarity 
      seed_rate = r$`planting density (Kg/ha)`,
      rep = as.integer(r$REP),
      N_fertilizer = r$`Rate N\r\n(kg/ha)`,
      yield = yield_col
    )
  }
		
  x1 <- make_standard_df(f, "2016")  
  x2 <- make_standard_df(f, "2017")
  x3 <- make_standard_df(f, "2018")
  x4 <- make_standard_df(f, "2019")
  
  d <-rbind(x1,x2,x3,x4)
  d$crop <- "wheat"
  d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
  d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	
	d$land_prep_method <- gsub("conservation","minimum tillage", d$land_prep_method)
	
	carobiner::write_files(path, meta, d)
}
