# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
TZ TAMASA APS 2016  pigeon peas Yield MetaData

Survey at multiple locations in Tanzania  (600 to 2100 m)  to establish baseline yields at farm level.
"

	uri <- "hdl:11529/10548236"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA APS 2016",
		carob_date = "2026-05-20",
		carob_effort = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "elevation",
		response_vars = "dmy_total", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "TZ_TAMASA_APS_2017_PP_Biomass_Yield_MetaData_V2.xls"]
	f2 <- ff[basename(ff) == "TZ_TAMASA_APS_2017_PP_Biomass_Yield_MetaData_V2.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2, sheet="Medadata")
	r3 <- carobiner::read.excel(f2, sheet="Protocol")
	r4 <- carobiner::read.excel(f2, sheet="Variables")
	r5 <- carobiner::read.excel(f2, sheet="Corrected-Raw-Data")
	#r6 <- carobiner::read.excel(f2, sheet="Sheet1")


	d1 <- data.frame(
	  country = r5$Country,
	  location_id = r5$District,
	  hhid = r5$HHID,
	  trial_id = paste(r5$`Farmer Name`,  r5$QID, sep = "-"),
	  latitude = r5$Latitude,
	  longitude = r5$Longitude,
	  elevation = r5$Altitude,
	  farmland = r5$`Area by Farmer_est`,
	  crop_cut = grepl("Yes", r5$Cropcut_present),
	  dmy_total = r5$`Biomass dry weight (kg/ha)`,
	  yield_moisture = r5$`MC (%)`,
	  crop = "pigeon pea",
	  planting_date = as.character(NA), 
	  on_farm = FALSE, 
	  is_survey = TRUE, 
	  yield_part = "none", 
	  yield = as.numeric(NA), 
	  irrigated = NA, 
	  geo_from_source = TRUE
	  
	)
	
	d <- d1[which(d1$country=="Tanzania"),]
	
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}


