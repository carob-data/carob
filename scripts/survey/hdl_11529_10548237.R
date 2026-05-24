# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
TZ TAMASA APS 2017 MZ Biomass Yield MetaData

Survey at multiple locations in Tanzania  (680 to 2100 m)  to establish baseline biomass yields at farm level.
"


	uri <- "hdl:11529/10548237"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TZ TAMASA APS",
		carob_date = "2026-05-21",
		design = NA,
		data_type = "survey",
		treatment_vars = "elevation",
		response_vars = "dmy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "TZ_TAMASA_APS_2017_MZ_Biomass_Yield_MetaData.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="Medadata")
	r2 <- carobiner::read.excel(f1, sheet="Protocol")
	r3 <- carobiner::read.excel(f1, sheet="Variables")
	r4 <- carobiner::read.excel(f1, sheet="Corrected-Raw-Data", na = c("n/a", "`"))



	d1 <- data.frame(
		country = r4$Country,
		location_id = r4$District,
		hhid = r4$HHID,
		trial_id = paste(r4$`Farmer Name`, r4$QID, sep = "-"),
		latitude = r4$Latitude,
		longitude = r4$Longitude,
		elevation = r4$Altitude,
		farmland = r4$`Area by Farmer_est`,
		crop_cut = grepl("Yes",r4$Cropcut_present),
		yield_moisture = r4$`MC (%)`,
		dmy_total = r4$`Biomass dry weight (kg/ha)`,
		planting_date = as.character(NA), 
		crop = "maize",
		on_farm = FALSE, 
		is_survey = TRUE, 
		yield_part = "none", 
		yield = as.numeric(NA), 
		irrigated = NA, 
		geo_from_source = TRUE
	)

	d <- d1[which(d1$country=="Tanzania"),]
	
	d <- d[!is.na(d$elevation),]
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	
	carobiner::write_files(path, meta, d)
}


