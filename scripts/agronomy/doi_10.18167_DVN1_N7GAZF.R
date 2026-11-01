# R script for "carob"
# license: GPL (>=3)

## ISSUES
## Treatment-mean yields from the paper are not in the GHG sheet. They are not copied onto the daily gas and mineral-N rows.

carob_script <- function(path) {

"
Data for: Long-term tillage, residue management and crop rotation impacts on N2O and CH4 emissions on two contrasting soils in sub-humid Zimbabwe

These are the raw data of the paper 'Long-term tillage, residue management and crop rotation impacts on N2O and CH4 emissions on two contrasting soils in sub-humid Zimbabwe' authored by Armwell Shumba, Regis Chikowo, Marc Corbeels, Johan Six, Christian Thierfelder, Rémi Cardinael
"

	uri <- "doi:10.18167/DVN1/N7GAZF"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "UZIM; CIRAD; ETH; CIMMYT",
		publication ="doi:10.1016/j.agee.2022.108207",
		project = NA,
		carob_effort = NA,
		carob_date = "2026-03-10",
		design = "component omission trial",
				data_type = "experiment",
		treatment_vars = "land_prep_method;crop_rotation",
		response_vars = "yield;soil_NH4;soil_NO3", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		notes = "LLM-review: [medium, metadata] trial_id is position plus treatment number, so it is per plot and shared across seasons. on_farm is TRUE for the Domboshava and University of Zimbabwe stations. [medium, yield] yield_isfresh is FALSE together with 12.5 percent moisture. The rotation contrast is only inside the treatment string."
	)
	

	f1 <- ff[basename(ff) == "Shumba_et_al_Raw_data.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="GHG_Data")
	r2 <- carobiner::read.excel(f1, sheet="Rainfall")


	d1 <- data.frame(
		location = r1$Site,
		treatment_code = r1$Treatment_ID,
		treatment = ifelse(grepl("^CTR$", r1$Treatment_ID), "conventional-rotation",
		            ifelse(grepl("^CT$", r1$Treatment_ID), "conventional",
		            ifelse(grepl("^NT$", r1$Treatment_ID), "No tillage",
		            ifelse(grepl("^NTM$", r1$Treatment_ID), "No tillage-mulch",
		            ifelse(grepl("^NTR$", r1$Treatment_ID), "No tillage-rotation",
		            ifelse(grepl("^NTMR$", r1$Treatment_ID), "No tillage-mulch-rotation", NA)))))),
		land_prep_method = ifelse(grepl("^CT$|^CTR$", r1$Treatment_ID), "conventional", "none"),
		rep = as.integer(r1$Replicate),
		season = r1$Season,
		planting_date = as.character(r1$Sowing_date),
		DAP = as.integer(r1$Days_after_sowing),
		soil_NO3 = r1$Soil_NO3_N_ppm,
		soil_NH4 = r1$Soil_NH4_N_ppm,
		soil_N = r1$Tot_Min_N_ppm,
		soil_bd = r1$BD_g_cm3,
		crop_rotation = ifelse(grepl("CTR|NTR|NTR", r1$Treatment_ID), "cowpea;maize", "none"),
		
		herbicide_used = TRUE,
		herbicide_product = "glyphosate", ### from publication
		herbicide_amount = 1.025,
		N_fertilizer = 116.6,
		P_fertilizer = 10.6,
		K_fertilizer = 9.6,
		plant_density = 44444,
		plant_spacing = 25,
		row_spacing = 90,
		trial_id = paste(r1$Position, r1$Treatment_no, sep = "-"), 
		on_farm = TRUE, 
		is_survey= FALSE, 
		crop = ifelse(r1$Position == "interow", "cowpea", "maize"), 
		yield_part = "grain", 
		yield_moisture = ifelse(r1$Position == "interow", NA, 12.5), # publication: 12.5% maize grain moisture
		yield_isfresh = ifelse(r1$Position == "interow", NA, FALSE),
		irrigated = FALSE, # No supplemental water as per publication
		country = "Zimbabwe"
	)

	d <- d1
	d$location <- ifelse(grepl("DTC", d$location), "Domboshava Training Centre", "University of Zimbabwe Farm")
  
	### Adding longitude and latitude
	
	geo <- data.frame(
	   location = c("Domboshava Training Centre","University of Zimbabwe Farm"),
	   longitude = c(31.1372, 31.01318),
	   latitude = c(-17.6072, -17.7101),
	   geo_from_source = FALSE
	)
	
	d	<- merge(d, geo, by= "location", all.x = TRUE)
	
	d$season <- d$treatment_code <- NULL
	
	### Process weather data 
	
	dw <- data.frame(
	  location = ifelse(grepl("DTC", r2$Site), "Domboshava Training Centre", "University of Zimbabwe Farm"),
	  date = as.character(r2$Date),
	  prec = r2$Cumul_Rainfall_mm,
	  temp = as.numeric(NA),
	  longitude = ifelse(grepl("DTC", r2$Site),31.1372, 31.01318 ),
	  latitude = ifelse(grepl("DTC", r2$Site),-17.6072, -17.7101 ),
	  country = "Zimbabwe",
	  geo_from_source = FALSE
	) 
	
carobiner::write_files(path, meta, d, wth = dw)

}


