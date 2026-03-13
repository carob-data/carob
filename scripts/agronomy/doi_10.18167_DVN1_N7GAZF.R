# R script for "carob"
# license: GPL (>=3)

## ISSUES

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
		carob_date = "2026-03-10",
		design = "component omission trial",
		data_type = "experiment",
		treatment_vars = "land_prep_method;crop_rotation",
		response_vars = "yield;soil_NH4;soil_NO3", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Shumba_et_al_Raw_data.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="GHG_Data")
	r2 <- carobiner::read.excel(f1, sheet="Rainfall")


	d1 <- data.frame(
		location = r1$Site,
		treatment_code = r1$Treatment_ID,
		treatment = ifelse(grepl("CT", r1$Treatment_ID), "conventional",
		            ifelse(grepl("^CTR$", r1$Treatment_ID), "conventional-rotation",
		            ifelse(grepl("^NT$", r1$Treatment_ID), "No tillage",
		            ifelse(grepl("^NTM$", r1$Treatment_ID), "No tillage-mulch", 
		            ifelse(grepl("^NTR$", r1$Treatment_ID), "No tillage-rotation", "No tillage-mulch-rotation"))))),
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
		crop = "maize", 
		yield_part = "grain", 
		yield_moisture = as.numeric(12.5), # from publication
		yield_isfresh = as.logical(FALSE), # from publication: "adjust the fresh weight to 12.5% standard maize grain moisture"
		irrigated = FALSE, # No supplemental water as per publication
		country = "Zimbabwe"
	)

	### Adding yield from publication
	
	yd <- data.frame(
	  yield = c(1848.2, 2291.5,1815.6,  2868.3, 2391.1, 2572.6,
	            2241.7, 2229.8, 2152.1, 1726.4, 2279.6, 1409.5,
	            1800.2, 4116.6, 2355.3, 3122.8, 3880.6, 5014.1,
	            4829.8, 5616.5, 4004.6, 4578.9, 5465.7, 5544.1),
	  season = c(rep("2019/20", 6), rep("2020/21", 6), rep("2019/20", 6), rep("2020/21", 6)),
	  location = c(rep("DTC",12), rep("UZF", 12)),
	  treatment_code = rep(c("CT", "CTR", "NT", "NTM", "NTR", "NTMR"), time=4)
	)

	d	<- merge(d1, yd, by= c("season", "treatment_code", "location"), all.x = TRUE)
  d$location <- ifelse(grepl("DTC", d$location), "Domboshava Training Centre", "University of Zimbabwe Farm")
  
  # The data only reports maize yields (rows). Interrow treatments are reffered to cowpea, without reported yield, so could be dropped
  d <- d[!grepl("interow", d$trial_id), ]
  
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


