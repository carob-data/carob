# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Dataset for: Iron, zinc and vitamin C concentration of hundreds of potato landraces from Huancavelica

Samples of 310 potato landraces were collected and analyzed for iron and zinc concentration using XRF analysis and for Vitamin C using the spectrophotometer. The results will be included in a potato catalog that will be prepared by 2020
"

	uri <- "doi:10.21223/P3/LXO6O2"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP; Yanapai",
		publication = NA,
		project = NA,
		carob_date = "2026-03-11",
		design = NA,
		data_type ="experiment",
		treatment_vars = "variety",
		response_vars = "tuber_Fe;tuber_Zn", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Data_dictionary.xls"]
	f2 <- ff[basename(ff) == "PTMinerals072018_CCASAP_exp1.xlsx"]
	f3 <- ff[basename(ff) == "PTVitaminC072018_CCASAP_exp1.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="Minimal")
	#r2b <- carobiner::read.excel(f2, sheet="Material_List")
	#r2c <- carobiner::read.excel(f2, sheet="Var_List")
	r2d <- carobiner::read.excel(f2, sheet="Fieldbook")
	#r2e <- carobiner::read.excel(f2, sheet="Summary")
	r3a <- carobiner::read.excel(f3, sheet="Minimal")
	#r3b <- carobiner::read.excel(f3, sheet="Material_List")
	#r3c <- carobiner::read.excel(f3, sheet="Var_List")
	r3d <- carobiner::read.excel(f3, sheet="Fieldbook")
	#r3e <- carobiner::read.excel(f3, sheet="Summary")

	v = data.frame(t(r2a$Value))
	names(v)= r2a$Factor
	d1 <- data.frame(
		plot_id = as.character(r2d$PLOT),
		rep = as.integer(r2d$REP),
		variety = r2d$INSTN,
		yield_moisture = r2d$AVDM,
		tuber_Fe = r2d$FeDW,
		tuber_Zn = r2d$ZnDW,
		trial_id= v$Trial_name,
		crop = v$Crop,
		planting_date = "2018",
		harvest_date = "2018-07-03",
		country = v$Country,
		adm1 = v$Admin1,
		adm2 = v$Admin2,
		adm3 = v$Admin3,
		location = v$Locality,
		elevation = as.numeric(v$Elevation),
		latitude = as.numeric(v$Latitude),
		longitude = as.numeric(v$Longitude),
		on_farm = TRUE, 
		is_survey = FALSE, 
		yield_part = "tubers", 
	  irrigated = NA, 
		geo_from_source = TRUE
	)
	
	#### 
	v = data.frame(t(r3a$Value))
	names(v)= r3a$Factor
	d2 <- data.frame(
	  plot_id = as.character(r3d$PLOT),
	  rep = as.integer(r3d$REP),
	  variety = r3d$INSTN,
	  yield_moisture = r3d$AVDM,
	  #dm_tuber_asc = r3d$ASC_DW,
	  tuber_asc = r3d$ASC_FW,
	  trial_id= v$Trial_name,
	  crop = v$Crop,
	  planting_date = "2018",
	  harvest_date = "2018-07-03",
	  country = v$Country,
	  adm1 = v$Admin1,
	  adm2 = v$Admin2,
	  adm3 = v$Admin3,
	  location = v$Locality,
	  elevation = as.numeric(v$Elevation),
	  latitude = as.numeric(v$Latitude),
	  longitude = as.numeric(v$Longitude),
	  on_farm = TRUE, 
	  is_survey = FALSE, 
	  yield_part = "roots", 
	  irrigated = NA, 
	  geo_from_source = TRUE
	)
	
	### merge d1 and d2
	d <- carobiner::bindr(d1, d2)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}

