# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. i intentionally left out particle size...although the proportions are there, no data states which soil types are they


carob_script <- function(path) {

"
TAMASA Nigeria. Nutrient omission trials (NOT) soil data, 2015

This dataset consists of Soil Analysis for the 95 on-farm field trial sites. The dataset is divided into six sheets; Metadata, variable, Raw Data, Data Summary and Data History. 'Metadata' give summary description of the dataset. 'Variables' give the name, unit, description and method of determination of the variables in the dataset. 'Raw Data' contain the raw dataset, Data summary gives the summary statistics of each of measured variable in the raw dataset. And 'Data History' describe the historical changes and exchange in the raw dataset
"

	uri <- "hdl:11529/280439"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "BUK; IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-06-21",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f <- ff[basename(ff) == "NG_NOT_SOIL_2015.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")
	
	d <- data.frame(
	  country=r$COUNTRY,
	  adm1=r$STATE,
	  adm3=r$LGA,
	  adm4=r$COMMUNITY,
	  longitude=r$Longitude,
	  latitude=r$Latitude,
	  soil_pH=r$`pH (Water)`,
	  soil_SOM=r$OC,
	  soil_N=r$N,
	  soil_P=r$MehP,
	  soil_Ca=r$Ca*200,#converting cmol/kg to mg/kg
	  soil_Mg=r$Mg*120,#converting cmol/kg to mg/kg
	  soil_K=r$K*390,#converting cmol/kg to mg/kg,
	  soil_Na=r$Na*230,#converting cmol/kg to mg/kg
	  soil_ex_acidity=r$`Exch. Acidity`,
	  soil_ECEC=r$ECEC,
	  soil_Zn=r$Zn,
	  soil_Cu=r$Cu,
	  soil_Mn=r$Mn,
	  soil_Fe=r$Fe,
	  soil_bd=r$BD,
	  soil_B=r$B,
	  soil_S=r$S
	)
	
	d$latitude  <- as.numeric(gsub("E", "", d$latitude))
	d$longitude <- as.numeric(gsub("N", "", d$longitude))
	d$geo_from_source <- TRUE
	d$irrigated <- FALSE
	d$planting_date <- as.character(NA)
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}
