# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Multi-country Dataset of Agronomic, Soil, and Climatic Drivers of Rice yield in Smart-Valleys Systems and Adjacent Non-Developed Inland Valleys Systems

This dataset contains agronomic, environmental, soil, management, and climatic data collected from inland valley rice production systems across multiple countries in West Africa under the Smart-Valleys approach. The dataset includes information from both Smart-Valleys-developed sites and adjacent non-developed inland valleys, enabling comparative analyses of rice productivity and system performance. Variables comprise rice grain yield, fertilizer application rates and timing, crop management practices, disease control, soil physicochemical properties, seasonal rainfall characteristics, temperature, and other environmental indicators influencing rice production. The dataset was compiled from multi-year field observations and surveys conducted across diverse agroecological conditions to assess the drivers of yield variability and the effectiveness of low-cost water control technologies in inland valleys. It supports research on climate-smart agriculture, sustainable intensification, land and water management, yield gap analysis, and adaptation strategies for improving rice productivity, resilience, and food security in sub-Saharan Africa.
"

	uri <- "doi:10.5281/zenodo.20345853"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = NA,
		response_vars = NA, 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-08-17",
		carob_completion = 100,	
		carob_effort = 5
	)
	
	f <- ff[basename(ff) == "Smart-Valleys%20dataset.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
	  country=r$Country,
	  longitude=r$Longitude,
	  latitude=r$Latitude,
	  crop="rice",
	  variety=r$Variety,
	  planting_method=tolower(r$Seedtype),
	  N_fertilizer=r$`N fertilizer rate`,
	  P_fertilizer=r$`P fertilizer rate`,
	  K_fertilizer=r$`K fertilizer rate`,
	  yield_developed_valleys=r$`Yield Smart-Valleys`*1000,
	  yield_non_developed_valleys=r$`Yield_non developed IV`*1000,
	  OM_used=r$`Organic amendment`,
	  soil_N=r$`Soil N`,
	  soil_P=r$`Soil P`,
	  soil_K=r$`Soil K`,
	  soil_Mg=r$`Soil Mg`,
	  soil_Ca=r$`Soil Ca`,
	  soil_Fe=r$`Soil Fe`,
	  soil_S=r$`Soil S`,
	  soil_Zn=r$`Soil Zn`,
	  soil_SOC=r$SOC,
	  soil_pH=r$`Soil pH`,
	  soil_clay=r$CLAY,
	  soil_sand=r$SAND,
	  soil_silt=r$SILT,
	  soil_CEC=r$CEC,
	  soil_bd=r$`Bulk density`
	)
	
	d$trial_id <- paste(d$country, 1:nrow(d), sep = "_")
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
	d$planting_date <- NA
	d$harvest_date  <- NA
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	d$planting_method <- gsub("directseeding","direct seeding",d$planting_method)
	d$planting_method <- gsub("broadcastseeding","broadcasting",d$planting_method)
	d$country <- gsub("Cote d'Ivoire","Côte d'Ivoire",d$country)
	
	d$country[d$longitude==1.636887] <- "Benin"
	d$country[d$latitude==7.32685] <- "Benin"
	
	yield_cols <- c("yield_developed_valleys", "yield_non_developed_valleys")
	
	d$row_id <- seq_len(nrow(d))
	
	d <-reshape(d,varying=yield_cols,v.names="yield",timevar="valley_development",times=c("developed inland valleys","non developed inland valleys"),idvar="row_id",direction="long")
	
	rownames(d) <- NULL
	d$row_id <- NULL

	carobiner::write_files(path, meta, d)
}
