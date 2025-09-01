# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. adm1 and adm2 are alpha-numeric and there is no legend to link the alpha-numeric characters to their actual places 

carob_script <- function(path) {


"
TAMASA Tanzania. Agronomy Panel Survey (APS) 2017. Crop Cut & Soil Sample.
  
TAMASA Agronomy Panel Survey 2016/17 Season. This file contains the results of soil analysis at 0-20 and 20-50 cm soil depths from approximately 580 maize fields in the Southern Highlands, Northern and Eastern Zones of Tanzania in May-August 2017. Soil data can be linked to associated maize yield and biomass by the common HHID. (2018-11-19)
"

	uri <- "hdl:11529/10548153"
	group <- "soil_samples"

	ff  <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-08-31",
		notes = NA,
		design = NA
	)

	f <- ff[basename(ff) == "TAMASA_TZ_APS_Soil_2017.xlsx"]
	r <- carobiner::read.excel(f, sheet ="Revised_Data")
	r <- r[which(r$Country == "Tanzania"), ] # remove empty rows and rows with stats
	d <- data.frame(
		country = r$Country,
		adm1=r$Region,
		adm2=r$District,
		latitude=r$Latitude,
		longitude=r$Longitude,
		elevation=r$Altitude,
		soil_C=r$C,
		soil_pH=r$pH,
		soil_Al = r$Al,
		soil_B=r$B,
		soil_Ca = r$Ca,
		soil_EC =r$EC.S,
		soil_Fe = r$Fe,
		soil_K = r$K,
		soil_Mg = r$Mg,
		soil_Mn = r$Mn,
		soil_N=r$N*10000,#converting % to mg/kg
		soil_Na = r$Na,
		soil_S = r$S,
		soil_P=r$P,
		soil_Zn=r$Zn,
		geo_from_source = TRUE
	) 

	i <- grepl("50", r$Depth)
	d$depth_top <- ifelse(i, 20, 0)
	d$depth_bottom  <- ifelse(i, 50, 20)

	carobiner::write_files(path, meta, d)
}
