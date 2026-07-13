# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. extreme values in soil_Ca and soil_EC originates from the raw data
# 2. NA values in sample_id originates from raw data 


carob_script <- function(path) {
  
"
TAMASA Tanzania. Soil data from farmers' maize fields in 2014/15 season

Soil analysis from 0-20 cm and 20-50 cm depths in 140 farmer fields from the in Southern Highlands, Eastern and Northern Zones of  Tanzania in 2014/2015.
"
  
	uri <- "hdl:11529/2188664"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT; IARI; SARI",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-06-28",
		carob_completion = 100,	
		carob_effort = 3
  )
  
  
  f1 <- ff[basename(ff) == "TAMASA_TZ_CC_Soil_2015.xlsx"]
  r <- carobiner::read.excel(f1, sheet="Data")
  r <- r[1:284, ]#removing non useful figures at the bottom 
  r <- r[!is.na(r$C), ]#raw dataset has empty rows with no useful info
  
  d <- data.frame(
    sample_id=r$SSID,
    date=r$Date,
    country = r$Country,
    adm1 = r$Region,
    adm2 = r$District,
    adm3 = r$Ward,
    adm4 = r$Village,
    latitude = r$Latitude,
    longitude = r$Longitude,
    elevation = r$Altitude,
    soil_SOC=r$C,
    soil_pH=r$pH,
    soil_Al=r$Al,
    soil_Ca=r$Ca,
    soil_EC=r$EC.S,
    soil_S = r$S,
    soil_Mn=r$Mn,
    soil_P=r$P,
    soil_Zn=r$Zn,
    soil_K=r$K,
    soil_Mg=r$Mg,
    soil_Na=r$Na,
    soil_Fe=r$Fe,
    soil_B=r$B,
    soil_N=r$N
  )
  
  splits <- splits <- strsplit(trimws(as.character(r$Depth)), "\\s*[-–]\\s*")
  
  d$depth_top    <- as.numeric(trimws(sapply(splits, `[`, 1)))
  d$depth_bottom <- as.numeric(trimws(sapply(splits, `[`, 2)))
  
  soilmeta <- data.frame(
    variable = c("soil_Al", "soil_B", "soil_Ca", "soil_Fe", "soil_K", "soil_Mg", "soil_Mn", "soil_Na", "soil_S", "soil_Zn","soil_P"),
    method = c("Mehlich3","Mehlich3","Mehlich3","Mehlich3","Mehlich3","hydrometallurgy","Mehlich3","Mehlich3","Mehlich3","Mehlich3","Mehlich3")
  )
  
  d$geo_from_source <- TRUE
  
  carobiner::write_files(path, meta, d)
}
