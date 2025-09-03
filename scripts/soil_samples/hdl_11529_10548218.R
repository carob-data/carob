# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {
  
"
TZ_TAMASA_APS_2016_Soil_MetaData
  
TAMASA Tanzania Agronomic Panel Survey (APS) Soil data for 2016. Soil samples were collected at harvest time at 0-20 and 20-50 cm depths in 560 geo-referenced fields. Soils data were analysed by mid-IR and XRF methods. (2016
"
 
  uri <- "hdl:11529/10548218"
  group <- "soil_samples"
  ff <- carobiner::get_data(uri, path, group)
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication=NA,
		project="TAMASA",
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Blessing Dzuda",
		carob_date="2025-09-02",
		completion = 100,	
		design=NA,
		notes = NA
  )
  f <- ff[basename(ff) == "TAMASA_APS_2016_Soil_Data.xlsx"]
  r <- carobiner::read.excel(f, sheet ="Revised_Data")
  r <- r[which(r$Country == "Tanzania"), ] # remove bad records
  
  d <- data.frame(
    country = r$Country,
    adm1 = r$Region,
    adm2 = r$District,
    adm3= r$Village,
    adm4= r$Ward,
    location=r$Hamlet,
    longitude = r$Longitude,
    latitude = r$Latitude,
    elevation=r$Altitude,
    soil_C = r$C,
    soil_pH = r$pH,
    soil_Al = r$Al,
    soil_B = r$B,
    soil_Ca = r$Ca,
    soil_Fe = r$Fe,
    soil_Mg = r$Mg,
    soil_Mn = r$Mn,
    soil_Na = r$Na,
    soil_S = r$S,
    soil_P=r$P,
    soil_Zn=r$Zn,
    soil_K=r$K,
    soil_N=r$N*10000,
    soil_EC=r$EC.S,
    geo_from_source = TRUE
  )
  
   #splitting depth column
   depth <- do.call(rbind, strsplit(r$Depth, "-"))
   d$depth_top <- as.numeric(depth[,1])
   d$depth_bottom <- as.numeric(depth[,2])
   
	soilmeta <- data.frame(
		variable = c("soil_Al", "soil_B", "soil_Ca", "soil_Fe", "soil_K", "soil_Mg", "soil_Mn", "soil_Na", "soil_S", "soil_P", "soil_Zn"),
		method = "Mehlich3"
	)
  
  carobiner::write_files(path, meta, d)
}
