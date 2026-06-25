# R script for "carob"


carob_script <- function(path) {

"This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Yaqui Valley. (2022-03-28)"

	uri <- "hdl:11529/10548652"
	group <- "fertilizer"
	ff <- carobiner::get_data(uri, path, group)
	

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
		project=NA,
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "Maize experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Yaqui Valley",
		data_organization = "CIMMYT",
   		data_type="experiment",
		carob_contributor="Blessing Dzuda", 
		carob_effort = NA,
		carob_date = "2023-11-02"
	)

	
	ss <- c("AB250.xlsx", "", 
	"AB250.xlsx", sheet = "AB250C1", 
	"AC250.xlsx", "", 
	"AC250.xlsx", sheet = "AC250C", 
	"AD250.xlsx", "", 
	"AD250.xlsx", sheet = "AD250C", 
	"U250.xls", sheet = "Data", 
	"V250 .xls", "", 
	"V250 .xls", sheet = "Data-250p1", 
	"W250.xls", "", 
	"W250.xls", sheet = "Data-250 P1", 
	"X250.xls", "", 
	"X250.xls", sheet = "Data 250 C2", 
	"X250.xls", sheet = "Data 250 P1", 
	"X250.xls", sheet = "Data 250 P2", 
	"Z250 .xlsx", "", 
	"Z250 .xlsx", sheet = "Data 250 C2", 
	"Z250 .xlsx", sheet = "Data 250 P1", 
	"Z250 .xlsx", sheet = "Data 250 P2") 

	
	
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE	
	d$irrigated <- TRUE
  
## the treatment code	
	d$treatment <- d$Trt
	d$rep <- d$Rep
	d$country <- "Mexico"
	d$location <- "Yaqui Valley"
	d$adm1 <- "Sonora"
	d$elevation <- 3	
	d$longitude <- -110.38863
	d$latitude <- 27.36915
	d$crop <- "Maize"
	d$yield_part <- "grain"
	d$inoculated <- FALSE
  
	
	d$variety <- 
	
	
	d$N_fertilizer <- d$`N at Planting`
	d2$N_fertilizer <- d2$`N at Planting`
	d3$N_fertilizer <- d3$`N at Planting (kg/ha)`
	d4$N_fertiliser <- d4$`N at Planting (kg/ha)`
	d5$N_fertiliser <- d5$`N at Planting (kg/ha)`
	d6$N_fertiliser <- d6$`N at Planting (kg/ha)`
	d7$N_fertiliser <- d7$`N at Planting`
	d8$N_fertiliser <- d8$`N at Planting`
	d9$N_fertiliser <- d9$`N at Planting`
	d10$N_fertiliser <- d10$`Total N`
	d11$N_fertiliser <- d11$`Total N`
	d12$N_fertiliser <- d12$`Total N TRT`
	d13$N_fertiliser <- d13$`N at Planting`
	d14$N_fertiliser <- d14$`Total N TRT`
	d15$N_fertiliser <- d15$`Total N TRT`
  d16$N_fertiliser <- d16$`N at Planting`
  d17$N_fertiliser <- d17$`N at Planting`
  d18$N_fertiliser <- d18$`N at Planting`
  d19$N_fertiliser <- d19$`N at Planting`
	
	d$yield <- d$`Yield at 14 % humidity`
	d2$yield <- d2$`Yield at 14 % humidity`
	d3$yield <- d3$`Yield at 14 % humidity`
	d4$yield <- d4$`Yield at 14 % humidity`
	d5$yield <- d5$`Yield at 14% hum`
	d6$yield <- d6$`Yield at 14% hum`
	d7$yield <- d7$`Yield at 14% hum`
	d8$yield <- d8$`Yield at 14 % humidity`
	d9$yield <- d9$`Yield at 14 % humidity`
	d10$yield <- d10$`Yield at14% hum`
	d11$yield <- d11$`Yield at14% hum`
	d12$yield <- d12$`Yield at 14 % humidity`
	d13$yield <- d13$`Yield at 14 % humidity`
	d14$yield <- d14$`Yield at 14 % humidity`
	d15$yield <- d15$`Yield at 14 % humidity`
	d16$yield <- d16$`Yield at 14 % humidity`
	d17$yield <- d17$`Yield at 14 % humidity`
	d18$yield <- d18$`Yield at 14 % humidity`
	d19$yield <- d19$`Yield at 14 % humidity`
	
	d$plant_density <- d$`Harvest Pop.`
	d2$plant_density <- d2$`Harvest Pop.`
	d3$plant_density <- d3$`Harvest Pop.`
	d4$plant_density <- d4$`Harvest Pop.`
	d5$plant_density <- d5$`Harvest Pop.`
	d6$plant_density <- d6$`Harvest Pop.`
	d7$plant_density <- d7$`Harvest Pop.`
	d8$plant_density <- d8$`Harvest Pop.`
	d9$plant_density <- d9$`Harvest Pop.`
	d10$plant_density <- d10$`Harvest Pop.`
	d11$plant_density <- d11$`Harvest Pop.`
	d12$plant_density <- d12$`Harvest Pop.`
	d13$plant_density <- d13$`Harvest Pop.`
	d14$plant_density <- d14$`Harvest Pop.`
	d15$plant_density <- d15$`Harvest Pop.`
	d16$plant_density <- d16$`Harvest Pop.`
	d17$plant_density <- d17$`Harvest Pop.`
	d18$plant_density <- d18$`Harvest Pop.`
	d19$plant_density <- d19$`Harvest Pop.`
	
	d$dmy_total <- d$BIOMASS
	d2$dmy_total <- d2$BIOMASS
	d3$dmy_total <- d3$BIOMASS
	d4$dmy_total <- d4$BIOMASS
	d5$dmy_total <- d5$BIOMASS
	d6$dmy_total <- d6$BIOMASS
  d7$dmy_total <- d7$BIOMASS
  d8$dmy_total <- d8$BIOMASS
  d9$dmy_total <- d9$BIOMASS
  d10$dmy_total <- d10$Biomass
  d11$dmy_total <- d11$Biomass
	d12$dmy_total <- d12$BIOMASS
	d13$dmy_total <- d13$BIOMASS
	d14$dmy_total <- d14$BIOMASS
	d15$dmy_total <- d15$BIOMASS
	d16$dmy_total <- d16$BIOMASS
	d17$dmy_total <- d17$BIOMASS
	d18$dmy_total <- d18$BIOMASS
	d19$dmy_total <- d19$BIOMASS
  

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	
	d$planting_date <- 2013
	d2$planting_date <- 2013
	d3$planting_date <- 2014
	d4$planting_date <- 2014
	d5$planting_date <- 2015
	d6$planting_date <- 2015
	d7$planting_date <- 2009
	d8$planting_date <- 2007
	d9$planting_date <- 2007
	d10$planting_date <- 2008
	d11$planting_date <- 2008
	d12$planting_date <- 2009
	d13$planting_date <- 2009
	d14$planting_date <- 2009
	d15$planting_date <- 2009
	d16$planting_date <- 2011
	d17$planting_date <- 2011
	d18$planting_date <- 2011
	d19$planting_date <- 2011
	
	d$harvest_date  <- as.character(as.Date(    ))

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 
	
	d<- d[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertilizer","yield","yield_part","dmy_total")]
	
	
	carobiner::write_files(meta, d, path=path)
}

