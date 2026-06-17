# R script for "carob"


carob_script <- function(path) {

"Agronomy of 100 fields is part of APS(AgronomyPanelSurvey) Study (2016)"

	uri <- "hdl:11529/10548392"
	group <- "crop_cuts"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		project= "TAMASA",
		publication= NA,
		data_organization = "CIMMYT",
   		data_type="survey",
		carob_contributor="Shumirai Manzvera",
		carob_effort = NA,
		carob_date="2023-08-24"
	)


	f <- ff[basename(ff) == "TAMASA_ET_CC_2015_BakoF.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Raw_Data")	

## use a subset
	d <- data.frame(
		location = r$`Name of the Village`,
		yield = r$`Average yield kg/ha or (Q1+Q2)/2`



#### about the data #####
## (TRUE/FALSE)
	
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$is_experiment <- FALSE
	d$irrigated <- FALSE


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Ethiopia"
	d$adm1 <- "Oromia"
	d$adm2 <- "West Showa"
	d$adm3 <- "Bako"
	d$elevation <- NA

## each site must have corresponding longitude and latitude
## see carobiner::geocode

##RH what is that supposed to mean? 
## without lon/lat the data are not useful
	d$longitude <- 35.52-42.12
	d$latitude <-  6.58-11.77

   stop("fix lon/lat")

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
#variety
	d$variety <- r$`Type of Variety`
 #farming type 
#	d$intercrops <- d$`Cropping System`
#	d$intercrops <- d$`Intercropping with legume`
#	d$croprotation <- d$`Crop Rotation with`
#	d$crop_rotation <- d$croprotation

	d$previous_crop <- r$`Previous/precursor crop`

##### Fertilizers #####

## normalize names 
   d$fertlizer_type <- r$`Type of Inorganic Fertilizer`
   d$inoculated <- FALSE
   d$OM_used <- r$`Apply Organic Fertilizer ?`
   d$OM_type <- r$`Type of Organic Fertilizer applied`
   d$soil_type <- r$`Soil type`
   d$soil_pH <- r$pH
   d$soil_K <- r$`K (mg kg-1)`
   d$soil_Mg <- r$`Mg (mg kg-1)`
   d$soil_Ca <- r$`Ca (mg kg-1)`
   
   stop("there are many more soil variables")
  
##### Yield #####

	
	#TAMASA_ET_CC_2015_CSAF
	r2 <- TAMASA_ET_CC_2015_CSAF
	d2 <- r2
	#### about the data #####
	## (TRUE/FALSE)
	
	
	d2$on_farm <- FALSE
	d2$is_survey <- TRUE
	d2$is_experiment <- FALSE
	d2$irrigated <- FALSE
	
	
	
	##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	
	d2$adm1 <- "Oromia,Amhara"
	d2$adm2 <- "Jimma, Wollega, West Showa, East Showa, Gojjam"

	d2$Latitude <- 7.10-11.59 
	d2$Longitude <- 35.32-40.59
	#crop 
	d2$crop <- "maize"
	#variety
	d2$variety <- d2$`Name of variety`
	d2$inoculated <- FALSE
	#fertiliser
	d2$OM_used <- d2$`Fertilizer type/organic`
	#soil characteristics
	d2$soil_pH <- d2$pH
	d2$soil_K <- d2$`K  (mg/kg)`
	d2$soil_Mg <- d2$`Mg  (mg/kg)`
	d2$soil_Ca <- d2$`Ca  (mg/kg)`
	d2$soil_P_total <- d2$`P  (mg/kg)`
	
	#yield
	d2$yield <- d2$`Moisture adjusted grain  yield (kg /ha)`
	
	#TAMASA_ET_CC_2015F
	r3 <- TAMASA_ET_CC_2015F
	d3 <- r3
	d3$on_farm <- FALSE
	d3$is_survey <- TRUE
	d3$is_experiment <- FALSE
	d3$irrigated <- FALSE
	#location of the village
	d3$adm1 <- "Oromia, Amhara and Southern regional state of Ethiopia" 
	d3$adm2 <- "Jimma, Ili Ababura, Wollega, East Showa, Gojjam, Woliyta"

	#each site must have corresponding longitude and latitude
	## see carobiner::geocode
	d3$longitude <- 35.52-42.12
	
	d3$latitude <-  6.58-11.77
	##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	d3$crop <- "maize"
	#variety
	d3$variety <- d3$`Name of variety`
	##Fertiliser used
	d3$OM_used <- d3$`Fertilizer type/inorganic`
	d3$soil_pH <- d3$pH
	d3$soil_K <- d3$`K  (mg/kg)`
	d3$soil_Mg <- d3$`Mg  (mg/kg)`
	d3$soil_Ca <- d3$`Ca  (mg/kg)`
	###YIELD
	d3$yield <- d3$`Quadrant(1)-Grain yield kg/ha`+d3$`Quadrant(2)-Grain yield kg/ha`+d3$`Quadrant(3)-Grain yield kg/ha`
	
	r4 <- TAMASA_ET_CC_2016F
	d4 <- r4
	d4$crop <- "maize"
	d4$on_farm <- FALSE
	d4$is_survey <- TRUE
	d4$is_experiment <- FALSE
	d4$irrigated <- FALSE
	
	d4$adm1 <- d4$Zone
	d4$adm2 <- d4$Districts
	d4$adm3 <- d4$Kebele
	d4$adm4 <- d4$Community
	#plant details
	d4$row_spacing <- d4$`Row Distance (cm)`
	d4$plant_spacing <- d4$`Plant Distance (cm)`
	#location
	
	d4$longitude <- 36.02-37.53
	d4$latitude <-  7.40-10.41
	#yield
	d4$yield <- d4$`Grain yield (kg/ha)`
	
	carobiner::write_files(meta, d, path=path)
}


