# R script for "carob"

## ISSUES
# ....
# Treatments involved various combinations of hand cultivation (HC), ridging with animal traction
# and  planting on ridges (AT), limited P fertilizer application and rotation with sole cowpea (C)
# with two cropping systems: millet-cowpea intercrop (M/C) and sole millet
#
# The data have numeric codes for "Treatment", "Crop rotation" and "N Application" but there is no key
#
# "The experiment was established in 1986", but the data only cover 1995-2017
# Are the other years available

carob_script <- function(path) {

"
The experiment was established in 1986 and continued until 2018 at the ICRISAT Sahelian Center (ISC), located at Sadore, 45 km south of Niamey, Niger, West Africa (lat 13d l5' N long 2d 18'E) and at an altitude of 240 m asl. The climate at Sadore is characterized by a short rainy season from June to September (about 90 d). The average rainfall is 560 mm, is irregular and normally comes in the form of thunderstorms. During the crop growing season, maximum temperatures varied in the range of 30-40oC. Potential evapotranspiration (PET) exceeds the total rainfall in all months except July-August which are the peak months of the rainy season (Sivakumar, l986). The site is located on a sandy plain with Aeolian sands 2-8 m in depth covering one of a series of stepped surfaces comprised of cemented laterite gravels (West et al., l984). The surface horizon (25-30 cm in depth) is yellowish red sand underlain by a thick (> 1m) red loam or red sand horizon. Soils are coarse textured, with sand content exceeding 95%. Organic matter content is about 0.4%. The soils are acidic in nature (pHH2O 4.5-5.0), and low in nutrients (cation exchange capacity: 1.5 cmol kg-1) and water holding capacity (< 10%) (West et a1., 1984). The experiment was designed as a randomized complete bock (RCBD) with l3 treatments replicated four times, involving various combinations of hand cultivation (HC), ridging with animal traction and planting on ridges (AT), limited P fertilizer application and rotation with sole cowpea (C). These were tested with two cropping systems: millet-cowpea intercrop (M/C) and sole millet (M). Plot size was 500 m2 150 m x l0 m), and the area sampled for yield of each experimental plot was 75 m2 125 m x 3 m). In 1989, an additional treatment of crop residues (millet straw) was introduced by dividing each treatment plot into half. Thus, the plot size was reduced to half (250 m2, 25 m x l0 m), and similarly the sampled area became half (37.5 m2, 12.5 m x 3 m). In 1994, nitrogen treatment (15 kg N ha-1 as calcium ammonium nitrate) was introduced by further dividing each experimental plot into two, thereby creating additional treatments with nitrogen and without nitrogen.
"

	uri <- "doi:10.21421/D2/AVKD0T"
	group <- "fertilizer"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
	   publication="doi:10.21421/D2/AVKD0T",
	   carob_contributor="Eduardo Garcia Bendito",
	   carob_effort = NA,
	   carob_date="2021-06-29",
	   data_type="fertilizer"
	    
	)




	f <- ff[basename(ff) == "Data file of Long term millet trial ISC sadore Niger.xlsx"]

	d <- as.data.frame(readxl::read_excel(f))
                                         
## RH: this needs to be clarified 
#table(d$Treatment)
#  1   2   3   4   5   6   7   8   9  10 
#352 351 351 352 351 352 352 352 352 352 

# table(d$`Crop rotation`)
#   1    2 
#1757 1760 
 

	
	d$country <- "Niger"
	d$adm1 <- "Tillabéry"
	d$adm2 <- "Say"
	d$adm3 <- "Say"
	d$location <- "Sadoré"
	d$site <- "ICRISAT Sahelian Center (ISC)"
	d$trial_id <- paste0(d$site, '_', d$Years)
	d$latitude <- 13.234295
	d$longitude <- 2.283155
	d$planting_date <- as.character(d$Years)
	d$harvest_date <- as.character(d$Years)
	d$season <- "rainy"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	
### RH: this needs to be fixed. Need to know what is what.
### 
##	d$treatment <- "Treatments replicated four times, involving various combinations of hand cultivation (HC), ridging with animal traction and planting on ridges (AT), limited P fertilizer application and rotation with sole cowpea (C)"
	d$treatment <- d$Treatment
	d$rep <- d$`Replication number`
	d$crop <- "pearl millet"
	

###RH d$yield <- d$`Stover yield` + d$`Grain weight` + d$`Pod weight`
	
	
#Since 'crop' is pearl millet yield is grain yield
	d$yield <- d$`Grain weight` 
# need to think about "stover yield".
# sometimes you get stems and leaves, but here it is together	
	d$fwy_residue <- d$`Stover yield`

## pod weight is for cowpea
## what to do? Add another variable "yield2"? Or replicate the treatments that have cowpea, and now 
## refer to it is cowpea? 
#   ? <- d$`Pod weight`
	
#The description file says	
#Panicle weight – Units: Kg/ha
#Grain weight – Units: Kg/ha
#Stover yield – Units: Kg/ha

	d$fertilizer_type <- "CAN"
	
## RH: how do you know that 1 is 15kg/ha?? 
## I would have guessed that 1 = 0, and 2 = 15kg/ha	#
	d$N_fertilizer <- ifelse(d$`N Application` == 1, 15, 0)
## 	$`N Application` <- NULL

	d$OM_used <- FALSE
	d$soil_type <- "Aeolian sands"
	d$soil_pH <- 4.75
	d$soil_sand <- 95
	d$soil_SOC <- 0.4
	
## RH I generally do not like subsetting by column number. It is a bit risky. 
## I prefer to do as I suggest on line 92 (set unused variables to NULL)
## and/or select by names  
	
	d <- d[,c(9:35)]

	carobiner::write_files(path, meta, d)

}
