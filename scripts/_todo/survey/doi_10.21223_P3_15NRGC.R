# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: RTC-Action Malawi Project Baseline study for Potato Component

The baseline study was conducted to assess and understand the socio-economic situation of farmers including potato production methods and technologies, seed system, input and output market systems, farmer capacity levels, perception on potato nutrition issues and food security situation. The results show that potato production is mainly dominated by female headed households in the new districts (Dowa and Mzimba) mainly because the crop is relatively newer but slowly getting popularity due to the declining tobacco performance that has been the number one cash crop in these districts. The average age of the sampled household heads was 34 in all the districts and this result indicates that the potato production is mainly dominated by people who are still in the productive age. This observation is consistent with the fact that potato production is relatively labour intensive hence it requires participation of active adults and the youth. Further to this, the results show that on average the households have a total land size of 1.5 acres (0.6 ha) which is consistent with the results of the recent IHS 4 that also reported that on average in Malawi, the average cultivated area is about 1.5 acres and further results also show that on average, male-headed households cultivated 1.7 acres compared to their female counterparts who cultivated 1.2 acres.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/15NRGC"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Household; universe: Under 5 Children, Children above 5, Adult Men and Women; timeMethod: Cross sectional; samplingProcedure: The study used primary data that was collected from 360 potato growing households. Sampled households from Ntcheu and Dedza were amongst the households that participated during the endline of the potato phase II project and they were selected using simple random sampling.  Purposive and random sampling techniques were then used to sample the remaining households in Dowa and Mzimba by firstly listing all potato producing households from main potato growing villages within the potato hotspot EPAs then randomly sample 2 villages from each EPA where a total of 15 households were randomly selected.; researchInstrument: Semi-structured questionnaire",
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "Part 4 Input Use in Potato Production_2.dta"]

	r1 <- haven::read_dta(f1)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		season = r1[["season"]]
	)
##r1: "qnno", "part4b1", "part4b1_oth", "part4b2", "part4b3", "part4b3_oth", "part4b4", "part4b5", "part4b5_oth", "part4b6", "part4b7", "part4b8", "part4b8_oth", "part4b9", "part4b10", "part4b10_oth", "part4b11", "part4b12", "part4b12_oth", "part4b13", "part4b14", "part4b14_oth", "part4b15", "part4b16", "part4b16_oth", "part4b17", "part4b18", "part4b18_oth", "part4b19", "part4b20", "part4b20_oth", "part4b21", "part4b22", "part4b23", "part4b23_oth", "part4b24", "part4b25", "part4b25_oth", "part4b26", "part4b27", "part4b27_oth", "part4b28", "part4b29", "part4b29_oth", "part4b30", "part4b31", "part4b32", "part4b33", "part4b33_oth", "part4b34"


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
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

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

