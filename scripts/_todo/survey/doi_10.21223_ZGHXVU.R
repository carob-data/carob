# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Farmers' perception on late blight in Peruvian highlands - Ulcumayo (Junin, Peru)

A questionnaire with 25 questions, many with multiple answers, was used in the village of Ulcumayo (Junin) in the Central Peruvian highlands to learn more about the perceptions of farmers regarding potato production with particular emphasis on management practices for the disease late blight (LB), caused by the oomycete, Phytophthora infestans.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/ZGHXVU"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: farmers; universe: Ulcumayo (Junin, Peru); timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: 146 women and 396 men, also of different ages.; researchInstrument: Structured  survey questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "01_Data_survey_Ulcumayo.xlsx"]
	f2 <- ff[basename(ff) == "02_DataDictionary_Ulcumayo (1).xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		hhid = r1[["Farmer name"]],
		longitude = r1[["Longitude"]],
		latitude = r1[["Latitude"]],
		elevation = r1[["Altitude"]],
		yield = r1[["Potyield"]]
	)
##r1: "Data", "Age", "Education level", "Gender", "Place", "LB", "APW", "BS", "EB", "F", "PTM", "PFB", "TR", "PLM", "CW", "Othr1", "Sun", "Rai", "Mi", "SRM", "Insc", "Fun", "Othr2", "PD", "Eme", "30d", "Rainy", "Rain/sun", "Nspot", "NeighLB", "Others3", "AgSell", "Agron", "Neigh", "Ownexp", "Course", "RTV", "Others4", "Cv1", "Cv2", "Cv3", "Cv4", "Cv5", "Cv6", "CV7", "TotCv", "S1", "S2", "S3", "S4", "S5", "R1", "R2", "R3", "R4", "R5", "FHS1", "FHS2", "FHS3", "FHS4", "FHS5", "TotF", "FfreqHS", "DF1", "DF2", "DF3", "DF4", "DF5", "NFS", "FES1", "FES2", "FES3", "FES4", "TFRF", "FfreqES", "DFES1", "DFES2", "DFES3", "DFES4", "NFEachS", "Lbyield", "Difyield"


	d2 <- data.frame()
##r2: "Variable_name", "Abbreviation", "Method", "Formula", "Unit", "Reference", "Categories"


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

