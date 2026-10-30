# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication Data for: Estimating carbon footprint in potato trials under drip and furrow irrigation

This dataset has been specially created to estimate carbon footprint in potato field trials using Cool Farm Tool (CFT) Online. Includes three field trials were performed at CIP and National Agrarian University La Molina (UNALM) experimental stations in Lima during July 5th to October 10th, 2017 (E1-CIP), October 5th, 2017 to January 15th, 2018 (E2-CIP), and June 13th to September 13th, 2018 (E3-UNALM).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/LNLR9B"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = NA,
		
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
	

	f1 <- ff[basename(ff) == "CFT_PNIA 2017-2018.xlsx"]
	f2 <- ff[basename(ff) == "Data_dictionary.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Crop-details")
	r1b <- carobiner::read.excel(f1, sheet="Soil-characteristics")
	r1c <- carobiner::read.excel(f1, sheet="Fertiliser inputs")
	r1d <- carobiner::read.excel(f1, sheet="Crop protection inputs")
	r1e <- carobiner::read.excel(f1, sheet="Energy")
	r1f <- carobiner::read.excel(f1, sheet="Water")
	r1g <- carobiner::read.excel(f1, sheet="Transport")
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		crop = tolower(r1a[["Crop"]]),
		variety = r1a[["Variety"]],
		year = r1a[["Harvest year"]],
		planting_date = r1a[["Planting / sowing date"]],
		harvest_date = r1a[["Harvest date"]]
	)
##r1a: "Experiment  name", "Assessment name", "Crop area", "Harvested amount", "Amount rejected at harvest", "Seed amount", "Farm-gate ready amount", "Amount of unharvested crops", "Residue amount", "Residue management"


	d1b <- data.frame(
		soil_texture = tolower(r1b[["Soil texture"]])
	)
##r1b: "Experiment  name", "Assessment name", "Soil type", "Soil organic matter", "Soil moisture average", "Soil moisture at sowing", "Soil drainage", "Soil pH", "Your field name"


	d1c <- data.frame()
##r1c: "Assessment name", "Fertiliser application", "Name", "Fertiliser type", "Manufactured in", "Application rate", "Fertiliser weight, or units?", "Application method", "Emissions inhibitors"


	d1d <- data.frame()
##r1d: "Assessment name", "Applications", "Category", "Applications (doses)", "Applications rate", "Active ingredient"


	d1e <- data.frame()
##r1e: "Assessment name", "Type", "Energy source", "Energy used 1", "Category", "Machine", "Energy used 2", "Fuel use", "Number of operations", "Custom", "Do you store this product?", "Percentage stored", "Store loading", "Unloading", "Storage", "Time in storage", "Temperature", "CIPC applications", "CIPC dose"


	d1f <- data.frame()
##r1f: "Experiment  name", "Assessment name", "Method", "Water source", "Pumping depth", "Horizontal distance", "Power source", "TW_Week 1", "TW_Week 2", "TW_Week 3", "TW_Week 4", "TW_Week 5", "TW_Week 6", "TW_Week 7", "TW_Week 8", "TW_Week 9", "TW_Week 10", "TW_Week 11", "TW_Week 12", "TW_Week 13", "TW_Week 14"


	d1g <- data.frame()
##r1g: "Experiment  name", "Assessment name", "Mode", "Weight", "Distance"


	d2 <- data.frame(
		crop = tolower(r2[["Crop"]])
	)
##r2: "Group", "Subgroup", "Variable", "TraitName", "Description", "TraitUnit", "TraitDataType", "TraitValidation", "category"


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

