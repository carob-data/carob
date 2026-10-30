# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Assessment of fertilizer availability in potato producing areas in Rwanda

An assessment of fertilizer availability in potato producing areas in Rwanda was conducted. The five districts with the highest area under potato cultivation in Rwanda were selected: Nyabihu, Rubavu, Musanze, Burera and Gicumbi, representing > 70% of the annual potato production in Rwanda. Twelve sectors were randomly selected across these districts. Per sector, three agrodealer shops (or less if there are less than 3) were visited to collect data on available fertilizers. Data were collected by an enumerator and entered in an ODK form.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/NNN80U"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Agrodealer shops; universe: Rubavu, Nyabihu, Musanze, Gicumbi, Burera districts; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: The five districts with the highest area under potato cultivation in Rwanda were selected: Nyabihu, Rubavu, Musanze, Burera, and Gicumbi. Twelve sectors were randomly selected across these districts. Per sector, three agrodealer shops were evaluated.; collectionMode: A selection of agrodealer shops was based on a simple random sampling process.; researchInstrument: Strutured questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "01_Assessment_fertilizers_details.xlsx"]
	f2 <- ff[basename(ff) == "02_Assessment_availability_fertilizers.xlsx"]
	f3 <- ff[basename(ff) == "03_DataDictionary_Template_AssessmentFertilizers.xlsx"]
	f4 <- ff[basename(ff) == "04_Assessment_fertilizers_ODK survey tool.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3a <- carobiner::read.excel(f3, sheet="Availability of fertilizers")
	r3b <- carobiner::read.excel(f3, sheet="Fertilizer details")
	r4a <- carobiner::read.excel(f4, sheet="survey")
	r4b <- carobiner::read.excel(f4, sheet="choices")
	r4c <- carobiner::read.excel(f4, sheet="settings")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		country = r1[["Country_of_origin"]]
	)
##r1: "Agrodealer_index", "Fertilizer", "Commercial_name", "Composition", "Manufacturer", "Dealer", "Quantity_per_package", "Quantity_unit", "Original_price_per_kg", "Subsidized_price_per_kg"


	d2 <- data.frame(
		adm2 = carobiner::fix_name(r2[["District"]], "title")
	)
##r2: "Agrodealer_index", "Sector", "Date", "Availability of fertilizer1", "Availability of fertilizer2", "Availability of fertilizer3", "Availability of fertilizer4", "Availability of fertilizer5", "Availability of fertilizer6", "Availability of fertilizer7", "Availability of fertilizer8", "Availability of fertilizer9", "Availability of fertilizer10", "Availability of fertilizer11", "Availability of fertilizer12", "Availability of fertilizer13", "Availability of fertilizer14", "Availability of fertilizer15", "Availability of fertilizer16", "Availability of fertilizer17", "Availability of fertilizer18", "Availability of fertilizer19", "Availability of fertilizer20", "Availability of fertilizer21", "Availability of fertilizer22", "Availability of fertilizer23", "Availability of fertilizer24"


	d3a <- data.frame()
##r3a: "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference"


	d3b <- data.frame()
##r3b: "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference"


	d4a <- data.frame(
		rep = r4a[["repeat_count"]]
	)
##r4a: "type", "name", "label::English", "choice_filter", "hint", "constraint", "constraint_message", "calculation", "required", "appearance", "default", "relevant", "read_only", "image", "audio", "media::video"


	d4b <- data.frame()
##r4b: "list_name", "name", "label::English", "Q001"


	d4c <- data.frame()
##r4c: "form_title", "form_id", "default_language", "style"


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

