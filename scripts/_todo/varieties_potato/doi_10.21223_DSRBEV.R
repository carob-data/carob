# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataverse for: Evaluation of CIP advanced clones for late blight and virus resistance and early bulking

This dataset contains the evaluation of the International Potato Center (CIP) advanced clones for late blight and virus resistance and early bulking
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/DSRBEV"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=1,
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
	

	f1 <- ff[basename(ff) == "13394_Data.xlsx"]
	f2 <- ff[basename(ff) == "13394_DataDictionary_ElementDescription.xlsx"]
	f3 <- ff[basename(ff) == "13394_DataDictionary_UniqueIdentifier.xlsx"]
	f4 <- ff[basename(ff) == "13394_Material_List.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="virus resistance")
	r1b <- carobiner::read.excel(f1, sheet="Late blight resistance")
	r1c <- carobiner::read.excel(f1, sheet="MTW at 80-100-120 d")
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame()
##r1a: "NUM", "ACCENUMB", "CODE", "NPE", "Number of PVX infected plants", "PVX (Absorbance average value)", "Percentage of PVX infection", "PVX", "Number of PVY infected plants", "PVY (Absorbance average value)", "Percentage of PVY infection", "PVY", "Number of PLRV infected plants", "PLRV (Absorbance average value)", "Percentage of PLRV infection", "PLRV", "Number of PVS infected plants", "PVS (Absorbance average value)", "Percentage of PVS infection"


	d1b <- data.frame(
		latitude = r1b[["Late blight resistance"]]
	)
##r1b: "ACCENUMB", "POPULATION", "ACCENAME", "rAUDPC_Yunnan2015", "rAUDPC_Yunnan2016"


	d1c <- data.frame()
##r1c: "ACCENUMB", "MTWP 80 days after planting", "MTWP 100 days after planting", "MTWP 120 days after planting", "SLOPE", "Early bulking 80 days after planting"


	d2 <- data.frame(
		crop = tolower(r2[["Crop"]])
	)
##r2: "Variable ID", "Variable name", "Variable synonyms", "Other name", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9", "Category 10"


	d3 <- data.frame()
##r3: "Spreadsheet_Tab", "Element_DisplayName", "Unique_Identifier", "Source"


	d4 <- data.frame()
##r4: "Numeration", "Accession_Number", "Accession_Name", "Accession_Code", "Scale_audpc", "Family_AcceNumb", "Female_AcceNumb", "Male_AcceNumb", "Population", "Cycle", "Is_control", "Seed_source", "Simultaneous_trials", "Previous_trials", "list_name", "Date_Created"


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

