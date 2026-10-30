# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: 100 promising diploid clones from biofortification cycle III selected and assessed in Rwanda

Summary of mineral content for Iron, Zinc and yield parameters over 3 sites in Rwanda, 2015.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/MBBV5A"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
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
	

	f1 <- ff[basename(ff) == "4030_Data.xlsx"]
	f2 <- ff[basename(ff) == "4030_DataDictionary_ElementDescription.xlsx"]
	f3 <- ff[basename(ff) == "4030_DataDictionary_UniqueIdentifier.xlsx"]
	f4 <- ff[basename(ff) == "4030_Kinigi_2016_.xlsx"]
	f5 <- ff[basename(ff) == "4030_Sigira_2016.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Summary table 2015")
	r1b <- carobiner::read.excel(f1, sheet="Summary 3 sites 2015")
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4a <- carobiner::read.excel(f4, sheet="Minimal")
	r4b <- carobiner::read.excel(f4, sheet="Material_list")
	r4c <- carobiner::read.excel(f4, sheet="Kinigi Datasheet 2016")
	r4d <- carobiner::read.excel(f4, sheet="Kinigi MAP 2016")
	r5a <- carobiner::read.excel(f5, sheet="Minimal")
	r5b <- carobiner::read.excel(f5, sheet="Material_list")
	r5c <- carobiner::read.excel(f5, sheet="Sigira Datasheet 2016")
	r5d <- carobiner::read.excel(f5, sheet="Sigira MAP 2016")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		location = r1a[["Summary mineral content Fe Zn and yield parameters over 3 sites 2015"]],
		yield = r1a[["Summary mineral content Fe Zn and yield parameters over 3 sites 2015"]]
	)
##r1a: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25"


	d1b <- data.frame(
		location = r1b[["Mean over 3 sites"]]
	)
##r1b: "...1", "...2", "...3", "...4", "Kinigi 2015", "...6", "...7", "...8", "...9", "Rwerere 2015", "...11", "...12", "...13", "...14", "Sigira 2015", "...16", "...17", "...19", "...20", "...21", "...22", "...23", "...24"


	d2 <- data.frame(
		crop = tolower(r2[["Crop"]])
	)
##r2: "Variable ID", "Variable name", "Variable synonyms", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9", "Category 10"


	d3 <- data.frame()
##r3: "Spreadsheet_Tab", "Element_DisplayName", "Unique_Identifier", "Source"


	d4a <- data.frame()
##r4a: "Factor", "Value"


	d4b <- data.frame()
##r4b: "Numeration", "Accession_Number", "Accession_Name", "Accession_code", "Female_AcceNumb", "Female_codename", "Male_AcceNumb", "Male_codename", "Population"


	d4c <- data.frame(
		plot_id = r4c[["PLOT"]],
		rep = r4c[["REP"]]
	)
##r4c: "BLOCK", "INSTN", "NTP"


	d4d <- data.frame()
##r4d: "...1", "...2", "...3", "Road", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14"


	d5a <- data.frame()
##r5a: "Factor", "Value"


	d5b <- data.frame()
##r5b: "Numeration", "Accession_Number", "Accession_Name", "Accession_code", "Female_AcceNumb", "Female_codename", "Male_AcceNumb", "Male_codename", "Population"


	d5c <- data.frame(
		plot_id = r5c[["PLOT"]],
		rep = r5c[["REP"]]
	)
##r5c: "BLOCK", "INSTN", "NTP"


	d5d <- data.frame()
##r5d: "...1", "...2", "Car park", "...4", "cow shed", "...6", "...7", "...8"


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

