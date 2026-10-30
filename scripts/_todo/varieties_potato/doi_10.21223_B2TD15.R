# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Late blight resistant varieties adapted to humid highland sub-tropical agro-ecologies in SSA

Yield data of nine selected clones in Rwanda 2014B-2016A
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/B2TD15"
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
	

	f1 <- ff[basename(ff) == "4034_Bugeshi.xlsx"]
	f2 <- ff[basename(ff) == "4034_Burera.xlsx"]
	f3 <- ff[basename(ff) == "4034_DataDictionary_ElementDescription.xlsx"]
	f4 <- ff[basename(ff) == "4034_DataDictionary_UniqueIdentifier.xlsx"]
	f5 <- ff[basename(ff) == "4034_Gihonga.xlsx"]
	f6 <- ff[basename(ff) == "4034_Muhoza.xlsx"]
	f7 <- ff[basename(ff) == "4034_Nyabihu.xlsx"]
	f8 <- ff[basename(ff) == "4034_Summary_minerals2013_2014.xlsx"]
	f9 <- ff[basename(ff) == "4034_Summary_yield2014.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Minimal")
	r1b <- carobiner::read.excel(f1, sheet="Material_list")
	r1c <- carobiner::read.excel(f1, sheet="Bugeshi Fieldbook")
	r1d <- carobiner::read.excel(f1, sheet="summary_yield")
	r1e <- carobiner::read.excel(f1, sheet="Bugeshi Map")
	r2a <- carobiner::read.excel(f2, sheet="Burera Summary")
	r2b <- carobiner::read.excel(f2, sheet="Burera Fieldbook")
	r2c <- carobiner::read.excel(f2, sheet="Burera Pivot table")
	r2d <- carobiner::read.excel(f2, sheet="Burera Map")
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5a <- carobiner::read.excel(f5, sheet="Gihonga Summary")
	r5b <- carobiner::read.excel(f5, sheet="Gihonga Fieldbook")
	r5c <- carobiner::read.excel(f5, sheet="Gihonga Pivot table")
	r5d <- carobiner::read.excel(f5, sheet="Gihonga Map")
	r6a <- carobiner::read.excel(f6, sheet="Minimal")
	r6b <- carobiner::read.excel(f6, sheet="Muhoza Fieldbook")
	r6c <- carobiner::read.excel(f6, sheet="Muhoza Pivot table")
	r6d <- carobiner::read.excel(f6, sheet="Muhoza Map")
	r7a <- carobiner::read.excel(f7, sheet="Nyabihu Summary")
	r7b <- carobiner::read.excel(f7, sheet="Nyabihu Fieldbook")
	r7c <- carobiner::read.excel(f7, sheet="Nyabihu Pivot table")
	r7d <- carobiner::read.excel(f7, sheet="Nyabihu Map")
	r8 <- carobiner::read.excel(f8)
	r9 <- carobiner::read.excel(f9)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame()
##r1a: "Factor", "Value"


	d1b <- data.frame()
##r1b: "Entries"


	d1c <- data.frame(
		plot_id = r1c[["PLOT"]],
		rep = r1c[["REP"]]
	)
##r1c: "BLOCK", "INSTN", "NPH", "NNoMTP", "NoMTWP", "NMTCII", "MTWCII", "NMTCI", "MTWCI", "TNTP", "TTWP", "TTWPL"


	d1d <- data.frame(
		variety = r1d[["CloneID"]],
		yield = r1d[["Yield"]]
	)


	d1e <- data.frame()
##r1e: "PD: 18.3.16", "...2", "...3", "...4", "...5", "...6", "...7"


	d2a <- data.frame()
##r2a: "Factor", "Value"


	d2b <- data.frame(
		plot_id = r2b[["PLOT"]],
		rep = r2b[["REP"]]
	)
##r2b: "BLOCK", "INSTN", "NTP", "NPH", "NNoMTP", "NoMTWP", "NMTCII", "MTWCII", "NMTCI", "MTWCI", "TNTP", "TTWP", "TTWPL"


	d2c <- data.frame(
		yield = r2c[["Average of yield"]],
		variety = r2c[["Clone ID"]]
	)
##r2c: "...2", "...3", "Yield"


	d2d <- data.frame(
		plot_id = r2d[["Plot 10"]]
	)
##r2d: "...1", "...3", "...4", "Plot 11", "...6", "...7", "...8", "...9", "...10"


	d3 <- data.frame(
		crop = tolower(r3[["Crop"]])
	)
##r3: "Variable ID", "Variable name", "Variable synonyms", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9", "Category 10"


	d4 <- data.frame()
##r4: "Spreadsheet_Tab", "Element_DisplayName", "Unique_Identifier", "Source"


	d5a <- data.frame()
##r5a: "Factor", "Value"


	d5b <- data.frame(
		plot_id = r5b[["PLOT"]],
		rep = r5b[["REP"]]
	)
##r5b: "BLOCK", "INSTN", "NPH", "NNoMTP", "NoMTWP", "NMTCII", "MTWCII", "NMTCI", "MTWCI", "TNTP", "TTWP", "TTWPL"


	d5c <- data.frame(
		yield = r5c[["Average of yield"]],
		variety = r5c[["Clone"]]
	)
##r5c: "...2", "...3", "Yield"


	d5d <- data.frame(
		plot_id = r5d[["Plot 7"]]
	)
##r5d: "...1", "...2", "Plot 8", "Plot 22", "Plot 23", "...7", "...8", "...9"


	d6a <- data.frame()
##r6a: "Factor", "Value"


	d6b <- data.frame(
		plot_id = r6b[["PLOT"]],
		rep = r6b[["REP"]]
	)
##r6b: "BLOCK", "INSTN", "NTP", "NPH", "NNoMTP", "NoMTWP", "NMTCII", "MTWCII", "NMTCI", "MTWCI", "TNTP", "TTWP", "TTWPL", "Plant_Vigor", "Virus_Scoring", "OBS"


	d6c <- data.frame(
		yield = r6c[["Average of yield"]],
		variety = r6c[["Clone"]]
	)
##r6c: "...2", "...3", "Yield"


	d6d <- data.frame()
##r6d: "House and TARO plantation", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d7a <- data.frame()
##r7a: "Factor", "Value"


	d7b <- data.frame(
		plot_id = r7b[["PLOT"]],
		rep = r7b[["REP"]]
	)
##r7b: "BLOCK", "INSTN", "NTP", "NPH", "NNoMTP", "NoMTWP", "NMTCII", "MTWCII", "NMTCI", "MTWCI", "TNTP", "TTWP", "TTWPL"


	d7c <- data.frame(
		yield = r7c[["Average of yield"]],
		variety = r7c[["Clone ID"]]
	)
##r7c: "...2", "...3", "Yield"


	d7d <- data.frame()
##r7d: "...1", "FARM", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12"


	d8 <- data.frame()
##r8: "Accession_number", "Dry matter_Mean_Tamira", "Unit Errors...3", "FeDW_Means_Tamira", "Unit Errors...5", "ZnDW_Means_Tamira", "Unit Errors...7", "Dry matter_ Mean_kinigi", "Unit Errors...9", "FeDW_Mean_Kinigi", "Unit Errors...11", "ZnDW_mean_kinigi", "Unit Errors...13", "Dry matter _Mean_Sigara", "Unit Errors...15", "FeDW_Mean_Sigira", "Unit Errors...17", "ZnDW_Mean_Sigira", "Unit Errors...19", "DM_mean", "Rank...21", "FeDW_mean", "Rank...23", "ZnDW_mean", "Rank...25"


	d9 <- data.frame(
		location = r9[["Average over 2 sites in 2014-15A season"]],
		season = r9[["Average over 2 sites in 2014-15A season"]]
	)
##r9: "Accession_number", "Kinigi 2014B-15A", "Rwerere 2014B-15A", "Musanze 2015B", "Bugeshi 2016A", "Burera 2016A", "Gihonga 2016A", "Muhoza 2016A", "Nyabihu 2106A", "Average over 5 sites 2016 A season"


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

