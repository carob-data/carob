# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Health crisis and quarantine measures in Peru: Effects on livelihoods of potato farmers: Potato producers survey

International Potato Center (CIP) and partners collaborated to develop strategies to improve welfare and nutrition of Andean potato producers in La Libertad, Juinin and Huancavelica. COVID-19 pandemic affected those interventions and there was a need to understand the project implication of COVID-19 measurements in production, consumption and commercialization of potatoes in those settings. A representative household survey  to Andean potato producers was conducted from June to July 2020. The sample size includes 214 households located in Andean communities from 8 provinces and 3 Departments.  The survey covered the four major domains: food security, labor availability, farm system resilience, and agricultural systems connectivity.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/KLUBXA"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Potato producing households; universe: Potato households in CIP intervention areas in Peru; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: Sample selection differed for each crop. For potatoes, we identified 39 villages in three regions where the International Potato Center (CIP) and local partners were conducting interventions before the pandemic. Between 5 and 10 families were randomly selected for interviews from a list of beneficiaries in each community.; collectionMode: The survey was primarily conducted by telephone to reduce risk of infection to household members and enumerators. Therefore, the survey protocol includes an initial identification of families who own a cellphone. In La Libertad, 100% of the interviews were conducted by phone, while in Junín and Huancavelica, 50% of the interviews were conducted by phone, and remainder were conducted face-to-face.; researchInstrument: Structured household survey questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "Diccionario Encuesta Productores.xlsx"]
	f2 <- ff[basename(ff) == "Encuesta Productores - La Libertad - Junin - Huancavelica - COVID-19.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Nro", "Variable name", "Abbreviation", "Description", "Unit"


	d2 <- data.frame(
		P_fertilizer = r2[["P_01"]]
	)
##r2: "ID", "P_02", "P_03", "P_04", "P_05", "P_06a", "P_06b", "P_06c", "P_06d", "P_07", "P_08", "P_09", "P_10", "P_10a", "P_11a", "P_11b", "P_11c", "P_11d", "P_11e", "P_11f", "P_11g", "P_11h", "P_11i", "P_12a", "P_12b", "P_12c", "P_13", "P_14", "P_15", "P_15a", "P_16", "P_17", "P_18", "P_19", "P_19a", "P_20", "P_21", "P_22", "P_23", "P_23a", "P_24", "P_25", "P_25a", "P_25b", "P_25c", "P_26", "P_27", "P_27a", "P_28", "P_29", "P_30a", "P_30b", "P_30c", "P_31a", "P_31b", "P_31c", "P_32", "P33a", "P33b", "P_34a", "P_34b", "P_35a", "P_35b", "P_36a", "P_36b", "P_36c", "P_36d", "P_36e", "P_37", "P_38a", "P_38b", "P_39a...73", "P_39a...74", "P_39a...75", "P_40", "P_41", "P_42a", "P_42b", "P_42c", "P_42d", "P_43", "P_44", "P_45", "P_46a", "P_46b", "P_47a", "P_47b", "P_47c", "P_47d", "P_48", "P_48a", "P_49", "P_50a", "P_50b", "P_51", "P_52a", "P_52b", "P_52c", "P_53", "P_54a", "P_54b", "P_54c", "P_55", "P_56a", "P_56b", "P_56c", "P_56d", "P_56e", "P_57a", "P_57b", "P_57c", "P_58", "P_59a", "P_59b", "P_59c", "P_59d", "P_59f", "P_60a", "P_60b", "P_60c", "P_60d", "P_61a", "P_61b", "P_61c", "P_61d", "P_61e", "P_61f", "P_61g", "P_62a", "P_62b", "P_62c", "P_62d", "P_62e", "P_62f", "P_63a", "P_63b", "P_63c", "P_64", "P_65", "P_66", "P_67", "P_68", "P_69", "P_70", "P_71a", "P_71b", "P_72", "...149"


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

