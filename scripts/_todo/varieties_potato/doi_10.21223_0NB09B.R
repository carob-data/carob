# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Field Evaluation and Nutritional Analysis of Stenotomum and Goniocalyx Potatoes in Ayacucho - 2007

340 accessions of Solanum stenotomum and Goniocalyx were harvested at two locations in Peru: Huancayo and Ayacucho. Sample preparation is currently underway for the determination of mineral content, as well as the quantification of total and individual carotenoid, phenolic, and vitamin C concentrations. Iron, zinc, phenolic, vitamin C, and carotenoid characterizations will be carried out for the Stenotomum and Goniocalyx potato germplasm.'
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/0NB09B"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
	

	f1 <- ff[basename(ff) == "TVIT200711_QUISCH_Ayacucho_Gon_Stn_Field.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Minimal")
	r1b <- carobiner::read.excel(f1, sheet="Installation")
	r1c <- carobiner::read.excel(f1, sheet="Material List")
	r1d <- carobiner::read.excel(f1, sheet="Soil_analysis")
	r1e <- carobiner::read.excel(f1, sheet="Weather_data")
	r1f <- carobiner::read.excel(f1, sheet="Crop_management")
	r1g <- carobiner::read.excel(f1, sheet="Var List")
	r1h <- carobiner::read.excel(f1, sheet="Fieldbook")
	r1i <- carobiner::read.excel(f1, sheet="Summary by clone")
	r1j <- carobiner::read.excel(f1, sheet="Original-VitaminC")
	r1k <- carobiner::read.excel(f1, sheet="Tabla Ayacucho")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame()
##r1a: "Factor", "Value"


	d1b <- data.frame()
##r1b: "Factor", "Value"


	d1c <- data.frame(
		variety = r1c[["Code of clone"]]
	)
##r1c: "Numeration", "Control", "Institutional number", "Clone or variety name", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "Species acronym", "Population Group", "Mapping Population", "Molecular Marker", "Kind marker"


	d1d <- data.frame()
##r1d: "Variables", "Abbreviture", "Unit", "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9", "Data10"


	d1e <- data.frame()
##r1e: "Date of weather observation", "Hour of weather observation", "Rainfall (mm)", "Average temperature (°C)", "Minimum temperature (°C)", "Maximum temperature (°C)", "Temperature amplitude °C", "Relative humidity (%)", "Solar Radiation (w/m2)", "Barometric Pressure (mm)", "Dew point (°C)", "Wind speed (m/s)", "Gust speed", "Wind direction"


	d1f <- data.frame()
##r1f: "Intervention category", "Intervention type", "Date", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Uncertainty of Measurement"


	d1g <- data.frame()
##r1g: "Factor.Variables", "Abbreviations", "Fieldbook", "Summarize", "Analyze", "Selection direction", "Selection weight"


	d1h <- data.frame(
		plot_id = r1h[["PLOT"]],
		rep = r1h[["REP"]]
	)
##r1h: "INSTN", "Code Laboratory", "ASC_DW", "ASC_FW", "DM"


	d1i <- data.frame(
		N_fertilizer = r1i[["n"]]
	)
##r1i: "INSTN", "ASC_DW_mean", "ASC_DW_sd", "ASC_FW_mean", "ASC_FW_sd", "DM_mean", "DM_sd"


	d1j <- data.frame()
##r1j: "EVALUATION OF VITAMIN C", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13"


	d1k <- data.frame()
##r1k: "...1", "...2", "...3", "...4", "Ayacucho 2008...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "Ayacucho 2008...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "Ayacucho 2008...23", "...24", "...25"


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

