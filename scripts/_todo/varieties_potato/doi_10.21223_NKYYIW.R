# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in Uzbekistan (2009)

Trials featuring heat-tolerant potato varieties were conducted in South West Central Asia (SWCA), specifically in Tashkent, Uzbekistan, during the year 2009. In this study, 16 potato clones were evaluated using a randomized complete block design (RCBD) with three repetitions
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/NKYYIW"
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
	

	f1 <- ff[basename(ff) == "PTYL200907_TASHKENT_HEAT.xls"]
	f2 <- ff[basename(ff) == "01_Fieldbook_PTYL200907_TASHKENT_heat.txt"]
	f3 <- ff[basename(ff) == "02_Material_list_PTYL200907_TASHKENT_heat.txt"]
	f4 <- ff[basename(ff) == "03_Data_dictionary.txt"]

	r1a <- carobiner::read.excel(f1, sheet="Minimal")
	r1b <- carobiner::read.excel(f1, sheet="Installation")
	r1c <- carobiner::read.excel(f1, sheet="Material List")
	r1d <- carobiner::read.excel(f1, sheet="Soil_analysis")
	r1e <- carobiner::read.excel(f1, sheet="Weather_data")
	r1f <- carobiner::read.excel(f1, sheet="Crop_management")
	r1g <- carobiner::read.excel(f1, sheet="Var List")
	r1h <- carobiner::read.excel(f1, sheet="Fieldbook")
	r1i <- carobiner::read.excel(f1, sheet="Summary by clone")
	r1j <- carobiner::read.excel(f1, sheet="Charts")
	r1k <- carobiner::read.excel(f1, sheet="Ranks by Clone")
	#r2 <- read.???(f2)
	#r3 <- read.???(f3)
	#r4 <- read.???(f4)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame()
##r1a: "Factor", "Value"


	d1b <- data.frame()
##r1b: "Factor", "Value"


	d1c <- data.frame(
		variety = r1c[["Clone or variety name"]]
	)
##r1c: "Numeration", "Control", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "References to silmultaneous trials", "References to previous trials"


	d1d <- data.frame()
##r1d: "Variables", "Abbreviture", "Unit", "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9", "Data10"


	d1e <- data.frame()
##r1e: "Date of weather observation", "Hour of weather observation", "Rainfall (mm)", "Average temperature (°C)", "Minimum temperature (°C)", "Maximum temperature (°C)", "Temperature amplitude °C", "Relative humidity (%)", "Solar Radiation (PAR uE, min)", "Solar Radiation (PAR uE, avr)", "Solar Radiation (PAR uE, max)", "Barometric Pressure (mm)", "Dew point (°C)", "Wind speed (m/s)", "Gust speed", "Wind direction"


	d1f <- data.frame()
##r1f: "Intervention category", "Intervention type", "Date", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Uncertainty of Measurement"


	d1g <- data.frame()
##r1g: "Factor.Variables", "Abbreviations", "Fieldbook", "Summarize", "Analyze", "Selection direction", "Selection weight", "Min.", "1st Qu.", "Median", "Mode", "3rd Qu.", "Max.", "Mean", "St.Dev.", "N Valid", "N Miss."


	d1h <- data.frame(
		plot_id = r1h[["PLOT"]],
		rep = r1h[["REP"]]
	)
##r1h: "INSTN", "NTP", "NPH", "NMTCI", "NMTCII", "NNoMTP", "TNTP", "TNTPL", "NMTP", "NMTPL", "MTWCI", "MTWCII", "NoMTWP", "TTWP", "TTWPL", "TTYNA", "TTYA", "MTWP", "MTWPL", "ATW", "MTYA", "MTYNA", "PH", "NS", "PC"


	d1i <- data.frame()
##r1i: "INSTN", "NTP_n", "NTP_Mean", "NTP_sd", "NPH_n", "NPH_Mean", "NPH_sd", "NMTCI_n", "NMTCI_Mean", "NMTCI_sd", "NMTCII_n", "NMTCII_Mean", "NMTCII_sd", "NNoMTP_n", "NNoMTP_Mean", "NNoMTP_sd", "TNTP_n", "TNTP_Mean", "TNTP_sd", "TNTPL_n", "TNTPL_Mean", "TNTPL_sd", "NMTP_n", "NMTP_Mean", "NMTP_sd", "NMTPL_n", "NMTPL_Mean", "NMTPL_sd", "MTWCI_n", "MTWCI_Mean", "MTWCI_sd", "MTWCII_n", "MTWCII_Mean", "MTWCII_sd", "NoMTWP_n", "NoMTWP_Mean", "NoMTWP_sd", "TTWP_n", "TTWP_Mean", "TTWP_sd", "TTWPL_n", "TTWPL_Mean", "TTWPL_sd", "TTYA_n", "TTYA_Mean", "TTYA_sd", "TTYNA_n", "TTYNA_Mean", "TTYNA_sd", "MTWP_n", "MTWP_Mean", "MTWP_sd", "MTWPL_n", "MTWPL_Mean", "MTWPL_sd", "MTYA_n", "MTYA_Mean", "MTYA_sd", "MTYNA_n", "MTYNA_Mean", "MTYNA_sd", "ATW_n", "ATW_Mean", "ATW_sd"


	d1j <- data.frame()


	d1k <- data.frame()
##r1k: "INSTN", "TTYA_Mean", "TTYNA_Mean", "MTYA_Mean", "MTYNA_Mean", "TTYA_Rank", "TTYNA_Rank", "MTYA_Rank", "MTYNA_Rank", "RSS", "SWD", "EIM"


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

