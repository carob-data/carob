# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones for Performance in Burundi, 2006

An evaluation of 11 potato clones was carried out in the cities of Mugerero and Mparambo, Burundi, in 2006. The study utilized a Completely Randomized Block Design to assess the clones' performance under local conditions.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/1G3G6P"
	group <- "varieties_potato"
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
	

	f1 <- ff[basename(ff) == "01_data_yield_Mparambo_2006.xlsx"]
	f2 <- ff[basename(ff) == "02_data_yield_Mugerero_2006.xlsx"]
	f3 <- ff[basename(ff) == "03_Material_list.xlsx"]
	f4 <- ff[basename(ff) == "04_Data_dictionary.xlsx"]
	f5 <- ff[basename(ff) == "PTYL200606_MPA.xls"]
	f6 <- ff[basename(ff) == "PTYL200607_MUG.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5a <- carobiner::read.excel(f5, sheet="Minimal")
	r5b <- carobiner::read.excel(f5, sheet="Installation")
	r5c <- carobiner::read.excel(f5, sheet="Material List")
	r5d <- carobiner::read.excel(f5, sheet="Soil_analysis")
	r5e <- carobiner::read.excel(f5, sheet="Hobo_data")
	r5f <- carobiner::read.excel(f5, sheet="Weather_data")
	r5g <- carobiner::read.excel(f5, sheet="Weather_Original")
	r5h <- carobiner::read.excel(f5, sheet="Weather_daily")
	r5i <- carobiner::read.excel(f5, sheet="Weather_monthly")
	r5j <- carobiner::read.excel(f5, sheet="Crop_management")
	r5k <- carobiner::read.excel(f5, sheet="Var List")
	r5l <- carobiner::read.excel(f5, sheet="Fieldbook")
	r5m <- carobiner::read.excel(f5, sheet="Original")
	r5n <- carobiner::read.excel(f5, sheet="datos_orig")
	r5o <- carobiner::read.excel(f5, sheet="Fieldbook_backup")
	r5p <- carobiner::read.excel(f5, sheet="Summary by clone")
	r5q <- carobiner::read.excel(f5, sheet="Charts")
	r5r <- carobiner::read.excel(f5, sheet="Ranks by Clone")
	r6a <- carobiner::read.excel(f6, sheet="Minimal")
	r6b <- carobiner::read.excel(f6, sheet="Installation")
	r6c <- carobiner::read.excel(f6, sheet="Material List")
	r6d <- carobiner::read.excel(f6, sheet="Soil_analysis")
	r6e <- carobiner::read.excel(f6, sheet="Hobo_data")
	r6f <- carobiner::read.excel(f6, sheet="Weather_data")
	r6g <- carobiner::read.excel(f6, sheet="Weather_Original")
	r6h <- carobiner::read.excel(f6, sheet="Weather_daily")
	r6i <- carobiner::read.excel(f6, sheet="Weather_monthly")
	r6j <- carobiner::read.excel(f6, sheet="Crop_management")
	r6k <- carobiner::read.excel(f6, sheet="Var List")
	r6l <- carobiner::read.excel(f6, sheet="Fieldbook")
	r6m <- carobiner::read.excel(f6, sheet="Original")
	r6n <- carobiner::read.excel(f6, sheet="Sheet2")
	r6o <- carobiner::read.excel(f6, sheet="Fieldbook_backup")
	r6p <- carobiner::read.excel(f6, sheet="Summary by clone")
	r6q <- carobiner::read.excel(f6, sheet="Charts")
	r6r <- carobiner::read.excel(f6, sheet="Ranks by Clone")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		plot_id = r1[["PLOT"]],
		rep = r1[["REP"]],
		country = r1[["Country"]],
		adm1 = carobiner::fix_name(r1[["Admin1"]], "title"),
		adm2 = carobiner::fix_name(r1[["Admin2"]], "title")
	)
##r1: "INSTN", "Locality", "NTP", "NPE", "NPH", "PPH", "NNoMTP", "NMTP", "TNTP", "NoMTWP", "TTWP", "TTWPL", "TTYA", "TTYNA", "MTWP", "MTWPL", "MTYA", "MTYNA", "ATW", "ATMW", "AVDM"


	d2 <- data.frame(
		plot_id = r2[["PLOT"]],
		rep = r2[["REP"]],
		country = r2[["Country"]],
		adm1 = carobiner::fix_name(r2[["Admin1"]], "title"),
		adm2 = carobiner::fix_name(r2[["Admin2"]], "title")
	)
##r2: "INSTN", "Locality", "NTP", "NPE", "NPH", "PPH", "NNoMTP", "NMTP", "TNTP", "NoMTWP", "TTWP", "TTWPL", "TTYA", "TTYNA", "MTWP", "MTWPL", "MTYA", "MTYNA", "ATW", "ATMW", "AVDM"


	d3 <- data.frame(
		variety = r3[["Clone or variety name"]]
	)
##r3: "Numeration", "Institutional number", "Code of clone", "Female Institutional number", "Female code", "Male Institutional number", "Male code"


	d4 <- data.frame(
		crop = tolower(r4[["Crop"]])
	)
##r4: "numeration", "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref"


	d5a <- data.frame()
##r5a: "Factor", "Value"


	d5b <- data.frame()
##r5b: "Factor", "Value"


	d5c <- data.frame(
		variety = r5c[["Clone or variety name"]]
	)
##r5c: "Numeration", "Control", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "References to silmultaneous trials", "References to previous trials"


	d5d <- data.frame()
##r5d: "Variables", "Abbreviture", "Unit", "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9", "Data10"


	d5e <- data.frame()
##r5e: "Date", "Hour of weather observation", "Temperature °C", "Relative humidity (%)"


	d5f <- data.frame()
##r5f: "Date of weather observation", "Hour of weather observation", "Rainfall (mm)", "Average temperature (°C)", "Minimum temperature (°C)", "Maximum temperature (°C)", "Temperature amplitude °C", "Relative humidity (%)", "Solar Radiation (w/m2)", "Barometric Pressure (mm)", "Dew point (°C)", "Wind speed (m/s)", "Gust speed", "Wind direction"


	d5g <- data.frame(
		year = r5g[["Year"]],
		country = r5g[["Country"]],
		adm1 = carobiner::fix_name(r5g[["Adm_1"]], "title")
	)
##r5g: "Date", "Time", "Month", "Day", "Locality", "Temperature", "Time Interval, h"


	d5h <- data.frame(
		country = r5h[["Country"]],
		year = r5h[["Year"]],
		N_fertilizer = r5h[["n_sample"]]
	)
##r5h: "Department", "Locality", "Month", "Day", "TMIN", "TMAX", "TMEAN", "TSD"


	d5i <- data.frame(
		country = r5i[["Country"]],
		year = r5i[["Year"]],
		N_fertilizer = r5i[["n_sample"]]
	)
##r5i: "Department", "Locality", "Month", "TMIN", "TMAX", "TMEAN", "TSD"


	d5j <- data.frame()
##r5j: "Intervention category", "Intervention type", "Date", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Uncertainty of Measurement"


	d5k <- data.frame()
##r5k: "Factor.Variables", "Abbreviations", "Fieldbook", "Summarize", "Analyze", "Selection direction", "Selection weight", "Min....8", "1st Qu....9", "Median...10", "Mode...11", "3rd Qu....12", "Max....13", "Mean...14", "St.Dev....15", "N Valid...16", "N Miss....17", "Min....18", "1st Qu....19", "Median...20", "Mode...21", "3rd Qu....22", "Max....23", "Mean...24", "St.Dev....25", "N Valid...26", "N Miss....27"


	d5l <- data.frame(
		plot_id = r5l[["PLOT"]],
		rep = r5l[["REP"]]
	)
##r5l: "INSTN", "NTP", "NPE", "NPH", "PPH", "NNoMTP", "NMTP", "TNTP", "NoMTWP", "TTWP", "TTWPL", "TTYA", "TTYNA", "MTWP", "MTWPL", "MTYA", "MTYNA", "ATW", "ATMW", "AVDM"


	d5m <- data.frame(
		rep = r5m[["Rep"]]
	)
##r5m: "num", "Genotype", "NTP", "NPE", "NPH", "NMTP", "NNoMTP", "MTWP", "NoMTWP", "AVDM", "...12", "Accession number", "MTWP kg", "NoMTWP kg"


	d5n <- data.frame()
##r5n: "Design:", "RCBD"


	d5o <- data.frame(
		plot_id = r5o[["PLOT"]],
		rep = r5o[["REP"]]
	)
##r5o: "INSTN", "NTP", "NPE", "NPH", "PPH", "NNoMTP", "NMTP", "TNTP", "NoMTWP", "TTWP", "TTWPL", "TTYA", "TTYNA", "MTWP", "MTWPL", "MTYA", "MTYNA", "ATW", "ATMW", "AVDM"


	d5p <- data.frame()
##r5p: "INSTN", "NPH_n", "NPH_Mean", "NPH_sd", "PPH_n", "PPH_Mean", "PPH_sd", "TNTP_n", "TNTP_Mean", "TNTP_sd", "TTWP_n", "TTWP_Mean", "TTWP_sd", "TTWPL_n", "TTWPL_Mean", "TTWPL_sd", "TTYA_n", "TTYA_Mean", "TTYA_sd", "TTYNA_n", "TTYNA_Mean", "TTYNA_sd", "MTWP_n", "MTWP_Mean", "MTWP_sd", "MTWPL_n", "MTWPL_Mean", "MTWPL_sd", "MTYA_n", "MTYA_Mean", "MTYA_sd", "MTYNA_n", "MTYNA_Mean", "MTYNA_sd", "ATW_n", "ATW_Mean", "ATW_sd", "ATMW_n", "ATMW_Mean", "ATMW_sd", "AVDM_n", "AVDM_Mean", "AVDM_sd"


	d5q <- data.frame()


	d5r <- data.frame()
##r5r: "INSTN", "TTYA_Mean", "TTYNA_Mean", "MTWP_Mean", "MTYA_Mean", "MTYNA_Mean", "TTYA_Rank", "TTYNA_Rank", "MTWP_Rank", "MTYA_Rank", "MTYNA_Rank", "RSS", "SWD", "EIM"


	d6a <- data.frame()
##r6a: "Factor", "Value"


	d6b <- data.frame()
##r6b: "Factor", "Value"


	d6c <- data.frame(
		variety = r6c[["Clone or variety name"]]
	)
##r6c: "Numeration", "Control", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "References to silmultaneous trials", "References to previous trials"


	d6d <- data.frame()
##r6d: "Variables", "Abbreviture", "Unit", "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9", "Data10"


	d6e <- data.frame()
##r6e: "Date", "Hour of weather observation", "Temperature °C", "Relative humidity (%)"


	d6f <- data.frame()
##r6f: "Date of weather observation", "Hour of weather observation", "Rainfall (mm)", "Average temperature (°C)", "Minimum temperature (°C)", "Maximum temperature (°C)", "Temperature amplitude °C", "Relative humidity (%)", "Solar Radiation (w/m2)", "Barometric Pressure (mm)", "Dew point (°C)", "Wind speed (m/s)", "Gust speed", "Wind direction"


	d6g <- data.frame(
		year = r6g[["Year"]],
		country = r6g[["Country"]],
		adm1 = carobiner::fix_name(r6g[["Adm_1"]], "title")
	)
##r6g: "Date", "Time", "Month", "Day", "Locality", "Temperature", "Time Interval, h"


	d6h <- data.frame(
		country = r6h[["Country"]],
		year = r6h[["Year"]],
		N_fertilizer = r6h[["n_sample"]]
	)
##r6h: "Department", "Locality", "Month", "Day", "TMIN", "TMAX", "TMEAN", "TSD"


	d6i <- data.frame(
		country = r6i[["Country"]],
		year = r6i[["Year"]],
		N_fertilizer = r6i[["n_sample"]]
	)
##r6i: "Department", "Locality", "Month", "TMIN", "TMAX", "TMEAN", "TSD"


	d6j <- data.frame()
##r6j: "Intervention category", "Intervention type", "Date", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Uncertainty of Measurement"


	d6k <- data.frame()
##r6k: "Factor.Variables", "Abbreviations", "Fieldbook", "Summarize", "Analyze", "Selection direction", "Selection weight", "Min.", "1st Qu.", "Median", "Mode", "3rd Qu.", "Max.", "Mean", "St.Dev.", "N Valid", "N Miss."


	d6l <- data.frame(
		plot_id = r6l[["PLOT"]],
		rep = r6l[["REP"]]
	)
##r6l: "INSTN", "NTP", "NPE", "NPH", "PPH", "NNoMTP", "NMTP", "TNTP", "NoMTWP", "TTWP", "TTWPL", "TTYA", "TTYNA", "MTWP", "MTWPL", "MTYA", "MTYNA", "ATW", "ATMW", "AVDM"


	d6m <- data.frame(
		rep = r6m[["Rep"]],
		variety = r6m[["clones"]]
	)
##r6m: "datos", "Genotype", "NTP", "NPE", "NPH", "NMTP", "NNoMTP", "MTWP gr", "NoMTWP gr", "AVDM", "...12", "...13", "comp", "...16", "MTWP kg", "NoMTWP kg"


	d6n <- data.frame()
##r6n: "Design:", "RCBD"


	d6o <- data.frame(
		plot_id = r6o[["PLOT"]],
		rep = r6o[["REP"]]
	)
##r6o: "INSTN", "NTP", "NPE", "NPH", "PPH", "NNoMTP", "NMTP", "TNTP", "NoMTWP", "TTWP", "TTWPL", "TTYA", "TTYNA", "MTWP", "MTWPL", "MTYA", "MTYNA", "ATW", "ATMW", "AVDM"


	d6p <- data.frame()
##r6p: "INSTN", "NPH_n", "NPH_Mean", "NPH_sd", "PPH_n", "PPH_Mean", "PPH_sd", "TNTP_n", "TNTP_Mean", "TNTP_sd", "TTWP_n", "TTWP_Mean", "TTWP_sd", "TTWPL_n", "TTWPL_Mean", "TTWPL_sd", "TTYA_n", "TTYA_Mean", "TTYA_sd", "TTYNA_n", "TTYNA_Mean", "TTYNA_sd", "MTWP_n", "MTWP_Mean", "MTWP_sd", "MTWPL_n", "MTWPL_Mean", "MTWPL_sd", "MTYA_n", "MTYA_Mean", "MTYA_sd", "MTYNA_n", "MTYNA_Mean", "MTYNA_sd", "ATW_n", "ATW_Mean", "ATW_sd", "ATMW_n", "ATMW_Mean", "ATMW_sd", "AVDM_n", "AVDM_Mean", "AVDM_sd"


	d6q <- data.frame()


	d6r <- data.frame()
##r6r: "INSTN", "TTYA_Mean", "TTYNA_Mean", "MTWP_Mean", "MTYA_Mean", "MTYNA_Mean", "TTYA_Rank", "TTYNA_Rank", "MTWP_Rank", "MTYA_Rank", "MTYNA_Rank", "RSS", "SWD", "EIM"


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

