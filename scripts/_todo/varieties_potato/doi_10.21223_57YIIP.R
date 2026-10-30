# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones for Performance in Burundi, 2005

An evaluation of 24 potato clones was carried out in the cities of Mugerero , Burundi, in 2005. The study utilized a Completely Randomized Block Design to assess the clones' performance under local conditions.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/57YIIP"
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
	

	f1 <- ff[basename(ff) == "01_data_yield_Mugurero_2005.xlsx"]
	f2 <- ff[basename(ff) == "02_Material_list.xlsx"]
	f3 <- ff[basename(ff) == "03_Data_dictionary.xlsx"]
	f4 <- ff[basename(ff) == "PTYL200507_MUG.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4a <- carobiner::read.excel(f4, sheet="Minimal")
	r4b <- carobiner::read.excel(f4, sheet="Installation")
	r4c <- carobiner::read.excel(f4, sheet="Material List")
	r4d <- carobiner::read.excel(f4, sheet="Soil_analysis")
	r4e <- carobiner::read.excel(f4, sheet="Hobo_data")
	r4f <- carobiner::read.excel(f4, sheet="Weather_data")
	r4g <- carobiner::read.excel(f4, sheet="Weather_Original")
	r4h <- carobiner::read.excel(f4, sheet="Weather_daily")
	r4i <- carobiner::read.excel(f4, sheet="Weather_monthly")
	r4j <- carobiner::read.excel(f4, sheet="Crop_management")
	r4k <- carobiner::read.excel(f4, sheet="Var List")
	r4l <- carobiner::read.excel(f4, sheet="Fieldbook")
	r4m <- carobiner::read.excel(f4, sheet="Original")
	r4n <- carobiner::read.excel(f4, sheet="Fieldbook_backup")
	r4o <- carobiner::read.excel(f4, sheet="Summary by clone")
	r4p <- carobiner::read.excel(f4, sheet="Charts")
	r4q <- carobiner::read.excel(f4, sheet="Ranks by Clone")
	r4r <- carobiner::read.excel(f4, sheet="TTYA")
	r4s <- carobiner::read.excel(f4, sheet="MTWP")
	r4t <- carobiner::read.excel(f4, sheet="MTYA")

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
		variety = r2[["Clone or variety name"]]
	)
##r2: "Numeration", "Institutional number", "Code of clone", "Female Institutional number", "Female code", "Male Institutional number", "Male code"


	d3 <- data.frame(
		crop = tolower(r3[["Crop"]])
	)
##r3: "numeration", "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref"


	d4a <- data.frame()
##r4a: "Factor", "Value"


	d4b <- data.frame()
##r4b: "Factor", "Value"


	d4c <- data.frame(
		variety = r4c[["Clone or variety name"]]
	)
##r4c: "Numeration", "Control", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "References to silmultaneous trials", "References to previous trials"


	d4d <- data.frame()
##r4d: "Variables", "Abbreviture", "Unit", "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9", "Data10"


	d4e <- data.frame()
##r4e: "Date", "Hour of weather observation", "Temperature °C", "Relative humidity (%)"


	d4f <- data.frame()
##r4f: "Date of weather observation", "Hour of weather observation", "Rainfall (mm)", "Average temperature (°C)", "Minimum temperature (°C)", "Maximum temperature (°C)", "Temperature amplitude °C", "Relative humidity (%)", "Solar Radiation (w/m2)", "Barometric Pressure (mm)", "Dew point (°C)", "Wind speed (m/s)", "Gust speed", "Wind direction"


	d4g <- data.frame(
		year = r4g[["Year"]],
		country = r4g[["Country"]],
		adm1 = carobiner::fix_name(r4g[["Adm_1"]], "title")
	)
##r4g: "Date", "Time", "Month", "Day", "Locality", "Temperature", "Time Interval, h"


	d4h <- data.frame(
		country = r4h[["Country"]],
		year = r4h[["Year"]],
		N_fertilizer = r4h[["n_sample"]]
	)
##r4h: "Department", "Locality", "Month", "Day", "TMIN", "TMAX", "TMEAN", "TSD"


	d4i <- data.frame(
		country = r4i[["Country"]],
		year = r4i[["Year"]],
		N_fertilizer = r4i[["n_sample"]]
	)
##r4i: "Department", "Locality", "Month", "TMIN", "TMAX", "TMEAN", "TSD"


	d4j <- data.frame()
##r4j: "Intervention category", "Intervention type", "Date", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Uncertainty of Measurement"


	d4k <- data.frame()
##r4k: "Factor.Variables", "Abbreviations", "Fieldbook", "Summarize", "Analyze", "Selection direction", "Selection weight", "Min.", "1st Qu.", "Median", "Mode", "3rd Qu.", "Max.", "Mean", "St.Dev.", "N Valid", "N Miss."


	d4l <- data.frame(
		plot_id = r4l[["PLOT"]],
		rep = r4l[["REP"]]
	)
##r4l: "INSTN", "NTP", "NPE", "NPH", "PPH", "NNoMTP", "NMTP", "TNTP", "NoMTWP", "TTWP", "TTWPL", "TTYA", "TTYNA", "MTWP", "MTWPL", "MTYA", "MTYNA", "ATW", "ATMW", "AVDM"


	d4m <- data.frame()
##r4m: "Accession name", "Génotype", "Répétition", "...4", "Mat.sèche %", "Plants levés", "Levée corrig", "Poq récoltés", "Nbre tub cons.", "Poids cons.", "Poids billes", "Nbre tub non cons", "Nbre tot tuberc", "Cons + billes", "Poids tot g/plant", "Rdt tot t/ha", "Rdt cons t/ha", "Rdt bille t/ha", "Taux levée (%)", "% poids billes", "% nbre billes", "Tuberc/plant", "Cons sèche t/ha", "Rdt tot sec t/ha"


	d4n <- data.frame(
		plot_id = r4n[["PLOT"]],
		rep = r4n[["REP"]]
	)
##r4n: "INSTN", "NTP", "NPE", "NPH", "PPH", "NNoMTP", "NMTP", "TNTP", "NoMTWP", "TTWP", "TTWPL", "TTYA", "TTYNA", "MTWP", "MTWPL", "MTYA", "MTYNA", "ATW", "ATMW", "AVDM"


	d4o <- data.frame()
##r4o: "INSTN", "NTP_n", "NTP_Mean", "NTP_sd", "NPH_n", "NPH_Mean", "NPH_sd", "PPH_n", "PPH_Mean", "PPH_sd", "TNTP_n", "TNTP_Mean", "TNTP_sd", "TTWP_n", "TTWP_Mean", "TTWP_sd", "TTWPL_n", "TTWPL_Mean", "TTWPL_sd", "TTYA_n", "TTYA_Mean", "TTYA_sd", "TTYNA_n", "TTYNA_Mean", "TTYNA_sd", "MTWP_n", "MTWP_Mean", "MTWP_sd", "MTWPL_n", "MTWPL_Mean", "MTWPL_sd", "MTYA_n", "MTYA_Mean", "MTYA_sd", "MTYNA_n", "MTYNA_Mean", "MTYNA_sd", "ATW_n", "ATW_Mean", "ATW_sd", "ATMW_n", "ATMW_Mean", "ATMW_sd"


	d4p <- data.frame()


	d4q <- data.frame()
##r4q: "INSTN", "TTYA_Mean", "TTYNA_Mean", "MTWP_Mean", "MTYA_Mean", "MTYNA_Mean", "TTYA_Rank", "TTYNA_Rank", "MTWP_Rank", "MTYA_Rank", "MTYNA_Rank", "RSS", "SWD", "EIM"


	d4r <- data.frame()
##r4r: "################################################################################"


	d4s <- data.frame()
##r4s: "################################################################################"


	d4t <- data.frame()
##r4t: "################################################################################"


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

