# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Stability analysis for nutritional contents (phenolics) on 40 native potato cultivars -2009

'The analysis of phenolic content in potato cultivars aims to evaluate their nutritional quality and potential as a source of bioactive compounds, known for their health benefits such as antioxidant activity. This study focuses on identifying variations in phenolic content among different genotypes and their interaction with environmental conditions to select varieties with high stability and consistent phenolic concentrations.

Forty native potato cultivars were evaluated to analyze the magnitude and nature of Genotype (G), Environment (E), and GxE interaction effects on phenolic compound concentrations in the tropical highlands of Peru, as well as to study the contribution of soil fertility to the phenolic content in potato tubers. The cultivars were grown in randomized complete block designs with three replications of one hill per plot in six sites in the central Peruvian Andes in 2010: Ccasapata (3765 masl), Sotopampa (3754 masl), Ccollpaccasa (4067 masl), Conayca (4178 masl), La Victoria (3265 masl), and Rangra (3323 masl). Fully matured tubers were harvested at 150 and 180 days and analyzed via spectrophotometry to determine their phenolic content. This analysis explored the effects of genetic and environmental factors on tuber quality.

Statistical analyses included ANOVA to assess differences between genotypes and environments, as well as the AMMI model (Additive Main Effects and Multiplicative Interaction) to study genotype stability and adaptability. This comprehensive approach provides critical information for breeding programs, strategies to conserve native varieties, and the development of cultivars with outstanding nutritional characteristics that meet market demands and contribute to agricultural biodiversity.'
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/FH5RX0"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; YANAPAI",
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
	

	f1 <- ff[basename(ff) == "07_Data_dictionary_Stability_phenolic.xlsx"]
	f2 <- ff[basename(ff) == "TPHE200911_MRSCAC_Stability_Field.xls"]
	f3 <- ff[basename(ff) == "TPHE200912_SOTOP_Stability_Field.xls"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="Minimal")
	r2b <- carobiner::read.excel(f2, sheet="Installation")
	r2c <- carobiner::read.excel(f2, sheet="Material List")
	r2d <- carobiner::read.excel(f2, sheet="Soil_analysis")
	r2e <- carobiner::read.excel(f2, sheet="Weather_data")
	r2f <- carobiner::read.excel(f2, sheet="Crop_management")
	r2g <- carobiner::read.excel(f2, sheet="Var List")
	r2h <- carobiner::read.excel(f2, sheet="Fieldbook")
	r2i <- carobiner::read.excel(f2, sheet="Summary by clone")
	r2j <- carobiner::read.excel(f2, sheet="Results all")
	r3a <- carobiner::read.excel(f3, sheet="Minimal")
	r3b <- carobiner::read.excel(f3, sheet="Installation")
	r3c <- carobiner::read.excel(f3, sheet="Material List")
	r3d <- carobiner::read.excel(f3, sheet="Soil_analysis")
	r3e <- carobiner::read.excel(f3, sheet="Weather_data")
	r3f <- carobiner::read.excel(f3, sheet="Crop_management")
	r3g <- carobiner::read.excel(f3, sheet="Var List")
	r3h <- carobiner::read.excel(f3, sheet="Fieldbook")
	r3i <- carobiner::read.excel(f3, sheet="Summary by clone")
	r3j <- carobiner::read.excel(f3, sheet="Results all")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		crop = tolower(r1[["Crop"]])
	)
##r1: "numeration", "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref"


	d2a <- data.frame()
##r2a: "Factor", "Value"


	d2b <- data.frame()
##r2b: "Factor", "Value"


	d2c <- data.frame(
		variety = r2c[["Clone or variety name"]]
	)
##r2c: "Numeration", "Control", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "References to silmultaneous trials", "References to previous trials", "Species acronym"


	d2d <- data.frame()
##r2d: "Variables", "Abbreviture", "Unit", "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9", "Data10"


	d2e <- data.frame(
		year = r2e[["Year"]]
	)
##r2e: "Day", "Month", "Time", "Intervale time", "Rainfall (mm)", "Average temperature (°C)", "Relative humidity (%)", "Dew point (°C)", "Solar Radiation (w/m2)", "Temperature Soil 1", "Temperature Soil 2", "Temperature Soil 3", "Temperature Soil 4", "Water Content 1", "Water Content 2", "Water Content 3", "Water Content 4"


	d2f <- data.frame()
##r2f: "Intervention category", "Intervention type", "Date", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Uncertainty of Measurement"


	d2g <- data.frame()
##r2g: "Factor.Variables", "Abbreviations", "Fieldbook", "Summarize", "Analyze", "Selection direction", "Selection weight"


	d2h <- data.frame(
		plot_id = r2h[["PLOT"]],
		rep = r2h[["REP"]]
	)
##r2h: "INSTN", "Sample", "Code Laboratory", "DM", "TP_DW", "TP_FW", "TA_DW", "TA_FW", "CH_DW", "CH_FW", "Trip_DW", "Trip_FW", "CafAc_DW", "CafAc_FW", "AntCap_DW", "AntCap_FW"


	d2i <- data.frame(
		N_fertilizer = r2i[["n"]]
	)
##r2i: "INSTN", "AntCap_FW_Mean", "AntCap_FW_SD", "AntCap_DW_Mean", "AntCap_DW_SD", "Trip_FW_Mean", "Trip_FW_SD", "Trip_DW_Mean", "Trip_DW_SD", "CafAc_FW_Mean", "CafAc_FW_SD", "CafAc_DW_Mean", "CafAc_DW_SD", "CH_FW_Mean", "CH_FW_SD", "CH_DW_Mean", "CH_DW_SD", "TA_FW_Mean", "TA_FW_SD", "TA_DW_Mean", "TA_DW_SD", "TP_FW_Mean", "TP_FW_SD", "TP_DW_Mean", "TP_DW_SD", "DM_Mean", "DM_SD"


	d2j <- data.frame(
		location = r2j[["Total phenolic compounds (CFT), total anthocyanin, chlorogenic acid, cafeic acid and tryptophan concentration and antioxidant capacity of 8 native potato varieties in 5 locations"]]
	)
##r2j: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15"


	d3a <- data.frame()
##r3a: "Factor", "Value"


	d3b <- data.frame()
##r3b: "Factor", "Value"


	d3c <- data.frame(
		variety = r3c[["Clone or variety name"]]
	)
##r3c: "Numeration", "Control", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "References to silmultaneous trials", "References to previous trials", "Species acronym"


	d3d <- data.frame()
##r3d: "Variables", "Abbreviture", "Unit", "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9", "Data10"


	d3e <- data.frame(
		year = r3e[["Year"]]
	)
##r3e: "Day", "Month", "Time", "Intervale time", "Rainfall (mm)", "Average temperature (°C)", "Relative humidity (%)", "Dew point (°C)", "Solar Radiation (w/m2)", "Temperature Soil 1", "Temperature Soil 2", "Temperature Soil 3", "Temperature Soil 4", "Water Content 1", "Water Content 2", "Water Content 3", "Water Content 4"


	d3f <- data.frame()
##r3f: "Intervention category", "Intervention type", "Date", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Uncertainty of Measurement"


	d3g <- data.frame()
##r3g: "Factor.Variables", "Abbreviations", "Fieldbook", "Summarize", "Analyze", "Selection direction", "Selection weight"


	d3h <- data.frame(
		plot_id = r3h[["PLOT"]],
		rep = r3h[["REP"]]
	)
##r3h: "INSTN", "Sample", "Code Laboratory", "DM", "TP_DW", "TP_FW", "TA_DW", "TA_FW", "CH_DW", "CH_FW", "Trip_DW", "Trip_FW", "CafAc_DW", "CafAc_FW", "AntCap_DW", "AntCap_FW"


	d3i <- data.frame(
		N_fertilizer = r3i[["n"]]
	)
##r3i: "INSTN", "AntCap_FW_Mean", "AntCap_FW_SD", "AntCap_DW_Mean", "AntCap_DW_SD", "Trip_FW_Mean", "Trip_FW_SD", "Trip_DW_Mean", "Trip_DW_SD", "CafAc_FW_Mean", "CafAc_FW_SD", "CafAc_DW_Mean", "CafAc_DW_SD", "CH_FW_Mean", "CH_FW_SD", "CH_DW_Mean", "CH_DW_SD", "TA_FW_Mean", "TA_FW_SD", "TA_DW_Mean", "TA_DW_SD", "TP_FW_Mean", "TP_FW_SD", "TP_DW_Mean", "TP_DW_SD", "DM_Mean", "DM_SD"


	d3j <- data.frame(
		location = r3j[["Total phenolic compounds (CFT), total anthocyanin, chlorogenic acid, cafeic acid and tryptophan concentration and antioxidant capacity of 8 native potato varieties in 5 locations"]]
	)
##r3j: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14"


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

