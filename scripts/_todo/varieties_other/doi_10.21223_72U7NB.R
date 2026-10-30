# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Mass screening of sweetpotato germplasm for heat tolerance

This dataset presents the results of a screening for heat stress tolerance of 1973 sweetpotato accessions under field conditions in the coastal desert of Northern Peru (Piura). The experiment was laid out as an alpha lattice design with two replications in two consecutive seasons (2013-2014) characterized by hot and moderate temperatures representing a heat stress and a non-heat stress treatment. Yield performance metrics such as storage root yield, vine yield, total biomass yield, number of marketable roots, harvest index, dry matter content and  normalized difference vegetation index (NDVI) were recorded at harvest at 120 days after planting. The dataset includes selected morhological descriptors and passportdata elements  (elevation of germplasm collecting site, country of origin) originating from the database of CIP genebank . The original datasets of the screened accessions can be found at http://genebank.cipotato.org/gringlobal/search.aspx or https://www.genesys-pgr.org/. Climatic data were recorded (air temperature, air humidity, precipitation, solar radiation, and wind speed) every hour with a HOBObase weather station U30 4.0 (Onset Computer Corporation, MA, USA).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/72U7NB"
	group <- "varieties_other"
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
	

	f1 <- ff[basename(ff) == "Data dictionary_swp_heat stress screening.xlsx"]
	f2 <- ff[basename(ff) == "Meteorol_data Piura.xlsb.xlsx"]
	f3 <- ff[basename(ff) == "elevation.csv"]
	f4 <- ff[basename(ff) == "geographic_origin_accessions.csv"]
	f5 <- ff[basename(ff) == "harvest_canopy_traits_mean_across_rep.csv"]
	f6 <- ff[basename(ff) == "harvest_traits_mean_across_rep.csv"]
	f7 <- ff[basename(ff) == "morph_data_biostat2.csv"]

	r1a <- carobiner::read.excel(f1, sheet="agron_data dictionary")
	r1b <- carobiner::read.excel(f1, sheet="basic passport data")
	r1c <- carobiner::read.excel(f1, sheet="clim. data")
	r1d <- carobiner::read.excel(f1, sheet="morph_descriptors")
	r2a <- carobiner::read.excel(f2, sheet="Sheet1")
	r2b <- carobiner::read.excel(f2, sheet="Desc_HID")
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)
	r6 <- read.csv(f6)
	r7 <- read.csv(f7)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		crop = tolower(r1a[["Crop"]])
	)
##r1a: "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Formula abbreviation", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9", "Category 10", "Category 11", "Category 12", "Category 13", "Category 14"


	d1b <- data.frame()
##r1b: "Attribute Name", "Description", "Structure", "Abbreviation", "Reference/Source"


	d1c <- data.frame()
##r1c: "Variable name", "Abbreviation", "Unit", "Description"


	d1d <- data.frame(
		crop = tolower(r1d[["Crop"]])
	)
##r1d: "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Formula abbreviation", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9", "Reference", "Source"


	d2a <- data.frame()
##r2a: "Date Time, GMT-05:00", "Temp, °C (LGR S/N: 10292755, SEN S/N: 10238859)", "RH, % (LGR S/N: 10292755, SEN S/N: 10238859)", "Solar Radiation, W/m² (LGR S/N: 10292755, SEN S/N: 10280981)", "VPD kPa"


	d2b <- data.frame()
##r2b: "1...1", "41640.033206018517", "1...3", "41640.074872685182"


	d1 <- data.frame(
		country = r1[["CIP_number.Country_co.Elevation"]]
	)


	d2 <- data.frame()


	d3 <- data.frame()


	d4 <- data.frame()


	d5 <- data.frame()


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

