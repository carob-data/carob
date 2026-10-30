# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Evaluation of the Morphological Characterization of Native Potato Varieties in Tintec, Paucartambo - Cusco, 2024

In the Tintec field, situated in the province of Paucartambo, Cusco, 112 native potato varieties were cultivated under zero-tillage farming systems. The agronomic management activities included Yapuy (maintenance), Jaimay (first earthing-up), and Qasuy (final maintenance), incorporating traditional farming knowledge to promote sustainability. The experiment evaluated the morphological traits of native potato varieties, focusing on tuber characteristics (size, shape, and skin color) and flower features (petal color and structure). This study emphasizes assessing the agronomic performance of these varieties while conserving genetic diversity and improving agricultural systems in the Paucartambo region.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/FMTPF2"
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
	

	f1 <- ff[basename(ff) == "02_Data_dictionary.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Dictionary")
	r1b <- carobiner::read.excel(f1, sheet="Sheet2")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		crop = tolower(r1a[["Crop"]])
	)
##r1a: "Numeration", "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9", "Category 10"


	d1b <- data.frame(
		plot_id = r1b[["NTbPl_Ct_plplot"]],
		crop = tolower(r1b[["Crop research"]])
	)
##r1b: "CO_330:0000265", "...3", "NTP", "...5", "Evaluation in Breeding Trials", "...7", "Recommended...8", "...9", "CIP", "Breeders", "42859", "EN", "Potato", "CO_330:0000263", "Number of tubers planted", "The number of planted tubers", "Plants planted number/  Planted (sett)", "NTbPl", "...21", "planted tuber", "number", "Recommended...24", "De Haan, S et. al 2014...25", "CO_330:0000264", "Number of tubers planted - method", "Counting", "Count the number of planted tubers and record it", "...30", "De Haan, S et. al 2014...31", "CO_330:0000219", "tuber/plot", "Numerical", "0...35", "0...36", "500", "De Haan, S et. al 2014...38"


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

