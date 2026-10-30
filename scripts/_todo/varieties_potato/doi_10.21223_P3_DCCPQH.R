# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Ecophysiological and remotely sensed measurements taken in a trial testing potential growth of the potato minicore (2017)

Field experiment to assess physiological performance of 24 different genotypes, using ecophysiologial traits (net photosynthesis, stomatal conductance, carbon isotope discrimination, fluorescence), yield and remotely sensed variables (leaf area, canopy cover, NDVI) taken by octocopter (drone - 14 multiespectral aerial images were adquired) along the growing season of the first trial of potato minicore carried out under an Augmented design during July-November 2017 at La Molina CIP experimental station (Lima-Peru)
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/DCCPQH"
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
	

	f1 <- ff[basename(ff) == "Data_dictionary.xls"]
	f2 <- ff[basename(ff) == "Data_dictionary_v2.xlsx"]
	f3 <- ff[basename(ff) == "Image_description_v2.xlsx"]
	f4 <- ff[basename(ff) == "MINICORE 2017 DATA SET.xls"]
	f5 <- ff[basename(ff) == "Minicore_2017_v2.xlsx"]
	f6 <- ff[basename(ff) == "Minicore_NIRRG_170720.tif"]
	f7 <- ff[basename(ff) == "Minicore_NIRRG_170726.tif"]
	f8 <- ff[basename(ff) == "Minicore_NIRRG_170802.tif"]
	f9 <- ff[basename(ff) == "Minicore_NIRRG_170809.tif"]
	f10 <- ff[basename(ff) == "Minicore_NIRRG_170816.tif"]
	f11 <- ff[basename(ff) == "Minicore_NIRRG_170823.tif"]
	f12 <- ff[basename(ff) == "Minicore_NIRRG_170831.tif"]
	f13 <- ff[basename(ff) == "Minicore_NIRRG_170906.tif"]
	f14 <- ff[basename(ff) == "Minicore_NIRRG_170914.tif"]
	f15 <- ff[basename(ff) == "Minicore_NIRRG_170922.tif"]
	f16 <- ff[basename(ff) == "Minicore_NIRRG_170927.tif"]
	f17 <- ff[basename(ff) == "Minicore_NIRRG_171006.tif"]
	f18 <- ff[basename(ff) == "Minicore_NIRRG_171010.tif"]
	f19 <- ff[basename(ff) == "Minicore_NIRRG_171017.tif"]
	f20 <- ff[basename(ff) == "Images_v2.rar"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4a <- carobiner::read.excel(f4, sheet="A & gs_max")
	r4b <- carobiner::read.excel(f4, sheet="d13")
	r4c <- carobiner::read.excel(f4, sheet="cc")
	r4d <- carobiner::read.excel(f4, sheet="ndvi")
	r4e <- carobiner::read.excel(f4, sheet="ftw")
	r4f <- carobiner::read.excel(f4, sheet="dtw")
	r5a <- carobiner::read.excel(f5, sheet="A & gs_max")
	r5b <- carobiner::read.excel(f5, sheet="PRI")
	r5c <- carobiner::read.excel(f5, sheet="D13")
	r5d <- carobiner::read.excel(f5, sheet="CC")
	r5e <- carobiner::read.excel(f5, sheet="NDVI")
	r5f <- carobiner::read.excel(f5, sheet="YIELD")
	#r6 <- read.???(f6)
	#r7 <- read.???(f7)
	#r8 <- read.???(f8)
	#r9 <- read.???(f9)
	#r10 <- read.???(f10)
	#r11 <- read.???(f11)
	#r12 <- read.???(f12)
	#r13 <- read.???(f13)
	#r14 <- read.???(f14)
	#r15 <- read.???(f15)
	#r16 <- read.???(f16)
	#r17 <- read.???(f17)
	#r18 <- read.???(f18)
	#r19 <- read.???(f19)
	#r20 <- read.???(f20)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Abbreviation", "Variable name", "Unit", "Description", "Method", "Formula", "Reference"


	d2 <- data.frame()
##r2: "Abbreviation", "Variable name", "Unit", "Description", "Method", "Formula", "Reference"


	d3 <- data.frame(
		latitude = r3[["Lat"]]
	)
##r3: "Image", "DAP", "IF", "FA", "SR", "FW", "FH", "Lon", "TTC", "Band1", "Band2", "Band3"


	d4a <- data.frame(
		rep = r4a[["REP"]]
	)
##r4a: "DAP", "BLOCK", "INSTN", "CODE", "A", "gs_max"


	d4b <- data.frame()
##r4b: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "tm", "d13c", "d13"


	d4c <- data.frame()
##r4c: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "cc"


	d4d <- data.frame()
##r4d: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "NDVI", "sd"


	d4e <- data.frame()
##r4e: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "ftw"


	d4f <- data.frame()
##r4f: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "dtw"


	d5a <- data.frame()
##r5a: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "A", "gs_max"


	d5b <- data.frame()
##r5b: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "PRI"


	d5c <- data.frame()
##r5c: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "TM", "D13C", "D13"


	d5d <- data.frame()
##r5d: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "LA", "CC"


	d5e <- data.frame()
##r5e: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "NDVI", "SE"


	d5f <- data.frame()
##r5f: "DAP", "BLOCK", "INSTN", "CODE", "PLANT", "FTW", "DM", "DTW"


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

