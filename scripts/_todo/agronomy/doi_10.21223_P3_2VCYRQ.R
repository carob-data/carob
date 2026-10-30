# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Analyze the use of thresholds based on crop water stress index calculus in potato  'Unica' under drip irrigation

The objective of this study was to evaluate the UNICA potato crop variety's yield relate with the threshold of Crop Water Stress Index subjected to different water stress levels and under drip of irrigation. The trial was carried out in the experimental field of National Agrarian University La Molina during the months of June-2018 to September-2018. Three treatments of irrigation moments based on the Crop Water Stress Index (CWSI) were applied for this trial: Full irrigated (T1), CWSI less than 0.4 (T2), CWSI less than 0.7 (T3).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/2VCYRQ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
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
	

	f1 <- ff[basename(ff) == "Data.xls"]
	f2 <- ff[basename(ff) == "Dictionary.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Biomass")
	r1b <- carobiner::read.excel(f1, sheet="Canopy cover")
	r1c <- carobiner::read.excel(f1, sheet="CWSI")
	r1d <- carobiner::read.excel(f1, sheet="Irrigation")
	r1e <- carobiner::read.excel(f1, sheet="Soil humidity")
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		plot_id = r1a[["plot"]],
		treatment = r1a[["treatment"]]
	)
##r1a: "ID", "harvest", "dap", "irrigation", "block", "FL", "FS", "FR", "FT", "nT", "DL", "DS", "DR", "DT"


	d1b <- data.frame(
		plot_id = r1b[["plot"]],
		treatment = r1b[["treatment"]]
	)
##r1b: "ID", "dap", "irrigation", "block", "CC"


	d1c <- data.frame(
		plot_id = r1c[["plot"]],
		treatment = r1c[["treatment"]]
	)
##r1c: "ID", "dap", "irrigation", "block", "hour", "Tf", "Tw", "Ta", "RH", "es", "VPD", "PAR", "SR", "ws", "wd", "dT", "CWSI"


	d1d <- data.frame(
		plot_id = r1d[["plot"]],
		treatment = r1d[["treatment"]]
	)
##r1d: "ID", "dap", "daTI", "irrigation", "block", "nIrri", "V"


	d1e <- data.frame(
		plot_id = r1e[["plot"]],
		treatment = r1e[["treatment"]]
	)
##r1e: "ID", "dap", "irrigation", "block", "Hv"


	d2 <- data.frame()
##r2: "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Other"


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

