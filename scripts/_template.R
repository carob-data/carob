# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them

carob_script <- function(path) {

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset


"copy and paste the title and abstract from the repository"


## Identifiers
	uri <- "doi:10xxx/yyy"
	group <- "___"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
# change the major and minor versions if you see a warning
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "",
		# if there is a paper, include the paper's doi here
		# also add a RIS file in references folder (with matching doi)
		publication = "",
		project = NA,
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = "experiment",
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variable that describe management for all treatments or other observations that were not related to the aim of the trial (e.g. the presence of a disease).
		response_vars = "", 
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,
		# How many hours did it take to write this script?
		carob_effort = 0,
		carob_contributor = "Your Name",
		# The day the script was first written
		carob_date = "2025-06-08",
		notes = "", # notes for the end-user
		# if available report the experimental or survey design
		design = NA
	)
	
## read data 

	f <- ff[basename(ff) == "_____________"]
	r <- read.csv(f)
	# or  r <- carobiner::read.excel(f)

## select the variables of interest and assign them to the correct name
	d <- data.frame(
		country = r$Country,
		crop=tolower(r$crop), 

## make sure that names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
## first use "location", then use "site"
		location = r$Site,
		elevation = r$Alt,

		adm1 = carobiner::fix_name(r$Province, "title"),
		adm2 = carobiner::fix_name(r$District, "title"),

	## the treatment code/description (if any)	
		d$treatment <- r$Treatment
	)

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

