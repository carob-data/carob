# R script for "carob"
# license: GPL (>=3)

## ISSUES
#Dataset or resource not reachable.
#Status code:  404 

carob_script <- function(path) {

""

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.7910/DVN1/X98KVY"
	group <- "reject"
	ff  <- carobiner::get_data(uri, path, group)

## Non-metadata .json files in ff (e.g. nested Dataverse Dataset/*.json). Parsed with jsonlite::fromJSON.
## Optional string snapshots: lapply(json_list, function(x) jsonlite::toJSON(x, pretty=TRUE, auto_unbox=TRUE))
	jmeta <- paste0(yuri::simpleURI(uri), ".json")
	json_paths <- ff[grepl("\\.json$", ff, ignore.case=TRUE)]
	json_paths <- json_paths[!tolower(basename(json_paths)) %in% tolower(c(jmeta, "metadata.json"))]
	json_list <- if (length(json_paths) > 0) {
		stats::setNames(lapply(json_paths, jsonlite::fromJSON), basename(json_paths))
	} else {
		list()
	}

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "",
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
		carob_date = "2026-06-25",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "Data on Integrated Production System 2019 & 2020-LSIL & Collaborating projects_ Niger.xlsx"]
	f2 <- ff[basename(ff) == "Data on Pearl millet production _2019&2020_LSIL zones_Niger.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Integrated Pro. System 2019")
	r1b <- carobiner::read.excel(f1, sheet="Sheet1")
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		hhid = r1a[["N of farmer"]],
		adm1 = carobiner::fix_name(r1a[["Region"]], "title"),
		adm2 = carobiner::fix_name(r1a[["District"]], "title"),
		location = r1a[["Village"]],
		treatment = r1a[["Treatment Groups"]],
		yield = r1a[["Pearl millet Biomass Yield (kg/ha)"]]
	)
##r1a: "Departement", "Field area (ha)", "Number of trees/ha", "Number of shrubs/ha", "Production System", "Pearl millet grain yield (kg/ha)"


	d1b <- data.frame()


	d2 <- data.frame(
		hhid = r2[["N of farmer"]],
		adm1 = carobiner::fix_name(r2[["Region"]], "title"),
		location = r2[["Village"]],
		treatment = r2[["Treatment Groups/ Production System"]],
		yield = r2[["Pearl millet stover yield/ha"]]
	)
##r2: "Weight of ears/ha", "Pearl millet grain yield/ha", "...8", "...9", "...10", "...11", "...12", "...13", "...14"


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

