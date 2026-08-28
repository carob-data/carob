# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


### REJECTED because data already processed in year by year files
### However, this one has variety names and these were matched in processing the original files
 


carob_script <- function(path) {

"
Replication Data: IRRI Long Term Continuous Cropping Experiment

This study contains grain yield data collected from IRRI's long term continuous cropping experiment (LTCCE)
"


	uri <- "doi:10.7910/DVN/HKX9SF"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "IRRI",
		publication = "",
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		notes = "",
		carob_contributor = "Your Name",
		carob_date = "2026-08-26",
		carob_completion = 0,	
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "LTCCE-HP_2068-2023.csv"]
	r1 <- read.csv(f1)
	d1 <- data.frame(
		location = r1[["Site"]],
		year = r1[["Year"]],
		season = r1[["Season"]],
		variety = r1[["B_label"]]
	)
##r1: "Expt", "Afactor", "A_label", "Bfactor", "Rep", "GYtha"


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
	meta$comment <- d$comm
	carobiner::write_files(path, meta, d$x)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

