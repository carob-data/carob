# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Remote sensing and agronomic data for drought trials evaluated at Umbeluzi Research Station in 2019 (2nd experiment).

The trial had 120 clones,  the majority begin parents used in crossing blocks at Umbeluzi and Gurue Research Stations. The check clones such as Resisto and 199062.1 are parents in crossing blocks as well and some checks were landraces such as Xitsekele, Xiadla xakau. Trial life span started on 28 March 2019 and ended on 18 October 2019. The experiment was laid in two treatments defined as follows;   (i) Optimum Irrigation (OI, as control): No water restriction was imposed.  (ii) Late-season drought (LSD): Irrigation was withheld at 110 DAP and no irrigation was applied thereafter. Plot size was 25m2. The plots were arranged as 8 rows × 15 columns per treatment and replicated twice in each treatment. The plots were fertilized with urea, 46N:0P:0K at three weeks after planting and irrigated twice a week. All the field plots were kept weed-free by hoe weeding. A Cypermethrin (Avansagro, Shangai, China) insecticide was applied at 20 and 35 days after planting (DAP) in all trials with doses of 10mL 100mL-1 water.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/2GJMNA"
	group <- "varieties_other"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; International Potato Center - CIP-SSA",
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
	

	f1 <- ff[basename(ff) == "Data_Dictionary_Moz2.xlsx"]
	f2 <- ff[basename(ff) == "Image_description_Moz2.xlsx"]
	f3 <- ff[basename(ff) == "Umbeluzi_2019_Moz2.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Abbreviation", "Variable name", "Description", "Method", "Formula", "Unit", "Reference"


	d2 <- data.frame(
		latitude = r2[["Lat"]]
	)
##r2: "Image", "DAP", "IF", "FA", "SR", "FW", "FH", "Lon", "Adquisition System"


	d3 <- data.frame()
##r3: "...1", "...2", "...3", "...4", "...5", "...6", "Canopy Temperature (°C)", "...8", "NDVI", "...10", "NDVIgreen", "...12", "CI", "...14", "Coberture"


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

