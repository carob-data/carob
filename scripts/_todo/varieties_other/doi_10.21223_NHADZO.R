# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Data production of seed from cuttings 2019

Data production of seed from cuttings 2019.  This estudy was evaluated  in 9 districts of Uganda (Bugizardi, Kabale, Kanungu, Kapchorwa, Kazardi, Kisoro, Kween, Mbale and Rubanda ) in two varieties (Kachpot1  and Victoria).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/NHADZO"
	group <- "varieties_other"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Seed business; universe: Seed business; timeMethod: Cross-sectional; samplingProcedure: We evaluated 206 seed business  have been reached through CIP and partner interventions.
Number of users was:  26 female, 171 male and 9 research .",
		
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
	

	f1 <- ff[basename(ff) == "Data_production_of_seed_out_of_cuttings_2019_A1.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="RAC 2019A")
	r1b <- carobiner::read.excel(f1, sheet="Sheet1")
	r1c <- carobiner::read.excel(f1, sheet="Sheet2")
	r1d <- carobiner::read.excel(f1, sheet="Sheet3")
	r1e <- carobiner::read.excel(f1, sheet="Sheet4")
	r1f <- carobiner::read.excel(f1, sheet="Sheet8")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		season = r1a[["Season"]],
		adm1 = carobiner::fix_name(r1a[["Region"]], "title"),
		adm2 = carobiner::fix_name(r1a[["District"]], "title"),
		location = r1a[["Village"]],
		plot_id = r1a[["Plot No"]],
		variety = r1a[["Variety planted"]]
	)
##r1a: "Sub-county", "Name of the seed business", "Seed business type (individual, group, government)", "Gender (M, F, Research)", "Supplier of rooted apical cuttings", "Date of planting", "Off Type", "Bed No", "# rows in a bed", "# cuttings planted", "# cuttings harvested", "Survival rate", "Date of dehaulming", "Date of harvesting", "# tubers < 20mm", "Weight tubers < 20mm (Kg)", "# tubers 20-30mm", "Weight tubers 20-30mm (Kg)", "# tubers 31-45mm", "Weight tubers 31-45mm (Kg)", "# tubers 46-60mm", "Weight tubers 46-60mm (Kg)", "# tubers > 60mm", "Weight tubers > 60mm (Kg)", "Total # tubers < 20mm / bed", "Total # tubers ≥ 20mm / bed", "Total # tubers / bed", "Average # tubers < 20mm / plant", "Average # tubers ≥ 20mm / plant", "Average # tubers / plant", "Total weight tubers < 20mm / bed (Kg)", "Total weight tubers ≥ 20mm / bed (Kg)", "Total weight tubers / bed (Kg)", "Average weight tubers < 20mm / plant (gms)", "Average weight tubers ≥ 20mm / plant (gms)", "Average weight per tuber < 20mm (gms)", "Average weight per tuber ≥ 20mm (gms)", "Tons/ha", "% seed (weight basis / plant)", "% seed ( # tubers/plant)"


	d1b <- data.frame()
##r1b: "Row Labels", "Average of Survival rate", "Average of Average # tubers < 20mm / plant", "Average of Average # tubers ≥ 20mm / plant", "Average of Average # tubers / plant", "Average of Average weight per tuber < 20mm (gms)", "Average of Average weight per tuber ≥ 20mm (gms)"


	d1c <- data.frame()
##r1c: "Row Labels", "Average of Survival rate"


	d1d <- data.frame()
##r1d: "Row Labels", "Sum of # cuttings planted"


	d1e <- data.frame()
##r1e: "Row Labels", "Average of % seed ( # tubers/plant)"


	d1f <- data.frame()
##r1f: "Row Labels", "Average of Average # tubers < 20mm / plant", "Average of Average # tubers ≥ 20mm / plant", "Average of Average # tubers / plant", "Average of % seed ( # tubers/plant)"


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

