# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication Data for: Analyze Crop Water Stress Index (CWSI) values for an optimum tuber yield in potatoes under humid conditions.

It developed a field experiment, with the aim to analyze Crop Water Stress Index (CWSI) under humid conditions with testing two irrigation timing treatments with pre-established gs_max threshold (0.15 [T1] and 0.050 [T2] mol H2O m−2 s−1) against a control (frequently irrigated). The experiment was conducted in a 416 m2 total area, divided in 12 plots (3.6×5 m2 each) and sixty plants per plot were sown in five rows with a plant and furrow distance of 0.3 and 1 m, respectively. Seven furrow irrigations were provided at 0, 4, 7, 11, 17, 22 and 30 days after planting watering every plot with approximately 67 mm per irrigation; after that, a drip irrigation system was set up independently for each plot.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/PNJBH6"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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
	

	f1 <- ff[basename(ff) == "CWSI.xlsx"]
	f2 <- ff[basename(ff) == "Data_dictionary.xls"]
	f3 <- ff[basename(ff) == "Dry_biomass.xlsx"]
	f4 <- ff[basename(ff) == "Eco-physiological.xlsx"]
	f5 <- ff[basename(ff) == "Harvest.xlsx"]
	f6 <- ff[basename(ff) == "Irrigation.xlsx"]
	f7 <- ff[basename(ff) == "Tuber_isotopes.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "ID", "eval", "dap", "pot", "trat", "hour", "Tf", "Tw", "Ta", "dT", "CWSI"


	d2 <- data.frame()
##r2: "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference"


	d3 <- data.frame(
		plot_id = r3[["plot"]]
	)
##r3: "ID", "trat", "plant", "FW", "DW", "DBC"


	d4 <- data.frame(
		plot_id = r4[["plot"]]
	)
##r4: "ID", "eval", "dap", "trat", "gs_max", "LWP", "ldw", "dleaf", "Dleaf"


	d5 <- data.frame(
		plot_id = r5[["plot"]]
	)
##r5: "ID", "trat", "plant", "PF", "nT"


	d6 <- data.frame(
		plot_id = r6[["plot"]]
	)
##r6: "ID", "dds", "trat", "Hg", "Hv", "Vr", "Lr"


	d7 <- data.frame(
		plot_id = r7[["plot"]]
	)
##r7: "ID", "trat", "plant", "tdw", "dtuber", "Dtuber"


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

