# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: The photosynthesis recovery, a key trait as indicator of an appropriate irrigation criteria in potato crop

Field experiment to compare physiological performance and crop yield among different water stress levels before and after rehidratation periods in UNICA potato variety, to test the reliability of photosynthesis recovery as key trait for indicate an appropriate irrigation criteria, under two irrigation types.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/Q4KNAN"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
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
	

	f1 <- ff[basename(ff) == "CANOPY COVER-EX17-I.xlsx"]
	f2 <- ff[basename(ff) == "Data_Dictionary.xlsx"]
	f3 <- ff[basename(ff) == "Documentation_v2.xlsx"]
	f4 <- ff[basename(ff) == "ECOPHYSIOLOGY-EX17-I.xlsx"]
	f5 <- ff[basename(ff) == "HARVEST-EX17-I.xlsx"]
	f6 <- ff[basename(ff) == "HARVEST ISOTOPES-EX17-I.xlsx"]
	f7 <- ff[basename(ff) == "IRRIGATION VOLUME-EX17-I.xlsx"]
	f8 <- ff[basename(ff) == "Photosynthesis recovery-2017_v2.xlsx"]
	f9 <- ff[basename(ff) == "PRI EX17-I.xlsx"]
	f10 <- ff[basename(ff) == "RECOVERY ISOTOPES-EX17-I.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3a <- carobiner::read.excel(f3, sheet="Ecophysiology")
	r3b <- carobiner::read.excel(f3, sheet="Canopy cover")
	r3c <- carobiner::read.excel(f3, sheet="Isotopes")
	r3d <- carobiner::read.excel(f3, sheet="Recovery isotopes")
	r3e <- carobiner::read.excel(f3, sheet="PRI")
	r3f <- carobiner::read.excel(f3, sheet="Irrigation Vol")
	r3g <- carobiner::read.excel(f3, sheet="Harvest")
	r3h <- carobiner::read.excel(f3, sheet="wheather")
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)
	r8a <- carobiner::read.excel(f8, sheet="Ecophysiology")
	r8b <- carobiner::read.excel(f8, sheet="Canopy cover")
	r8c <- carobiner::read.excel(f8, sheet="Isotopes")
	r8d <- carobiner::read.excel(f8, sheet="Revovery isotopes")
	r8e <- carobiner::read.excel(f8, sheet="PRI")
	r8f <- carobiner::read.excel(f8, sheet="Irrigation Vol")
	r8g <- carobiner::read.excel(f8, sheet="Harvest")
	r8h <- carobiner::read.excel(f8, sheet="Weather")
	r9 <- carobiner::read.excel(f9)
	r10 <- carobiner::read.excel(f10)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		treatment = r1[["treatment"]],
		plot_id = r1[["plot"]]
	)
##r1: "ID", "DAP", "irrigation", "CC"


	d2 <- data.frame()
##r2: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d3a <- data.frame()
##r3a: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d3b <- data.frame()
##r3b: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d3c <- data.frame()
##r3c: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d3d <- data.frame()
##r3d: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d3e <- data.frame()
##r3e: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d3f <- data.frame()
##r3f: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d3g <- data.frame()
##r3g: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d3h <- data.frame()
##r3h: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d4 <- data.frame(
		treatment = r4[["treatment"]],
		plot_id = r4[["plot"]]
	)
##r4: "ID", "date", "dap", "irrigation", "plant", "A", "gs"


	d5 <- data.frame(
		treatment = r5[["treatment"]],
		plot_id = r5[["plot"]]
	)
##r5: "ID", "harvest", "dap", "irrigation", "plant", "fw-leaf", "fw-stem", "fw- root", "fw-tuber", "at", "dw-leaf", "dw-stem", "dw-root", "dw-tuber"


	d6 <- data.frame(
		treatment = r6[["treatment"]],
		plot_id = r6[["plot"]]
	)
##r6: "ID", "harvest", "dap", "irrigation", "tm", "dw", "D13"


	d7 <- data.frame(
		treatment = r7[["treatment"]],
		plot_id = r7[["plot"]]
	)
##r7: "ID", "date", "dap", "irrigation", "IV"


	d8a <- data.frame(
		treatment = r8a[["treatment"]],
		plot_id = r8a[["plot"]]
	)
##r8a: "id", "date", "dap", "irrigation", "plant", "Pn", "gs_max", "Fv'/Fm'"


	d8b <- data.frame(
		treatment = r8b[["treatment"]],
		plot_id = r8b[["plot"]]
	)
##r8b: "ID", "DAP", "irrigation", "CC"


	d8c <- data.frame(
		treatment = r8c[["treatment"]],
		plot_id = r8c[["plot"]]
	)
##r8c: "ID", "harvest", "dap", "irrigation", "tm", "dw", "D13"


	d8d <- data.frame(
		treatment = r8d[["treatment"]],
		plot_id = r8d[["plot"]]
	)
##r8d: "ID", "dap", "ms", "irrigation", "tm", "dw", "D13"


	d8e <- data.frame(
		treatment = r8e[["treatment"]],
		plot_id = r8e[["plot"]]
	)
##r8e: "ID", "dap", "irrigation", "plant", "PRI"


	d8f <- data.frame(
		treatment = r8f[["treatment"]],
		plot_id = r8f[["plot"]]
	)
##r8f: "ID", "date", "dap", "irrigation", "IV"


	d8g <- data.frame(
		treatment = r8g[["treatment"]],
		plot_id = r8g[["plot"]]
	)
##r8g: "ID", "harvest", "dap", "irrigation", "plant", "fw-leaf", "fw-stem", "fw- root", "fw-tuber", "at", "dw-leaf", "dw-stem", "dw-root", "dw-tuber"


	d8h <- data.frame(
		year = r8h[["Year"]]
	)
##r8h: "ID", "Month", "Day", "Hour", "Ta", "RH", "VPD", "WS", "Dir", "SR", "PAR", "PP", "Pa", "Eo"


	d9 <- data.frame(
		treatment = r9[["treatment"]],
		plot_id = r9[["plot"]]
	)
##r9: "ID", "dap", "irrigation", "plant", "PRI"


	d10 <- data.frame(
		treatment = r10[["treatment"]],
		plot_id = r10[["plot"]]
	)
##r10: "ID", "dap", "ms", "irrigation", "tm", "dw", "D13"


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

