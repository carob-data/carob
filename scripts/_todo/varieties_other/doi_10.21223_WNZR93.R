# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication data for: Potential Short-term Memory Induction as a Promising Method for Increasing Drought Tolerance in Sweetpotato Crop Wild Relatives [Ipomoea series Batatas (Choisy) D. F. Austin]

Potted experiment to develop water stress resilience and productivity in 59 accessions of sweetpotato crop wild relatives (eight plants per accession) through short-term memory induction (four plants in potencial conditions and four underwent a water stress priming process as non-primed and primed treatment, respectively) using ecophysiological traits (Chl_SPAD, foliar area, leaf-minus-air temperature and leaf isotopic discrimination). Experiment was carried out durind July-December 2018 in a completely randomized desing at San Ramón CIP experimental station (Junin-Peru).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/WNZR93"
	group <- "varieties_other"
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
	

	f1 <- ff[basename(ff) == "Data_CWR_Sweetpotato2018.xlsx"]
	f2 <- ff[basename(ff) == "Data_dictionary_CWR_Sweetpotato.xlsx"]
	f3 <- ff[basename(ff) == "Material_list_Sweetpotato2018.xlsx"]
	f4 <- ff[basename(ff) == "Weather_data_CRW_Sweepotato2018.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Foliar area")
	r1b <- carobiner::read.excel(f1, sheet="Gravimetric hum")
	r1c <- carobiner::read.excel(f1, sheet="Leaf temp")
	r1d <- carobiner::read.excel(f1, sheet="Stomatal cond")
	r1e <- carobiner::read.excel(f1, sheet="Spad")
	r1f <- carobiner::read.excel(f1, sheet="Isotopes")
	r1g <- carobiner::read.excel(f1, sheet="Harvest")
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		treatment = r1a[["treatment"]]
	)
##r1a: "ID", "dat", "Accession number", "fs", "tm", "FA", "image name", "...9"


	d1b <- data.frame(
		treatment = r1b[["treatment"]]
	)
##r1b: "ID", "dat", "Accession number", "indiv", "fw", "dw"


	d1c <- data.frame()
##r1c: "ID", "dat", "Accession number", "indiv", "trat", "tm", "fs", "greenhouse", "time", "Tl", "Ta", "dT"


	d1d <- data.frame(
		treatment = r1d[["treatment"]]
	)
##r1d: "ID", "dat", "Accession number", "indiv", "fs", "tm", "gs_max", "...9"


	d1e <- data.frame(
		treatment = r1e[["treatment"]]
	)
##r1e: "ID", "dat", "Accession number", "indiv", "fs", "tm", "Chl_SPAD"


	d1f <- data.frame(
		treatment = r1f[["treatment"]]
	)
##r1f: "ID", "dat", "Accessionnumber", "fs", "tm", "dw", "Δ"


	d1g <- data.frame(
		treatment = r1g[["Treatment"]]
	)
##r1g: "ID", "dat", "Accession number", "Individual", "fs", "tm", "fw", "dw"


	d2 <- data.frame()
##r2: "Num", "File", "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d3 <- data.frame(
		crop = tolower(r3[["Crop"]])
	)
##r3: "Accession number", "Collecting number/Breeder code/Biosafety code", "Accession name", "Population Group", "Parent Female", "Parent Male"


	d4 <- data.frame(
		P_fertilizer = r4[["P"]]
	)
##r4: "ID", "dat", "time", "Ta", "RH", "VPD", "ws", "Rs"


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

