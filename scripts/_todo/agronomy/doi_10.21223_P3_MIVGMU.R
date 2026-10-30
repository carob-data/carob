# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication data for: Effect of partial root-zone drying irrigation timing on potato tuber yield and water use efficiency

Partial root-zone drying (PRD) is an irrigation technique which has shown increased water use efficiency (WUE) without yield reductions in potato and other crops. However, questions remain as to the effect of the water restriction initiation timing and the level of alternate water restriction on the response of the potato crop. In this study, we tested: two PRD treatments with 25% (PRD25) and 50% (PRD50) of total water used in full irrigation (FI, as control), and a deficit irrigation treatment with 50% of water restriction (DI50). Two water restriction initiation timings were tested at: 6 weeks (WRIT6w) and 8 weeks (WRIT8w) after planting. Osmotic potential, osmotic adjustment, relative water content and chlorophyll concentration were assessed in four dates during the growing period. PRD50 initiated at WRIT6w showed the highest WUE without a tuber yield reduction respect to the control. While plants under PRDs and DI50 showed lower   than FI, PRDs treatments promoted higher osmotic adjustment particularly in WRIT6w. Our study suggests that early PRDs with mild water restriction allow drought hardiness (improving water stress response) and water saving avoiding a dramatic yield tuber reduction.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/MIVGMU"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
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
	

	f1 <- ff[basename(ff) == "Datos Yactayo et al_2013 (PRD).xlsx"]
	f2 <- ff[basename(ff) == "Dictionary_OA_Yactayo.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Read me")
	r1b <- carobiner::read.excel(f1, sheet="RWC")
	r1c <- carobiner::read.excel(f1, sheet="Chl SPAD")
	r1d <- carobiner::read.excel(f1, sheet="OP")
	r1e <- carobiner::read.excel(f1, sheet="TY")
	r1f <- carobiner::read.excel(f1, sheet="WUE")
	r1g <- carobiner::read.excel(f1, sheet="CWD")
	r1h <- carobiner::read.excel(f1, sheet="Weather data")
	r2a <- carobiner::read.excel(f2, sheet="READ ME")
	r2b <- carobiner::read.excel(f2, sheet=" Yactayo et al PRD")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame()
##r1a: "T", "Timing"


	d1b <- data.frame()
##r1b: "...1", "...2", "...3", "RWC (%)", "...5", "...6", "...7"


	d1c <- data.frame()
##r1c: "...1", "...2", "...3", "Chl SPAD", "...5", "...6", "...7"


	d1d <- data.frame()
##r1d: "...1", "...2", "...3", "OP (MPa)", "...5", "...6", "...7"


	d1e <- data.frame(
		B_fertilizer = r1e[["B"]]
	)
##r1e: "T", "TRT", "TY (t ha-1)"


	d1f <- data.frame()
##r1f: "Lamina Aprovechable Total", "...2", "...3", "...4"


	d1g <- data.frame(
		B_fertilizer = r1g[["B"]]
	)
##r1g: "T", "TRT", "CWD (m3 ha−1)"


	d1h <- data.frame()
##r1h: "Date", "T max(°C)", "T min (°C)", "GRad (MJ*m2)", "PP (mm)", "RH (%)"


	d2a <- data.frame()
##r2a: "Term type", "Column", "Description"


	d2b <- data.frame()
##r2b: "Variable synonyms", "Trait", "Trait class", "Trait description", "Method class", "Method description", "Formula", "Method reference", "Scale name/unit"


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

