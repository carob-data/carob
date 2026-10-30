# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Remote sensing and agronomic data for drought trials evaluated at Umbeluzi Research Station in 2019 (1st experiment).

The trial had 24 clones. Check clones were 199062.1, Nhacoongo 1, Caromex, Xiadlaxakau, TIS2534, Irene and Xitsekele. The remaining 17 are parents from Gurue and Umbeluzi. The trials were planted at Umbeluzi from 20 to 23 December 2018. Each plot was 25m2. In this trial, there were 5 treatments namely, (i) optimum irrigation & fertilizer (ii) optimum irrigation (iii) early season drought (iv) mid-season drought & (v) late season drought. The treatments are defined as follows;  (i)	Irrigation + fertilizer – no water stress and fertilizer (NPK) applied at 35 days after planting in the treatment. (irrigation to water holding capacity of the soil)  (ii)	Irrigation – no water stress in the treatment (irrigation to water holding capacity of the soil)  (iii)	Early season drought – water withheld at 40 days after planting for 15 days. Drought intensity targetted is moderate to severe (40 to 70 % stress). Irrigation resumed at 50 to 56 days after planting.    (iv)	Mid-season drought – water withheld at 65 days after planting for 15 days. Drought intensity targetted is moderate to severe (40 to 70 % stress). Irrigation resumed at 80 to 86 days after planting.  (v)	Terminal drought - water withheld at 120 days after planting. No irrigation applied thereafter. Drought intensity targetted is moderate to severe (40 to 70 % stress). 

Experimental design: row column design (6 rows x 8 columns per each treatment), 2 replications per treatment. Soil analysis done for each field. The trial was mainly supported by irrigation.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/WJWMZB"
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
	

	f1 <- ff[basename(ff) == "Data_Dictionary_Moz1.xlsx"]
	f2 <- ff[basename(ff) == "Image_description_Moz1.xlsx"]
	f3 <- ff[basename(ff) == "Umbeluzi_2019_Moz1.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3a <- carobiner::read.excel(f3, sheet="190319")
	r3b <- carobiner::read.excel(f3, sheet="150619")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Abbreviation", "Variable name", "Description", "Method", "Formula", "Unit", "Reference"


	d2 <- data.frame(
		latitude = r2[["Lat"]]
	)
##r2: "Image", "DAP", "IF", "FA", "SR", "FW", "FH", "Lon", "Adquisition System"


	d3a <- data.frame()
##r3a: "...1", "...2", "...3", "Canopy Temperature (°C)", "...5", "NDVI", "...7", "NDVIGreen", "...9", "CI", "...11", "Coberture"


	d3b <- data.frame()
##r3b: "...1", "...2", "...3", "Canopy Temperature (°C)", "...5", "NDVI", "...7", "NDVIgreen", "...9", "CI", "...11", "Coberture"


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

