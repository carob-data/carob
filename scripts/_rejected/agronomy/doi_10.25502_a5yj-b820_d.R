# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Datasets on yield components of fertilized improved and local varieties of Cassava grown in the highlands of South Kivu, DR Congo

The use of mineral fertilizer and organic inputs with an improved and local variety of cassava allow firstly to identify nutrient limitations to cassava production, and secondly to investigate the effects of variety and combined application of mineral and organic inputs on cassava growth and yields in the highland conditions of the Democratic Republic of Congo (DR Congo). Data on growth parameters, yields and yield components of the improved and local varieties of cassava, economic analysis and soil parameters, collected during two growing cycles of cassava are presented. The data support a research article which is under review “Increased cassava growth and yields through improved variety use and fertilizer application in the highlands of South Kivu, Democratic Republic of Congo” [1]. Data on plant height and diameter was measured throughout the growing period of the crop while the data on the storage root, stem, tradable storage root and non-tradable storage root was determined at 12 months after planting (MAP) of the field experiments. The economic analysis was performed using a simplified financial analysis where the additional benefits were calculated relative to the respective control treatments while the total costs included the purchasing prices of fertilizer and the additional net benefits, the revenue from the increased storage root yields due to fertilizer application. The value cost ratio (VCR) was calculated as the additional net benefits over the cost of fertilizer purchase.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.25502/a5yj-b820/d"
	group <- "reject"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "IITA",
		publication = NA,
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
		carob_date = "2026-10-05",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "varietyfertilizer_effect_data.csv"]
	f2 <- ff[basename(ff) == "nutrient-response_data.csv"]
	f3 <- ff[basename(ff) == "dataset_cassava-growth_data_dictionary.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		location = r1[["Site"]],
		season = r1[["Season"]],
		rep = r1[["Replicate"]],
		variety = r1[["Variety"]],
		yield = r1[["Total_yield_Root_stem"]]
	)
##r1: "ID", "Village", "Fertilizer", "Germination", "H3_4MAP", "H6MAP", "H8MAP", "H10MAP", "H12MAP", "D3_4MAP", "D6MAP", "D8MAP", "D10MAP", "D12MAP", "FW_StorageRoot", "FW_Stem", "Harvest_Index_HI", "Nr_tradRoot", "Nr_nontradRoot", "FW_TradRoot", "FW_nontradRoot", "X", "X.1", "X.2"


	d2 <- data.frame(
		location = r2[["Site"]],
		season = r2[["Season"]],
		rep = r2[["Replicate"]],
		variety = r2[["Variety"]],
		yield = r2[["Total_yield_Root_stem"]]
	)
##r2: "ID", "Village", "Fertilizer", "Germination", "H3_4MAP", "H6MAP", "H8MAP", "H10MAP", "H12MAP", "D3_4MAP", "D6MAP", "D8MAP", "D10MAP", "D12MAP", "FW_StorageRoot", "FW_Stem", "Harvest_Index_HI", "Nr_tradRoot", "Nr_nontradRoot", "FW_TradRoot", "FW_nontradRoot"


	d3 <- data.frame(
		country = r3[["coverage.country"]]
	)
##r3: "Tab", "Column", "description_abstract", "title", "data_type", "Measurement", "creator", "contributors", "source", "source.date", "source.file", "identifier", "subject", "subject.agrovoc", "format", "language", "relation", "coverage", "rights"


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

