# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Sweet potato vine transactions in Northern Uganda collected in 2013 and 2014

The goal of this study was to assess sweet potato seed (vine) transactions and associated actors in the Gulu region of northern Uganda. Sellers and multipliers were surveyed for all vine sales that occurred during the growing season (April-August) in 2013 and 2014. Transactions include seller location, seller type, buyer location, buyer type, point of sale, variety, volume, and cost. This work will help to better model and understand key actors in this system, with methods developed being transferable to other seed systems.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/6OEHTR"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=6,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "Natural Resources Institute - University of Greenwich; Department of Rural Development and Agribusiness - Gulu University",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Vine transactions.; universe: Northern Uganda.; dataCollector: Gulu University.; samplingProcedure: Sellers and multipliers in the Gulu region of northern Uganda were surveyed weekly throughout the growing season to capture vine sales transactions and variety information for the previous week. Actors were identified through local knowledge and initial surveys.; collectionMode: In person interviews and questionnaires.",
		
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
	

	f1 <- ff[basename(ff) == "Data_dictionary.xlsx"]
	f2 <- ff[basename(ff) == "Transaction Data_2013_2014.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="2014 All Data")
	r2b <- carobiner::read.excel(f2, sheet="2013 All Data")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference"


	d2a <- data.frame(
		location = r2a[["Seller_Village"]],
		country = r2a[["Customer_SubCountry"]],
		variety = r2a[["Variety"]]
	)
##r2a: "S/No.", "Seller_ID", "Seller_Age", "Seller_Parish", "Seller_SubCounty", "Sale_Date", "Customer_Village", "BundlesSold", "No.SmallBundles", "Selling Price /Bundle(big Size)", "Selling_Point", "Buyer-Type", "Distance Travelled by Buyer"


	d2b <- data.frame(
		location = r2b[["Seller_Village"]],
		variety = r2b[["Variety"]]
	)
##r2b: "S/No.", "Seller_Name", "Seller_ID", "Seller_Age", "Seller_Parish", "Seller_SubCounty", "Sale_Date", "Customer_Village", "Customer_SubCounty", "Customer_Type", "BundlesSold", "No.SmallBundles", "Selling Price /Bundle(big Size)", "Distance traveled by Buyer"


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

