# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Decentralised seed multiplier gross margin data

This study was conducted during the years of 2016, 2017 and 2018 in the countries of  Nandi, Uasin Gishu, Elgeyo Marakwet and Meru.   Here economic aspects of potato production were evaluated as the value of production by yield.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/ZTO9TF"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Farmer; universe: This study focused on farmers 217; timeMethod: Cross-Sectional; samplingProcedure: The data collected included demographic information and opinions about the economic aspects.",
		
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
	

	f1 <- ff[basename(ff) == "16940_data_dictionary.xlsx"]
	f2 <- ff[basename(ff) == "16940_Decentralised_seed_multiplier_gross_margin_data.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "variable_name", "Description", "Type", "Unit", "Value_minimum", "Value_maximum", "categoric"


	d2 <- data.frame(
		season = r2[["season"]],
		country = r2[["country"]],
		yield = r2[["total_yield_t_ha"]],
		adm3 = carobiner::fix_name(r2[["ward"]], "title")
	)
##r2: "ID_name", "gender", "total_seed_planted_kg", "up_ha_units_of_production", "tp_kg_total_production", "ware_harvested_kg", "ware_sold_kg", "ware_selling_price_kg", "vos_ware_kes", "vos_ware_usd", "seed_harvested_kg", "seed_sold_kg", "seed_selling_price_kg", "vos_seed_kes", "vos_seed_usd", "seed_retained_kg", "up_seed_retained_ha", "qs_kg_total_quantity_of_sales", "vs_ksh_total_value_of_sales", "vs_usd_total_value_of_sales", "ic_usd_total_recurrent_cash_input_costs", "gm_usd", "gm_seed", "ic_ksh_total_recurrent_cash_input_costs", "percent_production_that_is_seed", "gross_margin_per_ha_ksh"


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

