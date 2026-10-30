# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Potato seed degeneration in Ecuador

Vegetative multiplication of potato in the field can lead to an accumulation of pest and pathogens such as fungi, bacteria and mainly virus in the seed over successive cycles of reproduction. This process is known as seed degeneration, where the main vectors for virus infection and transmission are aphids. This study was carried out to evaluate the rate of seed degeneration in two potato cultivars, at different altitudes and considering the use of different on-farm seed management options in the Ecuadorian Andes with the aim to support the promotion of an integrated seed health strategy. The study involved the cultivars INIAP-Fripapa and Superchola (susceptible and resistant to degeneration, respectively, according to farmers' perceptions). The field trials were carried out at three altitudes ( 3400 masl) during 3 cropping cycles of successive vegetative reproduction. Twelve plots of 49 m2 were planted each year; 2 plots of each cultivar at each altitude/site, in which 3 types of seed management were carried out: positive selection, roguing and random selection. We evaluated: (1) virus incidence (PVX, PVY, PVS, PLRV, APLV and APMoV) in plants at emergence, flowering and in tubers using the DAS-ELISA; (2) abundance of aphids; (3) incidence and severity of pest damage and diseases in tubers; and (4) tuber yield. Similar data were taken from 10 farmers' fields, and separate trials were done to estimate the efficiency of auto-infection (percentage of infected progeny tubers from an infected mother tuber) of PVY at the three altitudes.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/3CT90C"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
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
	

	f1 <- ff[basename(ff) == "Aphids_Data_Potato_seed_degeneration_in_Ecuador.xls"]
	f2 <- ff[basename(ff) == "Dictionary_Potato_seed_degeneration_in_Ecuador.xls"]
	f3 <- ff[basename(ff) == "ELISA_Data_Potato_seed_degeneration_in_Ecuador.xls"]
	f4 <- ff[basename(ff) == "Harvest_Data_Potato_seed_degeneration_in_Ecuador.xls"]
	f5 <- ff[basename(ff) == "Locations_Data_Potato_seed_degeneration_in_Ecuador.xls"]
	f6 <- ff[basename(ff) == "Weather_Data_Potato_seed_degeneration_in_Ecuador_v2.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		elevation = r1[["Altitude"]],
		location = r1[["Location"]],
		variety = r1[["Variety"]],
		planting_date = r1[["Planting date"]]
	)
##r1: "Cycle", "Control", "Management", "TID", "DS", "DAP", "DBS", "Aphids_numb"


	d2 <- data.frame()
##r2: "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference"


	d3 <- data.frame(
		elevation = r3[["Altitude"]],
		location = r3[["Location"]],
		variety = r3[["Variety"]]
	)
##r3: "Cycle", "Control", "Stage", "DAP", "Management", "Platenum", "well", "sample", "PPVX_Inci_0to1", "PPVY_Inci_0to1", "PPVS_Inci_0to1", "PPLRV_Inci_0to1", "AAPLV_Inci_0to1", "AAPMoV_Inci_0to1"


	d4 <- data.frame(
		elevation = r4[["Altitude"]],
		location = r4[["Location"]],
		variety = r4[["Variety"]]
	)
##r4: "Cycle", "Control", "Management", "Plantnum", "TubN_Ct_plant", "TubWg_Cp_plant", "BScurf_Inci_0t01", "WWorm_Inci_0to1", "PScab_Inci_0to1", "CScab_Inci_0to1", "Malform_Inci_0to1", "PCN_Inci_0to1", "BLeg_Inci_0to1", "PhysDam_Inci_0to1", "Cracking_Inci_0to1", "Scracking_Inci_0to1", "DamOtherInsects_Inci_0to1", "MechDamage_Inci_0to1", "DryRut_Inci_0to1", "Smut_Inci_0to1"


	d5 <- data.frame(
		elevation = r5[["Altitude"]],
		location = r5[["Location"]],
		variety = r5[["Variety"]],
		latitude = r5[["Latitude"]],
		longitude = r5[["Longitude"]],
		planting_date = r5[["Planting date"]],
		harvest_date = r5[["Harvest date"]]
	)
##r5: "Cycle", "Control"


	d6 <- data.frame(
		elevation = r6[["Altitude"]],
		location = r6[["Location"]]
	)
##r6: "Cycle", "Control", "Date", "Time", "Temp", "HR", "Rain"


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

