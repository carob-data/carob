# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Preliminary trial for late blight assessment in HERLB population (First trial_Oxapampa)

'An open-field trial was conducted in Oxapampa, Peru, at an elevation of 1810 meters above sea level during the rainy season of 2022 to assess the Late Blight (LB) response in the HERLB population. This population originated from crosses between advanced clones, referred to as LBHT, and HER clones (4x hybrids obtained from pre-breeding). The evaluation encompassed 2996 genotypes, organized in a row-column statistical design with four plants per plot and no replication. The varieties included in the trial had a known response to late infection, with Kory, Amarilis, and Yungay representing high resistance, moderate resistance, and susceptibility to LB, respectively. At 40 days after planting (DAP), a meticulous recording of the percentage of leaf area affected by late blight infection per plot was undertaken throughout the season. Subsequently, the area under the disease progress curve (AUDPC) was calculated, offering a comprehensive measure of the impact of late blight infection.'
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/N00MNU"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
	

	f1 <- ff[basename(ff) == "01_Fieldbook_HERLB_PER_2022_LateBlight_Oxa.xlsx"]
	f2 <- ff[basename(ff) == "02_Material_List_HERLB_PER_2022_LateBlight_Oxa.xlsx"]
	f3 <- ff[basename(ff) == "03_RRC_G2_serp-HERLB_PER_2022_LateBlight_Oxa.xlsx"]
	f4 <- ff[basename(ff) == "04_Weather_data-HERLB_PER_2022_LateBlight_Oxa.xlsx"]
	f5 <- ff[basename(ff) == "05_Soil_Data-HERLB_PER_2022_LateBlight_Oxa.xlsx"]
	f6 <- ff[basename(ff) == "06_Layout-HERLB_PER_2022_LateBlight_Oxa.xlsx"]
	f7 <- ff[basename(ff) == "07_Data_dictionary-HERLB_PER_2022_LateBlight_Oxa.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		plot_id = r1[["plot"]]
	)
##r1: "row", "column", "rowgroup", "colgroup", "crgroup", "cip_number", "family", "type", "NTP", "NPE", "NPH", "LB1", "LB2", "LB3", "LB4", "LB5", "LB6", "LB7", "LB8", "AUDPC", "rAUDPC", "NMTP", "NNoMTP", "MTWP", "NoMTWP", "NMTPL", "ATMW", "TNTP", "TNTPL", "TTWP", "TTWPL"


	d2 <- data.frame()
##r2: "ord", "CIPNumber", "Accesion_Name", "Is_control", "Family_AcceNumb", "Female_AcceNumb", "Female_codename", "Male_AcceNumb", "Population", "Seed_source", "Previous_trials"


	d3 <- data.frame(
		treatment = r3[["treatment"]],
		rep = r3[["Rep"]]
	)
##r3: "row", "column", "rowgroup", "colgroup", "RC.group", "Item", "CIP_Number", "Family_Number", "Type"


	d4 <- data.frame()
##r4: "Date", "Time", "GMT", "TEMP", "RH"


	d5 <- data.frame()
##r5: "Variables", "Abbreviture", "Unit", "Data"


	d6 <- data.frame()


	d7 <- data.frame(
		crop = tolower(r7[["Crop_Ontology"]])
	)
##r7: "Ord", "Variable name", "Abbreviations"


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

