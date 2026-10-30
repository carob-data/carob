# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication data for: B-Carotene, Iron and Zinc Content, Effect of Storage and Type of Cooking in Sweet Potato (Ipomoea batatas L.) Genotypes

Sweet potato (Ipomoea batatas L) is the main component in the diet of millions of people living in tropical countries, and an improvement in β-carotene content could increase the supply of vitamin A to meet a greater part of the daily requirements of people. In the present study, the content of β-carotene (BC), iron (Fe) and zinc (Zn) were determined in 25 genotypes of sweet potato from two localities: La Molina and San Ramón. Furthermore, the changes in BC content in seven sweet potato genotypes were investigated using different cooking methods and storage times. The content of BC was measured via absorbance at 450 nm and identified through HPLC while Fe and Zn content were determined through ICP-OES techniques. The results show significant variation between genotype, environment and in the genotype-environment interaction. BC content in freshly harvested raw root ranged from 7.62 to 18.93 mg /100 g in fresh weight (PF), iron content ranged from 0.40 to 0.9 6mg/100 g PF and the zinc content ranged from 0.25 to 0.51 mg/100 mg PF. Genotypes 440442 and 440518 showed the highest levels of BC in both localities. Differences in BC content were found among cooking methods and storage times. BC concentration in boiled roots (15.15 mg/100 g PF) evaluated was higher than in baked roots (14.29 mg/100 g PF); however, without differences between genotypes 440442 and 420081 due to the cooking methods. BC content of root increased with storage time, where genotype 440442 showed the highest levels of BC during storage time, while genotypes 440413 and 440513 did not differ during storage time.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/TMFHKL"
	group <- "varieties_other"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; National Agrarian University",
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
	

	f1 <- ff[basename(ff) == "Data_dictionary.xls"]
	f2 <- ff[basename(ff) == "Data_storage_cooking_sweetpotato.xls"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="Material_List_genotypes")
	r2b <- carobiner::read.excel(f2, sheet="Genotypes (SR-LM)")
	r2c <- carobiner::read.excel(f2, sheet="Cooking")
	r2d <- carobiner::read.excel(f2, sheet="Storage")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		crop = tolower(r1[["Crop"]])
	)
##r1: "Num", "Variable ID", "Variable label", "Variable name", "Variable synonyms", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref", "Category 1", "Category 2", "Category 3", "Category 4"


	d2a <- data.frame()
##r2a: "Numeration", "Accession_Number", "Accession_Name", "Accession_Code", "Family_AcceNumb", "Female_AcceNumb", "Male_AcceNumb"


	d2b <- data.frame(
		rep = r2b[["REP"]]
	)
##r2b: "ENVIRONMENT", "INSTN", "BCFW", "FeFW", "ZnFW", "DM"


	d2c <- data.frame(
		treatment = r2c[["TREATMENT"]],
		rep = r2c[["REP"]]
	)
##r2c: "INSTN", "TYPE", "BCFW", "DM", "Pro-vitamin A (ug RAE/100g FW)"


	d2d <- data.frame(
		treatment = r2d[["TREATMENT"]],
		rep = r2d[["REP"]]
	)
##r2d: "INSTN", "BCFW", "DM"


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

