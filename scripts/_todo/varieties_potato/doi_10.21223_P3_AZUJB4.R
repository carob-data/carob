# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Standard evaluation trial for Vitamin C content documentation of CIP advanced breeding clones

Iron (Fe), zinc (Zn) and vitamins A and C are among the most widespread nutrient deficiencies in developing countries. Potato genotypes display from moderate to large ranges of mineral content. A survey of advanced breeding clones and cultivars showed broad range of iron contents extending from 18 to 65 μg/g DW, while less overall variation of Zinc contents was true, ranging from 12.5 to 20 μg/g DW.  Some evidence of consistency of genotype effects across environments suggest that significant gains for iron and zinc can be expected through breeding, particularly for iron if coupled with a high content of vitamin C considering the close association between iron availability and vitamin C. Concentration of vitamin C in potatoes is moderate to low compared with fruits and other vegetables, yet as a staple food, potatoes may be one of the highest contributors of vitamin C in the diets of people in many places around the world. Inheritance study using low and high concentration parents suggested that vitamin C concentration in tubers is highly heritable and hence increased levels could rapidly be achieved through selection of high concentration parents  Standard evaluation trials assessed iron, zinc and vitamin C contents in tuber samples from advanced breeding clones to document their nutritional values. Healthy and representative tubers with regard to tuber size of each clone  are picked up  from plots of two replications of 10 hill plots from trials conducted in pathogen and pest protected fields and grown following local procedures. Iron and Zinc content are determined by inductively coupled plasma–optical emission spectrophotometry (ICP) and concentrations expressed in mg/kg, dry weight (DW), while vitamin C is quantified by the spectrophotometric method and expressed in mg/100 g, fresh weight (FW)
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/AZUJB4"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
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
	

	f1 <- ff[basename(ff) == "Standard Evaluation Trial for Vitamin C Content Data.xls"]
	f2 <- ff[basename(ff) == "Standard Evaluation Trial for Vitamin C Content Dictionary.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		variety = r1[["Clone"]],
		year = r1[["Locality and Year of Evaluation"]]
	)
##r1: "Set", "Primary Trait", "Highland Tropic", "...5", "...6", "...7", "...8", "...9", "Vict. C (mg/100DW)", "Vict. C (mg/100FW)"


	d2 <- data.frame(
		crop = tolower(r2[["Crop"]])
	)
##r2: "Num", "Variable ID", "Variable label", "Variable name", "Variable synonyms", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9"


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

