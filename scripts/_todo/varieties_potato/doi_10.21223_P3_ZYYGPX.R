# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Standard evaluation trial for late blight  resistance documentation of CIP advanced breeding clones

One of the most notorious and economically devastating  disease of potatoes is late blight caused by Phytophthora infestans (Pi). Pi  is a hemibiotroph that can affect most parts of the potato plant, leaves  stems flowers fruits stolons and tubers.  Late blight is a polycyclic disease, having several cycles of infection and inoculum production during one growing season. Infection may increase proportionally to both the initial amount of inoculum and the amount of new inoculum produced during the growing season. The disease appears as water-soaked irregular pale green lesions mostly near tips and margins of leaves that rapidly grow into large brown to purplish black necrotic spots. When stems are attacked light and dark brown lesions appear  that elongate and stems become brittle at lesion areas and break.  Infected tuber show irregular reddish-brown to purplish slightly depressed areas that extend deep into internal tissue of the tubers.  Standard evaluation trial (SET)  for assessment late blight resistance in  advanced clones are conducted  in experimental trials under field conditions in endemic locations of the subtropical highlands. The percentage of leaf area affected by late blight infection is recorded by plot throughout the season to subsequently compute the area under the disease progress curve (AUDPC). Since AUDPC is subject to variation from factors that are experiment specific it is not appropriate for use as a measure to determine the level of resistance of a genotype across trials. However, AUDPC values are used to compute the predicted susceptibility scale value of any candidate genotype within a scale where increasing numbers represent increasing susceptibility. The predicted susceptibility scale value of any candidate genotype is calculated as the scale value of the standard cultivar (1 =for the most resistance standard cultivar -9 = for the most susceptible one) multiplied by the observed disease measure (AUDPC)  of the candidate genotype divided by the disease measure (AUDPC) of the standard genotype.  This predicted susceptibility scale (sAUDPC) is a  disease measure used by SET for  classifying potato genotypes for susceptibility to P. infestans with the advantage that its calculation  is not affected by the  inoculum, location, season,  and host susceptibility variation. Thus  sAUDPC allows to compare the level of resistance of a genotype under different environments and determine its overall performance
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/ZYYGPX"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
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
	

	f1 <- ff[basename(ff) == "Standard Evaluation Trial for Late Blight Resistance_Data.xls"]
	f2 <- ff[basename(ff) == "Standard Evaluation Trial for Late Blight Resistance_Dictionary.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		variety = r1[["Clone"]]
	)
##r1: "Set", "PT", "OXAPAMPA First Trial", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "OXAPAMPA Second Trial", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "OXAPAMPA Third Trial", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "COMAS First Trial", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "COMAS Second Trial", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "MONOBAMBA First Trial", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "OXA-1", "...59", "...60", "OXA-2", "...62", "...63", "OXA-3", "...65", "...66", "COM-1", "...68", "...69", "COM-2", "...71", "...72", "MONO-1", "...74", "...75", "Reaction", "...77", "...78", "...79", "...80", "...81", "AUDPC...82", "sAUDPC...83", "R...84", "AUDPC...85", "sAUDPC...86", "R...87"


	d2 <- data.frame(
		crop = tolower(r2[["Crop"]])
	)
##r2: "Num", "Variable label", "Variable synonyms", "Context of use", "Variable status", "Institution", "Date", "Language", "Trait ID", "Trait", "Trait class", "Trait description", "Trait status", "Method ID", "Method", "Method class", "Method description", "Method reference", "Scale ID", "Scale name", "Scale class", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9", "Category 10"


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

