# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication data for: Pests and diseases affecting potato landraces and bred varieties grown in Peru under indigenous farming system

The major pests and diseases were identified and quantified on thirteen potato landraces and three bred varieties cultivated in Peru. Late blight (Phytophthora infestans) was the primary biotic constraint affecting plants before flowering with an average severity of 24%. No other pathogens caused severe foliar disease, but black scurf (Rhizoctonia solani) was relatively common on tubers of some genotypes with incidence ranging from 4.30 to 33.33%. The viruses most generally considered important in potato seed degeneration, PVY and PLRV, were extremely rare, with 1.11 and 0.12 % incidence, respectively. Other viruses considered mild, such as PVX and PVS, were more common, with incidence of 28.23 and 22.29 %, respectively. Potato flea beetle (Epitrix spp.), potato leaf beetle (Diabrotica spp.) and Andean potato weevil (Premnotrypes spp.) were common, with incidence of 28.14, 18.75 and 13.61%, respectively. Potato landraces known as Ishkupuru, Lengua de vaca, Chaulina, Chaulina Tajacaja and Negro cayash were identified as potentially resistant to P. infestans.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/B6ACYT"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Hemilio Valdizan University; Daniel Alcides Carrion University",
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
	f2 <- ff[basename(ff) == "Data_pests_and_diseases_incidence.xls"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="Fields_Geographic_locations")
	r2b <- carobiner::read.excel(f2, sheet=" Database incidence & severity")
	r2c <- carobiner::read.excel(f2, sheet="Screening for LB resistance")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		crop = tolower(r1[["Crop"]])
	)
##r1: "Abbreviation", "Variable ID", "Variable synonyms", "Trait ID", "Trait name", "Trait description", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale Xref", "Category 1", "Category 2"


	d2a <- data.frame(
		adm2 = carobiner::fix_name(r2a[["Province"]], "title"),
		plot_id = r2a[["Plot"]],
		elevation = r2a[["Altitude"]],
		latitude = r2a[["Latitude"]],
		longitude = r2a[["Longitude"]]
	)
##r2a: "Department", "Distrit", "Place", "Total area by plot (m2)"


	d2b <- data.frame(
		variety = r2b[["Sample plant/variety"]]
	)
##r2b: "Number of sample", "Place code", "Variety", "SFLate Blight", "SFEarly Blight", "SFAPW", "SFPotato flea beetle", "SFPotato leaf beetle", "IFLate Blight", "IFEarly Blight", "IFAPW", "IFPotato flea beetle", "IFPotato leaf beetle", "IFCanker by Rhizoctonia", "IFPVX", "IFPVY", "IFPLRV", "IFPVS", "IFAPLV", "IFAPMoV", "IFPMTV", "Tuber number harvested", "ITSoft rot", "ITLate blight tuber rot", "ITAPW", "ITBlack scarf", "ITDry rot", "ITWet rot", "ITPowdery scab", "ITWart", "ITPotato flea beetle", "ITBlack dot", "STSoft rot", "STLate blight tuber rot", "STAPW", "STBlack scarf", "STDry rot", "STWet rot", "STPowdery scarf", "STWart", "STPotato flea beetle", "STBlack dot"


	d2c <- data.frame()
##r2c: "...1", "...2", "Severity", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18"


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

