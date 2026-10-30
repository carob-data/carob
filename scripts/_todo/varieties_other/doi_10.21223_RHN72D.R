# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Phenotypic characterization of 142 Tarwi accessions using agro-morphological cualitiative traits.

Tarwi (Lupinus mutabilis S.) is a legume native to the andes that stands out for its nutritional value and potential in food security programs. In the experimental fields of the National Institute of Agrarian Innovation (INIA) - Santa Ana, in the period from 2022 to 2023, 140 accessions from the INIA national collection of Tarwi germoplasm were evaluated, along with 2 reference cultivars, 'INIA 445 Masacan-chino' and 'Andenes 90', selected for their high adaptation and genetic stability. The agromorphological characterization was carried out using 40 qualitative descriptors based on the Lupin descriptors (https://hdl.handle.net/10568/73417).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/RHN72D"
	group <- "varieties_other"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
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
	

	f1 <- ff[basename(ff) == "01_Data_Tarwi_accesions.xlsx"]
	f2 <- ff[basename(ff) == "02_Frequency of QC.xlsx"]
	f3 <- ff[basename(ff) == "03_Data_dictionary.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		latitude = r1[["Latitude"]],
		longitude = r1[["Longitude"]],
		elevation = r1[["Altitude"]]
	)
##r1: "ID", "ID PARCELA", "PER CODE", "Local code", "CLUSTER", "ICCT", "SF", "ISC", "LS", "CLT", "STC", "ISTC", "PGPE", "FBCJBO", "IFBCJBO", "FWCJBO", "IFWCJBO", "MBCSJOF", "IMBCSJOF", "ICCSSJOF", "IRCSJOF", "IIRCSJOF", "FKCJBO", "IFKCJBO", "FWCJBW", "IFWCJBW", "MBCSFJBW", "IMBCSFJBW", "CCSSFJBW", "ICCSSFJBW", "IRCSFJBW", "IIRCSFJBW", "FKCJBW", "IFKCJBW", "UFP", "ICNFP", "IGPC", "GPP", "MPP", "SSH", "PSC", "IPSC", "SSC", "ISSC", "DSSC", "DF", "DH", "ST", "NPB", "LNS", "LI", "NPCA", "TNPP", "PLMS", "PWMS", "TSWHS", "YPLA", "YPLO", "SWI", "SLE", "STH", "sos", "eos", "los", "mgs"


	d2 <- data.frame(
		adm1 = carobiner::fix_name(r2[["State"]], "title")
	)
##r2: "Abbreviation", "Qualitative Characteristics (QC)", "Category", "General frequency", "Relative frequency"


	d3 <- data.frame()
##r3: "Variable name", "Abbreviation", "Unit", "Variable type", "Description", "Method", "Formula", "References"


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

