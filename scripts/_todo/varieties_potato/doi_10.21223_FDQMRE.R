# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Presence of seed-borne pest and pathogens on/in the seed produced by farmers in the province of Cotopaxi

Seed degeneration (PSD)  threats potato production in developing countries. PSD is defined as the accumulation of pest and pathogens in/on the seed tuber due to the successive cycles of vegetative propagation leading potentially to a yield and quality reduction (Thomas-Sharma et al., 2016; Pl. Path. [vol 65, issue 1]). However, the understanding of PSD in the Andes is deficient due to the limited comprehension of the spatial distribution of potato seed- and soil- borne pests and pathogens. For this reason, 260 farmers´ seed lots and fields were surveyed in the province of Cotopaxi-Ecuador from September to October 2018.   The survey was implemented using a stratified sampling design (stratum = Cantons of Latacunga, Pujilí, Salcedo and Saquisilí). The sample size was defined based on the seed replacement rate reported by farmers in a pilot study previously implemented. In each place, farmers kindly provided a sample of (1 to 10) potato seed tubers depending on their willingness. In addition to it, a soil sample was collected from the closest field to the house after farmers provided oral consent. Symptoms and damages on the seed tubers caused by insects and fungi were visually inspected following the methodology suggested by James (1971, [Canadian pl. dis. survey {vol. 51}]) and the photography guide of the main pests and pathogens of the potato crop in Ecuador (Montesdeoca et al., 2013)(Reported in sheet coined 'Insects and Fungi').   Virus identification was carried out on plantlets coming from the tubers assessed previously. This was performed by using the kits and the protocol for DAS-ELISA manufactured and suggested by CIP (2007). Six viruses were identified: PVX, PVS, PVY, APLV, PLRV, and APMoV (Reported in sheet coined 'Virus'). Forty three soil samples out of the 260 were selected depending on the farmers' field altitude and landscape location (Reported in sheet coined 'nematodes'). These were sent to the laboratory of Plant Protection of the National Agriculture Research Center (INIAP) for nematodes identification. Nematodes were identified according to the methodologies of Oostenbrink (1960) and Fenwick (1940).   It is expected that this database contributes to a deeper knowledge about the presence of seed-borne pests and pathogens in the tropical highlands of Ecuador and to design better seed system interventions.;The dataset contains data about presence of seed-borne pest and pathogens on/in the seed produced by farmers in the province of Cotopaxi.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/FDQMRE"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=8, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Wageningen University & Research Centre - WUR",
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
	

	f1 <- ff[basename(ff) == "DataDictionary_ElementDescription.csv"]
	f2 <- ff[basename(ff) == "DataDictionary_Introduction.csv"]
	f3 <- ff[basename(ff) == "ID_Information.csv"]
	f4 <- ff[basename(ff) == "Insects_Fungi.csv"]
	f5 <- ff[basename(ff) == "Nematodes.csv"]
	f6 <- ff[basename(ff) == "Virus.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)
	r6 <- read.csv(f6)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()


	d2 <- data.frame()


	d3 <- data.frame(
		location = r3[["ID.Location.Altitude.Index.Variety.N_Sample"]],
		elevation = r3[["ID.Location.Altitude.Index.Variety.N_Sample"]],
		variety = r3[["ID.Location.Altitude.Index.Variety.N_Sample"]]
	)


	d4 <- data.frame()
##r4: "X1.0.0.0.0.0.0.0.1.0.0.0.0.0.0.ok"


	d5 <- data.frame()
##r5: "X45.1.0.0.0.0.1.0.0.1.0.0.0.0.0.NA.NA.0.4.2.Incipient.60.Moderate.NA.NA.NA.20.NA.NA.NA"


	d6 <- data.frame(
		variety = r6[["ID.Variety.Other_Name.Obs.Presence_PVX.Presence_PVS.Presence_PVY.Presence_PLRV.Presence_APLV.Presence_APMoV"]]
	)


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

