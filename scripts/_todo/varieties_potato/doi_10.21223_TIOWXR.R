# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Organic agriculture trial in the locality of La Soledad, Chugay district, La Libertad - Peru (Second season 2023-2024)

An experimental potato trial based on organic agriculture in the second season (October 25th, 2023 to April 16th, 2024) was conducted in a total area of 987 m2 with the support of the 'Asociación  de Productores Agropecuarios y Semilleristas La Poderosa' of the locality of La Soledad, Chugay district, province of Sánchez Carrión, La Libertad. This trial, the improved potato variety INIA-325 Poderosa was used with a split-plit plot design considering three factors: crop intercropping (intercropping with faba bean, variety Señorita and no intercropping), use of a decision support system for fungicide application or 'Juego de Ruedas' (Disc and no disc) and the application of organic amendments (1, 2 and 4 t ha-1 of poultry manure). The combination of these factors generated a total of 12 treatments; and an additional control treatment (conventional practices in the study zone). Four replicates were established, with a total of 52 plots of 3x4 m2 each (distance between plant and furrow of 0.3 m and 1 m, respectively). The variables evaluated were: fresh tuber yield (t ha-1), dry matter content of tubers (%), cost-benefit ratio, carbon footprint (kg CO2e Total) and soil analysis.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/TIOWXR"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Pataz Association - AP",
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
	

	f1 <- ff[basename(ff) == "1_Harvest LS2.xlsx"]
	f2 <- ff[basename(ff) == "2_Yield-BCR-CF LS2.xlsx"]
	f3 <- ff[basename(ff) == "3_Dry matter of tuber LS2.xlsx"]
	f4 <- ff[basename(ff) == "4_Soil analysis LS2.xlsx"]
	f5 <- ff[basename(ff) == "5_Data_Dictionary_OA.xlsx"]
	f6 <- ff[basename(ff) == "Design Schematic_OA.xlsx"]
	f7 <- ff[basename(ff) == "La Soledad S2.jpg"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	#r7 <- read.???(f7)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		plot_id = r1[["Plot"]],
		treatment = r1[["Treatment"]]
	)
##r1: "ID", "Block", "Sub-Plot", "Sub-Sub-Plot", "Code", "CI_f1", "DSSF_f2", "OA_f3", "dap", "MTW", "NoMTW", "WDT", "nMTW", "nNoMTW", "nWDT", "nPha", "Tub_Unif", "nFBHa", "WFB"


	d2 <- data.frame(
		plot_id = r2[["Plot"]],
		treatment = r2[["Treatment"]]
	)
##r2: "ID", "Block", "Sub-Plot", "Sub-Sub-Plot", "Code", "CI_f1", "DSSF_f2", "OA_f3", "FTY", "FFBY", "TEY", "MTY", "MEY", "CP", "GR", "BCR", "CF", "CF_ha", "CF_ton"


	d3 <- data.frame(
		plot_id = r3[["Plot"]],
		treatment = r3[["Treatment"]]
	)
##r3: "ID", "Block", "Sub-Plot", "Sub-Sub-Plot", "Code", "CI_f1", "DSSF_f2", "OA_f3", "FWt", "DWt", "DMCt"


	d4 <- data.frame(
		plot_id = r4[["Plot"]],
		treatment = r4[["Treatment"]],
		P_fertilizer = r4[["P"]],
		K_fertilizer = r4[["K"]]
	)
##r4: "ID", "Block", "Sub-Plot", "Sub-Sub-Plot", "Code", "CI_f1", "DSSF_f2", "OA_f3", "pH", "EC", "CaCO3", "OM1", "Sand", "Silt", "Clay", "TC", "CEC", "Ca2+", "Mg2+", "K+", "Na+", "Al+3 + H+3", "Scat", "Sbas", "Bsat", "BD", "Clevel A", "OClevel A", "OM2level A", "Nlevel A", "Clevel B", "OClevel B", "OM2level B", "Nlevel B"


	d5 <- data.frame()
##r5: "Variable name", "Abbreviation", "Unit", "Description", "Method", "Formula", "Reference"


	d6 <- data.frame()
##r6: "EXPERIMENTAL DESIGN OF ORGANIC AGRICULTURE TRIAL IN THE LOCALITY OF LA SOLEDAD - CHUGAY 2023-2024", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26"


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

