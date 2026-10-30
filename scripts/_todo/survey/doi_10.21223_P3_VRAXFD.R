# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Participatory mapping for long-term monitoring and red listing of cultivated potato populations in Pasco, Peru

Biodiversity sustains a wide variety of genetic traits that are very valuable for the potato's adaption to changing environments and successful cultivation in the future. It is assumed that several factors are threatening biodiversity in the Andes. Without robust baselines and indicators used for reliable time lines, it is impossible to assess diversity loss or increase. Long-term studies in situ are necessary to distinguish short term fluctuation from general tendencies caused by environmental and socio-cultural shifts and to understand ongoing evolutionary principles within the potato genepool. Powerful tools are needed that facilitate the management, evolution and exchange of mainly oral transmitted traditional knowledge. Participatory mapping is such a tool where cognitive spatial knowledge is transformed into maps at grassroots level by trained community members. Local knowledge can be documented in context with social, political, biotic and abiotic factors and if accompanied with comprehensive capacity building and the application of robust indicators will strengthen the cross-generation knowledge exchange and sensitization towards land use shifts, resource dynamics affected by climate and societal related factors and pressure.  Most local farmers practice traditional small scale agriculture with focus on potato production, partly organized in sectoral fellow systems. Because of the high biological diversity, coexistence of wild potato species and the extensive local knowledge owned by the potato farmers, communities of this study site contribute to generate knowledge on dynamics of potato diversity in situ and complement the diversity hotspot network 'Chirapaq Ñan' for long-term in situ monitoring of cultivated potato. This dataset contributes to i) elaborate robust baseline to assess spatial and temporal dynamics and distribution ranges of potato diversity within and among fields, families and altitudinal belts, ii) elaborate on the application of utilized diversity indexes for red listing of potato landraces and cultivar types. The dataset contains parcel and community maps, geographic distribution of cultivated potato populations, red listing of potato landraces and cultivar types and supplementary files (surveys, protocols) used for this study.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/VRAXFD"
	group <- "survey"
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
	

	f1 <- ff[basename(ff) == "FrecuencyOfVarietiesByArea_Pasco.xlsx"]
	f2 <- ff[basename(ff) == "Household_Dataset_Pasco.xlsx"]
	f3 <- ff[basename(ff) == "RotationRatio_Pasco.xlsx"]
	f4 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Bellavista.CPG"]
	f5 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Bellavista.dbf"]
	f6 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Bellavista.prj"]
	f7 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Bellavista.rar"]
	f8 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Bellavista.sbn"]
	f9 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Bellavista.sbx"]
	f10 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Bellavista.shp"]
	f11 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Bellavista.shp.xml"]
	f12 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Bellavista.shx"]
	f13 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Chupaca.CPG"]
	f14 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Chupaca.dbf"]
	f15 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Chupaca.prj"]
	f16 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Chupaca.rar"]
	f17 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Chupaca.sbn"]
	f18 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Chupaca.sbx"]
	f19 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Chupaca.shp"]
	f20 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Chupaca.shp.xml"]
	f21 <- ff[basename(ff) == "CommunityBoundaries_Pasco_Chupaca.shx"]

	r1a <- carobiner::read.excel(f1, sheet="Lista_Area")
	r1b <- carobiner::read.excel(f1, sheet="Lista_OCF")
	r1c <- carobiner::read.excel(f1, sheet="Lista_RCF")
	r1d <- carobiner::read.excel(f1, sheet="data dictionary")
	r2a <- carobiner::read.excel(f2, sheet="data")
	r2b <- carobiner::read.excel(f2, sheet="DataDiccionary")
	r3a <- carobiner::read.excel(f3, sheet="data")
	r3b <- carobiner::read.excel(f3, sheet="Data_dictionary")
	#r4 <- read.???(f4)
	#r5 <- read.???(f5)
	#r6 <- read.???(f6)
	#r7 <- read.???(f7)
	#r8 <- read.???(f8)
	#r9 <- read.???(f9)
	#r10 <- read.???(f10)
	#r11 <- read.???(f11)
	#r12 <- read.???(f12)
	#r13 <- read.???(f13)
	#r14 <- read.???(f14)
	#r15 <- read.???(f15)
	#r16 <- read.???(f16)
	#r17 <- read.???(f17)
	#r18 <- read.???(f18)
	#r19 <- read.???(f19)
	#r20 <- read.???(f20)
	#r21 <- read.???(f21)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		variety = r1a[["Variedad"]]
	)
##r1a: "categoria", "Area m2"


	d1b <- data.frame(
		adm1 = carobiner::fix_name(r1b[["Region"]], "title"),
		variety = r1b[["variedad_corregida"]]
	)
##r1b: "categoria", "OCF", "Clasificacion"


	d1c <- data.frame(
		adm1 = carobiner::fix_name(r1c[["Region"]], "title"),
		variety = r1c[["variedad_corregida"]]
	)
##r1c: "categoria", "RCF", "clasificacion"


	d1d <- data.frame()
##r1d: "Lista Area", "...2", "...3", "...4", "Lista OCF", "...6", "...7", "...8", "Lista RCF", "...10", "...11"


	d2a <- data.frame()
##r2a: "Geographic Data", "...2", "...3", "Basic data", "...5", "Family data", "...7", "...8", "...9", "...10", "Potatoe data", "...12", "...13", "Family economy", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28"


	d2b <- data.frame()
##r2b: "ID", "Name of The Field", "Description"


	d3a <- data.frame(
		country = r3a[["Country"]],
		adm1 = carobiner::fix_name(r3a[["Region"]], "title"),
		adm3 = carobiner::fix_name(r3a[["Community"]], "title"),
		latitude = r3a[["latitud"]],
		longitude = r3a[["longitud"]],
		planting_date = r3a[["Planting date"]],
		harvest_date = r3a[["Harvest date"]],
		variety = r3a[["Cultivar"]],
		N_fertilizer = r3a[["N_Fallow"]]
	)
##r3a: "R_Alt_100m", "R_Alt_200", "CategoriaFinal", "Area", "Code...8", "Fam", "Par", "Code...11", "Zone Name", "type of planting", "Planting method", "Apply pesticide or fertilizer", "Property", "Sector", "Slope", "Seed source", "Main destination planned production", "further use", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "Observation", "General", "C", "L", "H", "ARTC", "POTATO", "TOTAL", "PROP", "Total", "Ratio", "Cultivar_Final"


	d3b <- data.frame(
		variety = r3b[["Cultivar"]]
	)
##r3b: "Name", "General", "...4", "Nombre", "clasificacion general", "simbolo general", "...8", "R_Alt_100m", "=", "Altitudinal range every 100 m"


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

