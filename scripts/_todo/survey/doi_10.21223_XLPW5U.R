# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Farmer agroecological knowledge - CA3561

This dataset included information on 83 farmers' agroecological knowledge in  Phu Tho Phu Yen and Tay Ninh (CA3561).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/XLPW5U"
	group <- "survey"
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
	

	f1 <- ff[basename(ff) == "01_CA3561_database_of_farmer_agroecological_knowledge.xlsx"]
	f2 <- ff[basename(ff) == "02_CA3561_datadictionary_of_farmer_agroecological_knowledge.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		location = r1[["VILLAGE"]],
		adm3 = carobiner::fix_name(r1[["COMMUNE"]], "title"),
		adm2 = carobiner::fix_name(r1[["DISTRICT"]], "title"),
		hhid = r1[["HHFARMERS"]],
		yield = r1[["Q216YIELDCOMPARE"]],
		rep = r1[["Q36LANDPREPA"]],
		treatment = r1[["Q36STAKETREAT"]],
		plot_id = r1[["Q41PLOTRANK1"]]
	)
##r1: "NUM", "DATE", "USER", "PROVINCE", "ETHNIC", "AGE", "SEX", "EDU", "HHSIZE", "Q21CASSVARIETIES1", "Q21CASSVARIETIES2", "Q211CASSAREA1", "Q211CASSAREA2", "Q22PLANTMONTH1", "Q22HARVESTMONTH1", "Q22PLANTMONTH2", "Q22HARVESTMONTH2", "Q23METHMONO", "Q23METHINTER", "Q23PLANTINTER", "Q23METHROTATION", "Q23PLANTROTATION", "Q23METHVERTICAL", "Q23METHHORIZ", "Q23METHINCL", "Q24SOURCEPM", "Q24SOURCENAME", "Q24SOURCEDISTANCE1", "Q24SOURCEDISTANCE2", "Q24SOURCEDISTANCE3", "Q25CRITERIAPM1", "Q25CRITERIAPM2", "Q25CRITERIAPM3", "Q26FERTILIZERN", "Q26FERTILIZERNPK1", "Q26FERTILIZERNPK2", "Q26FERTILIZERP", "Q26FERTILIZERUREA2", "Q27TOTALHARVEST", "Q28SELLFRESHROOTS", "Q28SELLDRIEDCHIPS", "Q28SELLWHOLEFIELD", "Q29SELLTRADERS", "Q29TRADERPLACE", "Q29TRADERDISTANCE", "Q29SELLFACTORIES", "Q29FACTORIESDISTANCE", "Q213SUPPORT", "Q214SUPPORT", "Q215SELLPLACE", "Q216REASON", "Q217CASSCONSTRAINT1", "Q217CASSCONSTRAINT2", "Q217CASSCONSTRAINT3", "Q218RESEARCHPRIORITY1", "Q218RESEARCHPRIORITY2", "Q218RESEARCHPRIORITY3", "Q218RESEARCHPRIORITY4", "Q218RESEARCHPRIORITY5", "Q31PESTDIS", "Q32CMB", "Q32CMB1", "Q32CMB2", "Q32CMBPHOTO", "Q32CMBAPP", "Q32CMBSYMP", "Q32CMBYLOSS", "Q32CMBCAUS", "Q32CMBMANAG", "Q32CMBIFS", "Q32CWB", "Q32CWB1", "Q32CWB2", "Q32CWBPT", "Q32CWBAPP", "Q32CWBSYMP", "Q32CWBYLOSS", "Q32CWBCAUS", "Q32CWBMANAG", "Q32CWBIFS", "Q32CRM", "Q32CRM1", "Q32CRMAPP", "Q32CRMSYMP", "Q32CRMYLOSS", "Q32CRMCAUSE", "Q32CRMMANGE", "Q32CRMIFS", "Q33ATTRIBUTEPM", "Q34BIOLWA", "Q34BIOLWA1", "Q34BIOLWA2", "Q34BIOLWL", "Q34BIOLWL1", "Q34BIOLWL2", "Q34BIOLB", "Q34BIOLB1", "Q34BIOLB2", "Q34BIOAW", "Q34BIOAW1", "Q34BIOAW2", "Q34BIOPM", "Q34BIOPM1", "Q34BIOPM2", "Q35Pesticide1", "Q35PESTICIDE", "Q36PREVENTION", "Q36SOURCEINFO", "Q37INFORANK1", "Q37INFORANK2", "Q37INFORANK3", "Q41Totalarea", "Q41CASSAREA", "Q41CASSINFO1", "Q41CASSINFO2", "Q41CASSINFO3", "Q41MAIZEAREA", "Q41MAIZEINFO1", "Q41MAIZEINFO2", "Q41MAIZEINFO3", "Q41RICEAREA", "Q41RICEINFO1", "Q41RICEINFO2", "Q41RICEINFO3", "Q41SCAREA", "Q41SCINFO1", "Q41SCINFO2", "Q41SCINFO3", "Q41ACAAREA", "Q41ACAINFO1", "Q41ACAINFO2", "Q41ACAINFO3", "Q41RUBBERAREA", "Q41RUBBERINFO1", "Q41RUBBERINFO2", "Q41RUBBERINFO3", "Q41TEAAREA", "Q41TEAINFO1", "Q41TEAINFO2", "Q41TEAINFO3", "Q41PLOTRANK2", "Q41PLOTRANK3", "Q42PESTCONSTR", "Q43TRAINNING", "Q44TRAINING1", "Q441REASON", "Q45GENDER1", "Q45GENDER2", "Q45GENDER3", "Q45GENDER4", "Q45GENDER5", "Q45GENDER6", "Q45GENDER7", "Q45GENDER8", "Q45GENDER9", "Q46GENDERRANK1", "Q46GENDERRANK2", "Q46GENDERRANK3", "Q46GENDERRANK4", "MENTIONEDPEST", "KNOWPEST"


	d2 <- data.frame()
##r2: "Variable", "Question", "Scale", "Missings", "category1", "category2", "category3", "category4", "category5", "category6", "category7", "category8"


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

