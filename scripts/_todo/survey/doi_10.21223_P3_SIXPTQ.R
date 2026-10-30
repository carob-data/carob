# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Tackling Food Insecurity and Malnutrition through Diversification: Exploiting the Potential of Potato and Sweetpotato to Reduce Food Insecurity and Dependence on Cereals in SNNPR and Tigray. Better Potato for a Better Life (BPBL) Baseline Survey

The baseline survey is designed with an aim of determining the level of community involvement in potato and sweetpotato production, consumption and marketing and as a means of improving livelihoods of the communities in SNNPR and Tigray regions. It looks at the general household demographic profile, livelihood, income, production and marketing realities and constraints affecting the potato and sweetpotato yields in the two regions prior to CIPUSAID project intervention. Food insecurity is the major problem of smallholder farmers in many areas of rural Ethiopia. The data is expected to set benchmark values to serve as initial measure to assess the outcomes or impact of the 'Better Potato for a Better Life' project interventions and will be used as a reference by the project and the donor, USAID. The data from the baseline survey will be incorporated directly into the programmatic decision making process to ensure that best strategies are employed during implementation of the project. The baseline survey was conducted from August 2010 to December 2010 in the two project intervention regions namely, Southern Nations, Nationalities and Peoples Region (SNNPR) and Tigray. The survey was carried out in the eleven administrative zones, twenty two woredas (districts) and forty four kebeles/tabias (Peasant Associations-PAs) of these two regions. The first criterion used was potato and sweetpotato potential producing areas as sampling frame. These areas were identified in collaboration with partner institutions Bureau of Agriculture from both regions, GOAL Ethiopia and Vita-RT. Different administrative levels were considered for stratification which included region, zone, woreda and kebele/tabia (the lowest administrative unit). All the woredas in which the project is being implemented in the two regions and the eleven zones were considered during the baseline study. The sample size distribution between the two regions was done proportionally with respect to the potato and sweetpotato production potential across all project intervention woredas and kebeles/tabias. In each of the project kebeles/tabias, twenty four participating farm households were interviewed. These were selected randomly from the list of project participating households if their total number was more than twenty four. In case there were less than twenty four participating farmers in a selected intervention kebele/tabia, all participating  farmers plus additional farmers who were selected to participate the next season were interviewed to meet the total twenty four samples of households. Another nearby kebele/tabia was selected randomly as non-participant from which twelve farmers were  selected randomly for the interview. In case the farmer targeted for an interview was not found at home during the visit, a spouse was recommended to answer on behalf of the farmer. If both of them were not able to be located during the day, additional reserve samples were drawn to meet the required number of households to be interviewed. Data entry and cleaning was done from December 2010 - August 2011 and data analysis and report writing was done September 2011 - December 2011.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/SIXPTQ"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; CIAT; GIZ",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Households; universe: The study targeted farm households expected to participate in project activities (participants) as well as information from households not to be exposed to the project intervention (non-participants), thereby establishing the counter-factual.; timeMethod: Cross-sectional Survey; dataCollector: International Potato Centre (CIP); samplingProcedure: The first criterion used was potato and sweet potato potential producing areas as sampling frame. These areas were identified in collaboration with partner institutions. Different administrative levels were considered for stratification which included region, zone, wereda and kebele/tabia (the lowest administrative unit). All the weredas in which the project is being implemented in the two regions and the eleven zones were considered during the baseline study. The sample size distribution between the two regions was done proportionally with respect to the potato and sweet potato production potential across all project intervention weredas and kebeles/tabias.
In each of the project kebeles/tabias, twenty four participating farm households were interviewed. These were selected randomly from the list of project participating households if their total number was more than twenty four. In case there were less than twenty four participating farmers in a selected intervention kebele/tabia, all participating farmers plus additional farmers who were selected to participate the next season were interviewed to meet the total twenty four samples of households. Another nearby kebele/tabia was selected randomly as non-participant from which twelve farmers were selected randomly for the interview. In case the farmer targeted for an interview was not found at home during the visit, a spouse was recommended to answer on behalf of the farmer. If both of them were not able to be located during the day, additional reserve samples were drawn to meet the required number of households to be interviewed.; targetSampleSize: list(targetSampleActualSize = list(typeName = "targetSampleActualSize", multiple = FALSE, typeClass = "primitive", value = "732")); collectionMode: Face-to-Face Interviews using Structured Questionnaires; researchInstrument: Structured Questionnaires available in local languages",
		
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
	

	f1 <- ff[basename(ff) == "MODULE A HOUSEHOLD IDENTIFICATION-1.dta"]
	f2 <- ff[basename(ff) == "MODULE B HOUSEHOLD COMPOSITION-1.dta"]
	f3 <- ff[basename(ff) == "MODULE C HOUSEHOLD RESOURCES-1.dta"]
	f4 <- ff[basename(ff) == "MODULE D HOUSEHOLD CROP ACTIVITY BELG SEASON-1.dta"]
	f5 <- ff[basename(ff) == "MODULE D HOUSEHOLD CROP ACTIVITY MEHER SEASON-1.dta"]
	f6 <- ff[basename(ff) == "MODULE E  HOUSEHOLD PRIOR EXPOSURE TO AGRICULTURE TECHNICAL ASSISTANCE-1.dta"]
	f7 <- ff[basename(ff) == "MODULE F POTATO MANAGEMENT-1.dta"]
	f8 <- ff[basename(ff) == "MODULE F POTATO MANAGEMENT CONTROL METHODS-1.dta"]
	f9 <- ff[basename(ff) == "MODULE G SWEETPOTATO MANAGEMENT -1.dta"]
	f10 <- ff[basename(ff) == "MODULE H FREQUENCY OF CONSUMPTION OF VITAMIN A RICH FOODS-1.dta"]
	f11 <- ff[basename(ff) == "MODULE I HOUSEHOLD LEVEL DIETARY DIVERSITY AND YOUNG CHILD DIET DIVERSITY-1.dta"]
	f12 <- ff[basename(ff) == "MODULE A HOUSEHOLD IDENTIFICATION.sav"]
	f13 <- ff[basename(ff) == "MODULE B HOUSEHOLD COMPOSITION.sav"]
	f14 <- ff[basename(ff) == "MODULE C HOUSEHOLD RESOURCES.sav"]
	f15 <- ff[basename(ff) == "MODULE D HOUSEHOLD CROP ACTIVITY BELG SEASON.sav"]
	f16 <- ff[basename(ff) == "MODULE D HOUSEHOLD CROP ACTIVITY MEHER SEASON.sav"]
	f17 <- ff[basename(ff) == "MODULE E  HOUSEHOLD PRIOR EXPOSURE TO AGRICULTURE TECHNICAL ASSISTANCE.sav"]
	f18 <- ff[basename(ff) == "MODULE F POTATO MANAGEMENT CONTROL METHODS.sav"]
	f19 <- ff[basename(ff) == "MODULE F POTATO MANAGEMENT.sav"]
	f20 <- ff[basename(ff) == "MODULE G SWEETPOTATO MANAGEMENT .sav"]
	f21 <- ff[basename(ff) == "MODULE H FREQUENCY OF CONSUMPTION OF VITAMIN A RICH FOODS.sav"]
	f22 <- ff[basename(ff) == "MODULE I HOUSEHOLD LEVEL DIETARY DIVERSITY AND YOUNG CHILD DIET DIVERSITY.sav"]

	r1 <- haven::read_dta(f1)
	r2 <- haven::read_dta(f2)
	r3 <- haven::read_dta(f3)
	r4 <- haven::read_dta(f4)
	r5 <- haven::read_dta(f5)
	r6 <- haven::read_dta(f6)
	r7 <- haven::read_dta(f7)
	r8 <- haven::read_dta(f8)
	r9 <- haven::read_dta(f9)
	r10 <- haven::read_dta(f10)
	r11 <- haven::read_dta(f11)
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
	#r22 <- read.???(f22)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		hhid = r1[["HHID"]],
		crop = tolower(r1[["CROPTYPE"]])
	)
##r1: "A01B", "A02B", "A03B", "A04B", "A05B", "A06B", "FARMTYPE", "A24A", "A24B", "A24C", "A25A", "A25B", "A26C", "A26D", "A27A", "BPLG", "BPLM", "BPLS", "BPLOG", "BPLOM", "BPLOS", "BPAL", "BPP", "BSPP", "BOFSP", "DPOE1", "DPOF2", "DPOG3"


	d2 <- data.frame(
		hhid = r2[["HHID"]],
		crop = tolower(r2[["CROPTYPE"]])
	)
##r2: "FARMTYPE", "A01B", "A02B", "A03B", "A04B", "A05B", "A06B", "A06A", "MID_CODE", "FM_NAME", "MSEX", "RSHH", "AGE", "MSTATUS", "MOCCUP", "EDUCATION", "MFGROUP", "MINCOME"


	d3 <- data.frame(
		hhid = r3[["HH_ID"]],
		crop = tolower(r3[["CROPTYPE"]]),
		plot_id = r3[["PotatoplotM_big"]]
	)
##r3: "A01B", "A02B", "A03B", "A04B", "A06B", "FARMTYPE", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12_ha", "C13_ha", "C14_ha", "C15_ha", "C15A", "C16", "C17", "C18", "C19", "C19A", "C19B", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "NENOUGH", "AUG", "SEPT", "OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JULY", "PotatoplotB_big", "D1_ha", "D2_ha", "D3_ha", "D4_ha", "D5_ha", "Totalland"


	d4 <- data.frame(
		hhid = r4[["HHID"]],
		crop = tolower(r4[["CROPTYPE"]]),
		plot_id = r4[["BPLOT"]]
	)
##r4: "FARMTYPE", "A01B", "A02B", "A03B", "A04B", "A05B", "A06B", "A24A", "A24B", "A24C", "A25A", "A25B", "A26C", "A26D", "BPP", "BSPP", "BOFSP", "BAREA_ha", "BWD", "BC", "BTP", "BUP", "BSOLD", "BSEED", "BCONS", "BOTHER"


	d5 <- data.frame(
		hhid = r5[["HHID"]],
		crop = tolower(r5[["CROPTYPE"]]),
		plot_id = r5[["MPLOT"]]
	)
##r5: "A01B", "A02B", "A03B", "A04B", "A05B", "A06B", "FARMTYPE", "A24A", "A24B", "A24C", "A25A", "A25B", "A26C", "A26D", "A28", "BPP", "BSPP", "BOFSP", "LTUN", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "MAREA_ha", "MWDP", "MCD", "MTP", "Totproduction", "MSOLD", "MSEED", "MCONS", "MOTHER"


	d6 <- data.frame(
		hhid = r6[["HHID"]],
		crop = tolower(r6[["CROPTYPE"]])
	)
##r6: "FARMTYPE", "A01B", "A02B", "A03B", "A04B", "A05B", "E1", "E2A", "E2B", "E2C", "E2D", "E2E", "E2EA", "E3AA", "E3AB", "E4A", "E4B", "E4C", "E4D", "E4E", "E4EA", "A5AA", "E5AY", "A5AB", "E5BY", "A5AC", "E5CY", "A5AD", "E5DY", "E6A", "E6B", "E6C", "E6D", "E6E", "E6F", "E6G", "E6H", "E6I", "E6J", "E6K", "E6L", "E6M", "E6N", "E6O", "E6OA", "E7A", "E7B", "E7C", "E7D", "E7DA"


	d7 <- data.frame(
		hhid = r7[["HH_ID"]],
		crop = tolower(r7[["CROPTYPE"]])
	)
##r7: "A01B", "A02B", "A03B", "A04B", "A05B", "A06B", "FARMTYPE", "F1A", "F1B", "F2A", "F2B", "F3A", "F3B", "F3C", "F3D", "F3E", "F3F", "F3G", "F3H", "F3I", "F3J", "F3JA", "F4A", "F4B", "F5", "F6A", "F6B", "F6C", "F7", "F8A", "F8B", "F8C", "F8DA", "F8D", "F8EA", "F8E", "F8FA", "F8F", "F8GA", "F8G", "F8HA", "F8H", "VU_1", "VU_2", "VU_3", "SS_1", "SS_2", "SS_3", "QS_1", "QS_2", "QS_3", "PQ_1", "PQ_2", "PQ_3", "F10A", "F10B", "F15CA", "F15CB", "F15CC", "F15CD", "F11", "F12", "F13AA", "F13AB", "F13AC", "F13BA", "F13BB", "F13BC", "F14A", "F14B", "F14C", "F14D", "F14E", "F14F", "F14FA", "F15", "F17", "F18A", "F18B", "F19A", "F19B", "F19C", "F19D", "F19E", "F19F", "F19G", "F19H", "F19HA", "F20"


	d8 <- data.frame(
		hhid = r8[["HHID"]],
		crop = tolower(r8[["CROPTYPE"]])
	)
##r8: "A01B", "A02B", "A03B", "A04B", "A05B", "A06B", "FARMTYPE", "F14A", "F14B", "F14C", "F14D", "F14E", "F14F", "F14FA", "F15", "C_Measures", "LBCC", "LBNA", "BWCC", "BWNA", "LRCC", "LRNA", "EBCC", "EBNA", "PCC", "PNA", "OCC", "ONA"


	d9 <- data.frame(
		hhid = r9[["HH_ID"]],
		crop = tolower(r9[["CROPTYPE"]])
	)
##r9: "A01B", "A02B", "A03B", "A04B", "A05B", "A06B", "FARMTYPE", "G1A", "G1B", "G2A", "G2B", "G3A", "G3B", "G4A", "G4B", "G5A", "G5B", "G5C", "G5D", "G5E", "G5F", "G5G", "G5H", "G6A", "G6B", "G6C", "G6D", "G6E", "G6F", "G6FA", "G7A", "G7B", "G7C", "G7D", "G7E", "G7F", "G7FA", "G8A", "G8B", "G8C", "G8D", "G8E", "G8EA", "G9", "G10A", "G10B", "G10C", "G11", "G12A", "G12B", "G12C", "G12DB", "G12D", "G12EB", "G12E", "G12FB", "G12F", "G12GB", "G12G", "G12HB", "G12H", "G13A", "G13B", "G13C", "G13D", "G13E", "G13F", "G13G", "G13GA", "G14A", "G15", "G16", "G17", "G18", "G19", "G20", "G21", "G22", "G23", "G24", "G25", "G26", "G29", "G30", "G31", "G32", "G33", "G34", "G35A", "G35B", "G36A", "G36B", "G37A", "G37B", "G38A", "G38B", "G39A", "G39B", "G40A", "G40B", "G41A", "G41B", "G42A", "G42B", "G43", "G44A", "G44C", "G44B", "G44D", "G45A", "G45B", "G45C", "G45D", "G45E"


	d10 <- data.frame(
		hhid = r10[["HH_ID"]],
		crop = tolower(r10[["CROPTYPE"]])
	)
##r10: "A01B", "A02B", "A03B", "A04B", "A05B", "A06B", "FARMTYPE", "H01A", "H01B", "H02", "H03", "H04", "H05", "H06A", "H06B", "H06C", "H06D", "H06E", "H06F", "H06G", "H06H", "H06I", "H06J", "H06K", "H06L", "H06M", "H06N", "H06O", "H06P", "H06Q", "H07A", "H07B", "H07C", "H07D", "H07E", "H07F", "H07G", "H07H", "H07I", "H07J", "H07K", "H07L", "H08A", "H08B", "H09A", "H09B", "H10", "H11"


	d11 <- data.frame(
		hhid = r11[["HH_ID"]],
		crop = tolower(r11[["CROPTYPE"]])
	)
##r11: "A01B", "A02B", "A03B", "A04B", "A05B", "A06B", "FARMTYPE", "NHSC", "CNHSC", "I1A", "I1B", "I2A", "I2B", "I3A", "I3B", "I4A", "I4B", "I5A", "I5B", "I6A", "I6B", "I7A", "I7B", "I8A", "I8B", "I9A", "I9B", "I10A", "I10B", "I11A", "I11B", "I12A", "I12B"


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

