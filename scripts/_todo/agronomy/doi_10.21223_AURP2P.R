# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Effect of nitrogen fertilization on potato late blight in the field in Huancayo, Junin Peru

The effect of nitrogen (N) fertilization on the development of potato late blight disease, caused by Phytophthora infestans (Mont.) de Bary, was studied through field experiments in two locations in Peru (Huancayo and Oxapampa). These datasets correspond to the field experiments in Huancayo. Three levels of N (0, 160, and 320 kg/ha) and three levels of fungicide were applied in all combinations on two potato (Solanum tuberosum L.) varieties with different levels of resistance (18 treatments), and in four repetitions. N had no effect on the disease when severity was very low or high. At intermediate levels of disease severity, increased N led to increased disease. In Huancayo, the effect of 0 vs. 160 kg·ha-1 of N on  the more susceptible variety was negligible because of increased late blight.  For the more resistant variety, late blight had little effect on yield when N was applied at 0 or 160 kg·ha-1. Increasing the N fertilization rate to 320 kg·ha-1 did not further increase yield in the absence of late blight, but decreased it sharply in the treatments where the disease was present because no fungicides were applied. These results indicate the importance of considering N fertilization levels in integrated management schemes for potato late blight, and in comparison of results across experiments. If farmers want to increase potato yields with N fertilizer, they may need to use more fungicides or varieties with higher resistance.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/AURP2P"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
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
	

	f1 <- ff[basename(ff) == "01_data_LateBlightSeverity_Huancayo.xls"]
	f2 <- ff[basename(ff) == "02_LateBlight_Evaluation_date.xls"]
	f3 <- ff[basename(ff) == "03_Data_Canopy_cover_Huancayo.xls"]
	f4 <- ff[basename(ff) == "04_canopy_evaluation_days-1.xls"]
	f5 <- ff[basename(ff) == "05_TotalYield_Huancayo.xls"]
	f6 <- ff[basename(ff) == "06_Evaluation_date_Total biomass dry matter content.xls"]
	f7 <- ff[basename(ff) == "07_SpraysOfFungicides_Huancayo.xls"]
	f8 <- ff[basename(ff) == "DailyWeatherData_Huancayo.xls"]
	f9 <- ff[basename(ff) == "HourlyWeatherData_Huancayo.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)
	r8 <- carobiner::read.excel(f8)
	r9 <- carobiner::read.excel(f9)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		variety = r1[["Variety"]],
		N_fertilizer = r1[["Nitrogen_rate"]],
		treatment = r1[["Treatment"]],
		rep = r1[["Repetition"]]
	)
##r1: "id", "Fungicide_rate", "LB1", "LB2", "LB3", "LB4", "LB5", "LB6", "LB7", "LB8", "LB9", "LB10", "LB11", "LB12", "LB13", "LB14", "LB15", "LB16", "LB17", "LB18", "LB19", "LB20", "LB21", "LB22", "LB23", "LB24", "LB25", "LB26", "LB27", "LB28", "LB29"


	d2 <- data.frame()
##r2: "Abbreviation", "Evaluation_date", "Variable name", "Id ontology"


	d3 <- data.frame(
		variety = r3[["Variety"]],
		N_fertilizer = r3[["Nitrogen_rate"]],
		treatment = r3[["Treatment"]],
		rep = r3[["Repetition"]]
	)
##r3: "id", "Fungicide_rate", "CC_G_Et_%_1", "CC_G_Et_%_2", "CC_G_Et_%_3", "CC_G_Et_%_4", "CC_G_Et_%_5", "CC_G_Et_%_6", "CC_G_Et_%_7", "CC_G_Et_%_8", "CC_G_Et_%_9", "CC_G_Et_%_10", "CC_G_Et_%_11", "CC_G_Et_%_12", "CC_G_Et_%_13", "CC_G_Et_%_14", "CC_G_Et_%_15", "CC_G_Et_%_16", "CC_G_Et_%_17", "CC_G_Et_%_18", "CC_G_Et_%_19", "CC_G_Et_%_20", "CC_G_Et_%_21"


	d4 <- data.frame()
##r4: "Numeration", "Abbreviation", "Variable_name", "Number_evaluation_days", "Evaluation_date_CCG", "ID Ontology"


	d5 <- data.frame(
		variety = r5[["Variety"]],
		N_fertilizer = r5[["Nitrogen_rate"]],
		treatment = r5[["Treatment"]],
		rep = r5[["Repetition"]]
	)
##r5: "id", "Fungicide_rate", "TBDMC_kg_ha_Ev1", "TBDMC_kg_ha_Ev2", "TBDMC_kg_ha_Ev3", "TBDMC_kg_ha_Ev4", "TBDMC_kg_ha_Ev5"


	d6 <- data.frame()
##r6: "Numeration", "Abbreviation", "Evaluation_date_TBDMC", "Variable_name", "Id ontology"


	d7 <- data.frame()
##r7: "...1", "Fechas de Aplicación", "...3", "...4", "...5", "...6"


	d8 <- data.frame()
##r8: "ANEXO  5.", "Datos ambientales. Huancayo, 2000.", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11"


	d9 <- data.frame()
##r9: "Día", "texto", "Leaf Weat", "Temperature (*C)", "PAR (uM/m2)", "HR (%)", "LLUVIA mm"


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

