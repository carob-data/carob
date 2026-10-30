# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication Data for: Promotion of Orange-Fleshed Sweet Potato Increased Vitamin A Intakes and Reduced the Odds of Low Retinol-Binding Protein among Postpartum Kenyan Women

Background:Orange-fleshed sweet potato (OFSP) improves vitamin A (VA) status of young children; research with pregnant and lactating women is limited.Objective:We examined the effectiveness of the Mama SASHA (Sweetpotato Action for Security and Health in Africa) program to improve nutrition knowledge, diets, and nutritional status of pregnant and lactating women (PLW) in Western Kenya. Methods: Eight health facilities were allocated to the Mama SASHA intervention or comparison arms. PLW in intervention facilities received enhanced nutrition counseling at health clinics, were linked with community-based maternal support groups, and received vouchers for OFSP vine cuttings. Control PLW received clinic-based nutrition counseling only. A totalof 505 women in early and midpregnancy, attending their first antenatal care visit, and with no previous engagement in project activities were enrolled from the 8 facilities. Nutrition and health-seeking knowledge, food security, dietary patterns, and anthropometric measurements were collected at 4 time points at 9 mo postpartum. VA intakes were assessed with multipass 24-h recalls in a subsample of 206 mothers at 8–10 mo postpartum. VA status was assessed by using serum retinol-binding protein (RBP). Impacts were estimated with multilevel mixed models adjusted for clustering and differences at enrollment. Results: At enrollment, 22.9% of women had RBP <1.17mmol/L. By 9 mo postpartum, intervention women had significantly higher intakes of VA [adjusted difference = 297.0 retinol activity equivalent (RAE) units; 95% CI: 82, 513 RAE units;P= 0.01;n= 206], greater consumption of VA-rich fruit and vegetables in the previous 7 d (difference-in-difference estimate: 0.40 d; 95%CI: 0.23, 0.56 d;P< 0.01), and a 45% reduction in the odds of RBP <1.17mmol/L (OR: 0.55; 95% CI: 0.33, 0.92;P= 0.01). Conclusion: Promotion of OFSP to PLW through health services is a feasible strategy to improve women's nutrition knowledge, VA intakes, and maternal RBP. 

Promotion of Orange-Fleshed Sweet Potato Increased Vitamin A Intakes and Reduced the Odds of Low Retinol-Binding Protein among Postpartum Kenyan Women. Available from: https://www.researchgate.net/publication/316111399_Promotion_of_Orange-Fleshed_Sweet_Potato_Increased_Vitamin_A_Intakes_and_Reduced_the_Odds_of_Low_Retinol-Binding_Protein_among_Postpartum_Kenyan_Women [accessed Jul 23 2018].
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/XAW0XY"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "Hubert Department of Global Health, Rollins School of Public Health, Emory University, Atlanta, GA; CIP; Division of Epidemiology, Dalla Lana School of Public Health, University of Toronto, Toronto, Ontario, Canada; Department of Global Health, University of Washington, Seattle, WA",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Women from their first ANC visit to 9 months postpartum; universe: A longitudinal cohort study that followed women from their first ANC visit to 9 mopostpartum. From November 2012 to February 2013, trained health workers pre-screened 3025 women attending the 8 facilities for eligibility criteria. Pre-screening criteria included age 17–40 y, attending the first ANC visit, and being in early and mid pregnancy (10–26 wk). Of those pre-screened, 2307 were not eligible. Additional criteria were then applied and included planning to breastfeed and planning to live in their current village until 10 mo postpartum. Exclusion criteria included involvement in Mama SASHA activities during a previous pregnancy, living in an intervention village but visiting a control facility, or vice versa, or living outside of the Mama SASHA project catchment area (i.e., outside control and intervention villages). After pre-screening, of the 718 eligible participants, 505 women were eligible upon additional screening and were consented and enrolled.; timeMethod: A longitudinal cohort study; dataCollector: CIP; frequencyOfDataCollection: At enrollment and at 3 subsequent visits in late pregnancy at 4 and at 9 months postpartum; samplingProcedure: From the 2010 baseline household surveydata on predictors of child VA status conducted in Mama SASHA controland intervention communities, we estimated the intraclass correlation coefficient for infant VA status to be 0.02. Data on RBP in women were not available for this population. However, assuming a population SD in breast-milk retinol of 0.7mmol/L (7), we estimated that 400 women would be required to detect a significant double difference in breast-milk retinol of 0.1mmol/L, with 95% confidence and 80% power. Recruiting an additional 25% for anticipated losses to follow-up yielded an estimated required sample size of 500 women.; targetSampleSize: list(targetSampleActualSize = list(typeName = "targetSampleActualSize", multiple = FALSE, typeClass = "primitive", value = "505")); researchInstrument: Trained research assistants administered an extensive questionnaire at enrollment and at 3 subsequent visits in late pregnancyat 4 and at 9 months postpartum. Questionnaires collected detailed information on socio demographic characteristics, household food security and diet diversity, maternal and infant diet and consumption of Vitamin A-rich foods, maternal health and nutrition knowledge, sweetpotato production, and participation in program activities. Maternal weight and maternal midupper arm circumference (MUAC) were measured in duplicate by using a Seca 874 scale and flexible, nonstretchable tape, respectively, at each visit.",
		
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
	

	f1 <- ff[basename(ff) == "COVA_FC_COVERPAGE_CHILD_MR_RR_22FEB14_FINAl.xlsx"]
	f2 <- ff[basename(ff) == "COVA_FC_COVERPAGE_MOTHER_MR_RR_22FEB14_FINAL.xlsx"]
	f3 <- ff[basename(ff) == "COVA_FC_LONGREPORT_MR_RR_VERSION1_22FEB14_FINAL.xlsx"]
	f4 <- ff[basename(ff) == "COVA_FC_LONGREPORT_MR_RR_VERSION2_22FEB14_FINAL.xlsx"]
	f5 <- ff[basename(ff) == "COVA_FC_SHORTREPORT_MR_RR_22FEB14_FINAL.xlsx"]
	f6 <- ff[basename(ff) == "COVA_FC_SURVEY LOCATION FILE_22FEB14_FINAL.xlsx"]
	f7 <- ff[basename(ff) == "COHORT STUDY VISIT 1 DATA DICTIONARY.txt"]
	f8 <- ff[basename(ff) == "COHORT STUDY VISIT 2 DATA DICTIONARY.txt"]
	f9 <- ff[basename(ff) == "COHORT STUDY VISIT 3 DATA DICTIONARY.txt"]
	f10 <- ff[basename(ff) == "COHORT STUDY VISIT 4 DATA DICTIONARY.txt"]
	f11 <- ff[basename(ff) == "fc_merge_ea.sas7bdat"]

	r1a <- carobiner::read.excel(f1, sheet="Data")
	r1b <- carobiner::read.excel(f1, sheet="Data Dictionary")
	r2a <- carobiner::read.excel(f2, sheet="Data")
	r2b <- carobiner::read.excel(f2, sheet="Data Dictionary")
	r3a <- carobiner::read.excel(f3, sheet="DATA")
	r3b <- carobiner::read.excel(f3, sheet="VARIABLES")
	r4a <- carobiner::read.excel(f4, sheet="Data")
	r4b <- carobiner::read.excel(f4, sheet="Variables")
	r5a <- carobiner::read.excel(f5, sheet="Data")
	r5b <- carobiner::read.excel(f5, sheet="Variables")
	r6a <- carobiner::read.excel(f6, sheet="Data")
	r6b <- carobiner::read.excel(f6, sheet="Variables")
	#r7 <- read.???(f7)
	#r8 <- read.???(f8)
	#r9 <- read.???(f9)
	#r10 <- read.???(f10)
	#r11 <- read.???(f11)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		hhid = r1a[["HHID"]],
		latitude = r1a[["LATEST1"]]
	)
##r1a: "DI_ROUND", "FACILITY", "MATERNAL_ID", "MR2", "CFNAME", "CR02", "CR03", "CR04", "VITA_MNTHS", "LATEST2", "LATEST3", "CR06", "CR07", "CR08", "CR09A", "CR10A", "CR09B", "CR10B", "CR09C", "CR10C", "START", "END"


	d1b <- data.frame(
		rep = r1b[["MAIN RECALL OR REPEAT RECALL"]]
	)
##r1b: "DI_ROUND"


	d2a <- data.frame(
		hhid = r2a[["HHID"]],
		latitude = r2a[["LATEST1"]]
	)
##r2a: "DI_ROUND", "FAC_ID2", "MATERNAL_ID", "MFNAME", "MR2", "MR3", "MR4", "VITA_MNTHS", "LATEST2", "LATEST3", "MR6", "MR7", "MR8", "MR9", "MR10", "MR11", "MR11A", "MR12A", "MR11B", "MR12B", "MR11C", "MR12C"


	d2b <- data.frame(
		rep = r2b[["24-hour recall round (main or repeat)"]]
	)
##r2b: "DI_ROUND"


	d3a <- data.frame(
		hhid = r3a[["HHID"]],
		Zn_fertilizer = r3a[["ZINC_MG"]]
	)
##r3a: "DROUND", "FAC_ID2", "PID", "PNAME", "SEX", "AGEY", "AGEM", "PERIOD", "PERIOD_DESCR", "FOODTYPE", "FOODTYPE_DESCR", "GCODE", "GDESCR", "FCODE", "FDESCR", "WEIGHT_FOOD", "ENERGY_KCAL", "PROTEIN_G", "LIPID_G", "CARBOHYDRT_G", "FIBER_TD_G", "CALCIUM_MG", "IRON_MG", "VIT_C_MG", "THIAMIN_MG", "RIBOFLAVIN_MG", "NIACIN_MG", "VIT_B6_MG", "FOLATE_MCG", "FOLIC_ACID_MCG", "FOOD_FOLATE_MCG", "FOLATE_MCG_DFE", "VIT_B12_MCG", "VIT_A_IU", "VIT_A_MCG_RAE", "RETINOL_MCG", "ALPHA_CAROT_MCG", "BETA_CAROT_MCG", "BETA_CRYPT_MCG", "SERIAL", "ENUMERATOR", "ENUMERATORNAME", "FIELD_SEQ"


	d3b <- data.frame(
		rep = r3b[["24-hour recall round (main or repeat)"]]
	)
##r3b: "DROUND"


	d4a <- data.frame(
		hhid = r4a[["HHID"]],
		Zn_fertilizer = r4a[["ZINC_MG"]]
	)
##r4a: "DROUND", "SN", "FAC_ID2", "PID", "FOODTYPE", "PERIOD", "RECNUM", "RECIPE_CODE", "RECIPE_DESCR", "GCODE", "GDESCR", "FCODE", "FDESCR", "WEIGHT_FOOD", "ENERGY_KCAL", "PROTEIN_G", "LIPID_G", "CARBOHYDRT_G", "FIBER_TD_G", "CALCIUM_MG", "IRON_MG", "VIT_C_MG", "THIAMIN_MG", "RIBOFLAVIN_MG", "NIACIN_MG", "VIT_B6_MG", "FOLATE_MCG", "FOLIC_ACID_MCG", "FOOD_FOLATE_MCG", "FOLATE_MCG_DFE", "VIT_B12_MCG", "VIT_A_IU", "VIT_A_MCG_RAE", "RETINOL_MCG", "ALPHA_CAROT_MCG", "BETA_CAROT_MCG", "BETA_CRYPT_MCG"


	d4b <- data.frame(
		rep = r4b[["24-hour recall round (main or repeat)"]]
	)
##r4b: "DROUND"


	d5a <- data.frame(
		hhid = r5a[["HHID"]],
		Zn_fertilizer = r5a[["ZINC_MG"]]
	)
##r5a: "DROUND", "FAC_ID1", "FAC_NAME", "INTVGP", "INTVGPNAME", "FAC_ID2", "PID", "PNAME", "SEX", "AGEY", "AGEM", "WEIGHT_FOOD", "ENERGY_KCAL", "PROTEIN_G", "LIPID_G", "CARBOHYDRT_G", "FIBER_TD_G", "CALCIUM_MG", "IRON_MG", "VIT_C_MG", "THIAMIN_MG", "RIBOFLAVIN_MG", "NIACIN_MG", "VIT_B6_MG", "FOLATE_MCG", "FOLIC_ACID_MCG", "FOOD_FOLATE_MCG", "FOLATE_MCG_DFE", "VIT_B12_MCG", "VIT_A_IU", "VIT_A_MCG_RAE", "RETINOL_MCG", "ALPHA_CAROT_MCG", "BETA_CAROT_MCG", "BETA_CRYPT_MCG", "ENUMERATOR", "ENUMERATORNAME", "INTD", "INTM", "INTY", "DAYOFWEEK", "FIELD_SEQ"


	d5b <- data.frame(
		rep = r5b[["24-hour recall round (main or repeat)"]]
	)
##r5b: "DROUND"


	d6a <- data.frame(
		hhid = r6a[["HHID"]]
	)
##r6a: "FAC_ID1", "FAC_NAME", "FAC_ID2", "INTVGP", "INTVGPNAME", "DIST", "DISTNAME", "LOCID", "LOCNAME", "SLOC_ID", "SLOCNAME", "VILL_ID", "VILLNAME", "MATERNAL_ID", "MOTHER_FNAME", "MOTHER_LNAME", "DB", "MB", "YB"


	d6b <- data.frame(
		hhid = r6b[["HHID"]]
	)
##r6b: "HOUSEHOLD IDENTIFICATION NUMBER"


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

