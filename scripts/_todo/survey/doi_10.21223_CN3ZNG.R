# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: The Viable Sweetpotato Technologies in Africa (VISTA)-Mozambique: Endline survey for Nampula and Zambezia Provinces

The endline survey for Viable Sweetpotato Technologies in Africa (VISTA) project in Mozambique was conducted in November-December, 2018 in 15 districts: 11 in  Nampula province (11 districts in Nampula Province (Nampula City, Rapale, Murrupula, Malema, Mecuburi, Meconta, Monapo, Mogovolas, Moma, Larde and Angoche) and 4 in Zambezia Province (Alto Molocue, Gurue, Mocuba and Gile). The survey covered 1, 538 households from these 15 districts. A multi-stage sampling procedure was used in selecting households for the study. The first stage involved stratifying the 15 districts into two strata: the pilot districts that were first to be exposed to the intervention from 2014 and the expansion districts that were exposed from 2016. The second stage involved the random selection of the intervention and control villages from the list that was identified by the project staff and local agriculture and health extension officers at the beginning of the project. Once the villages were selected, the households from these villages who were among the 2,955 households interviewed during a baseline survey in 2015 and follow-up survey in 2017 were listed to create as the sampling frame from which the 1,538 households for the 2018/19 survey were randomly selected. This way, the households interviewed in previous surveys were revisited in the 2018/19 survey providing the opportunity to understand farmer experiences with OFSP and adoption, retention and dis-adoption patterns.  The number of villages per district and households per village was proportional to size. A structured household survey was used for data collection. The questionnaire had several modules including household socio-demographic characteristics; crop production and sales; knowledge, attitudes and practices on sweetpotato; preferred attributes for sweetpotato varieties; growing of OFSP in previous and current seasons; nutrition and vitamin A knowledge; intake of vitamin A rich foods including OFSP, dietary diversity and food security status.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/CN3ZNG"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Household; universe: Children under 5, above 5, adult Women and men; dataCollector: Temesgen Bocher and Miguel Arlindo; samplingProcedure: A total of 1,538 households were interviewed  during this survey. A multi-stage sampling procedure was used in selecting households for the study. The first stage involved stratifying the 15 districts into two strata: the pilot districts that were first to be exposed to the intervention from 2014 and the expansion districts that were exposed from 2016. The second stage involved the random selection of the intervention and control villages from the list that was identified by the project staff and local agriculture and health extension officers at the beginning of the project. Once the villages were selected, the households from these villages who were among the 2,955 households interviewed during a baseline survey in 2015 and follow-up survey in 2017 were listed to create as the sampling frame from which the 1,538 households for the 2018/19 survey were randomly selected. This way, the households interviewed in previous surveys were revisited in the 2018/19 survey providing the opportunity to understand farmer experiences with OFSP and adoption, retention and dis-adoption patterns.  The number of villages per district and households per village was proportional to size.; collectionMode: In person interview interviews using a structured household questionnaire.; researchInstrument: Structured household questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "Data dictionary-VISTA-Mozambique-HHsurvey_2018.xlsx"]
	f2 <- ff[basename(ff) == "VISTA_2018_survey_dataset_cleaned.dta"]

	r1 <- carobiner::read.excel(f1)
	r2 <- haven::read_dta(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Variable name", "Variable synonym", "Variable label", "Method", "Formula", "Unit/Measure", "Reference"


	d1 <- data.frame(
		hhid = r1[["hhid"]],
		year = r1[["year"]],
		country = r1[["country"]],
		season = r1[["OFSPseasons_num"]],
		crop = tolower(r1[["n_crops_sold"]]),
		N_fertilizer = r1[["n_crops_sold"]]
	)
##r1: "ad1", "ad2", "ad3", "ad4", "m02_14a", "m02_14c", "m02_15", "m02_17a", "m02_17c", "m02_10", "dvm", "sex", "age", "hhsize", "educ", "size_arm", "sizefarmupl", "sizevalley", "m0603", "m0604a", "m0604", "m0605", "m06_woman_01", "m06_woman_02", "m06_woman_03", "m06_woman_04", "m06_woman_05", "m06_woman_06", "m06_woman_07", "m06_woman_08", "m06_woman_09", "m06_woman_10", "m06_woman_11", "m06_woman_12", "m06_woman_13", "m06_woman_14", "m06_woman_15", "m06_woman_16", "m06_woman_17", "m06_woman_18", "m06_woman_19", "m06_woman_20", "m06_child_01", "m06_child_02", "m06_child_03", "m06_child_04", "m06_child_05", "m06_child_06", "m06_child_07", "m06_child_08", "m06_child_09", "m06_child_10", "m06_child_11", "m06_child_12", "m06_child_13", "m06_child_14", "m06_child_15", "m06_child_16", "m06_child_17", "m06_child_18", "m06_child_19", "m06_child_20", "m06_21a", "m06_21b", "m06_22", "m06_24", "m07_01a", "m07_01b", "m07_02a", "m07_02b", "m07_03a", "m07_03b", "m07_04a", "m07_04b", "m07_05a", "m07_05b", "m07_06a", "m07_06b", "m07_07a", "m07_07b", "m07_08a", "m07_08b", "m07_09a", "m07_09b", "m07_10a", "m07_10b", "m07_11a", "m07_11b", "m07_12a", "m07_12b", "m07_13a", "m07_13b", "m07_14a", "m07_14b", "m07_15a", "m07_15b", "m07_16a", "m07_16b", "m07_17a", "m07_17b", "m07_18a", "m07_18b", "m07_19a", "m07_19b", "m07_20a", "m07_20b", "m07_21a", "m07_21b", "m07_22a", "m07_22b", "m07_23a", "m07_23b", "m07_24a", "m07_24b", "m07_25a", "m07_25b", "m07_26a", "m07_26b", "m07_27a", "m07_27b", "m07_28a", "m07_28b", "m07_29a", "m07_29b", "m07_30a", "m07_30b", "m07_31a", "m07_31b", "m07_32a", "m07_32b", "m07_33a", "m07_34a", "m07_34b", "m07_34c", "m07_34d", "m07_36a", "m07_36b", "m07_36c", "m07_36d", "m07_37", "final_note", "m02_06", "ad3_a", "ad5_a", "m02_08a", "m02_11", "m02_13a", "age_in_years_hh", "m02_14b", "m02_16c", "age_in_year_ref_wom", "m02_17b", "m02_19a", "m02_19b", "m02_19c", "m02_19e", "m02_19f", "m02_21a", "m02_21b", "m02_21c", "m02_22a", "m02_22b", "m02_22c", "m02_23a", "m02_23b", "m02_23c", "m02_23d", "m02_23e", "m02_23f", "m02_24a", "m02_24b", "m02_24c", "m02_25", "m02_30", "m02_30b", "m02_31a", "m02_31b", "m02_31c", "m02_31d", "m02_32", "m02_32b", "m02_33", "m02_33a", "m02_34", "get_gps", "child_id_1", "child_id_2", "child_id_3", "child_id_4", "m02_c3_1", "m02_c3_2", "m02_c3_3", "m02_c3_4", "child_dob_1", "child_dob_2", "child_dob_3", "child_dob_4", "m02_c7_1", "m02_c7_2", "m02_c7_3", "m02_c7_4", "ref_child_age_in_days_1", "ref_child_age_in_days_2", "ref_child_age_in_days_3", "ref_child_age_in_days_4", "ref_child_valid_1", "ref_child_valid_2", "ref_child_valid_3", "ref_child_valid_4", "m02_c8_1", "m02_c8_2", "m02_c8_3", "m02_c8_4", "m02_c9_1", "m02_c9_2", "m02_c9_3", "m02_c9_4", "m02_c10_1", "m02_c10_2", "m02_c10_3", "m02_c10_4", "m02_c11_1", "m02_c11_2", "m02_c11_3", "m02_c11_4", "m02_c16", "m02_c16a", "m02_c17", "m02_c17a_where_gave_birth_other", "m02_c18_checkbox", "m02_c19", "m02_c20", "m02_c21", "m02_c21a", "m02_c21_date", "m02_c22", "m02_c23", "m02_c24", "m02_c25", "m02_c26", "m02_c27", "m02_c28", "m02_c29", "m02_c29a", "m02_c30", "m02_c31", "m02_c31a", "m02_c31b", "m02_c31c", "m02_c32", "m02_c32a", "m02_c32b", "g01a", "g01b", "g01ba", "g01aa", "g01bb", "g01ab", "g03", "g0a", "g05_01", "g05_02", "g05_03", "g05_04", "g05_05", "g05_06", "g05_07", "g05_08", "g05_09", "g05_10", "g05_11", "g05_12", "g05_13", "g06_01", "g06_02", "g06_03", "g06_04", "g06_05", "g06_06", "g06_07", "g06_08", "g06_09", "g06_10", "g06_11", "g06_12", "g06_13", "g07a", "g07b", "g07c", "gi07d", "g07e", "g07f", "g07g", "g07h", "g07i", "g24k", "g25k", "g26k", "g27k", "g24j", "g25j", "g26j", "g27j", "g24l", "g25l", "g26l", "g27l", "g28_1", "g28_2", "g28_3", "g28_4", "g28_5", "g28_6", "g28_7", "g28_8", "m03_01", "m03_02a", "m03_02b", "m03_02c", "m03_03_01", "m03_03_02", "m03_03_03", "m03_03_04", "m03_03_05", "m03_03_06", "m03_03_07", "m03_03_08", "m03_03_09", "m03_03_10", "m03_03_11", "m03_03_12", "m03_03_13", "m03_03_14", "m03_03p_01", "m03_03p_02", "m03_03p_03", "m03_03p_04", "m03_03p_05", "m03_03p_06", "m03_03p_07", "m03_03p_08", "m03_03p_09", "m03_03p_10", "m03_03p_11", "m03_03p_12", "m03_03p_13", "m03_03p_14", "m03_04", "m03_04a", "m03_04b", "m03_04c", "m03_05a", "m03_05b", "m03_06", "m03_06a", "m03_06b", "m03_06c", "m03_07a", "m03_07b", "m03_07c", "m03_10a", "m03_10b", "m03_10c", "m03_11", "m03_11_1", "m03_11_2", "m03_11_3", "m03_11_4", "m03_11_5", "m03_11_6", "m03_11_7", "m04_04", "m04_05_1", "m04_05_2", "m04_05_3", "m04_05_4", "m04_05_5", "m04_05_6", "m04_06_1", "m04_06_2", "m04_06_3", "m04_06_4", "m04_06_5", "m04_06_6", "m04_07b_1", "m04_07b_2", "m04_07b_3", "m04_07b_4", "m04_07b_5", "m04_07b_6", "m04_08_1", "m04_08_2", "m04_08_3", "m04_08_4", "m04_08_5", "m04_08_6", "m04_09_1", "m04_09_2", "m04_09_3", "m04_09_4", "m04_09_5", "m04_09_6", "m04_10_1", "m04_10_2", "m04_10_3", "m04_10_4", "m04_10_5", "m04_10_6", "m04_11_1", "m04_11_2", "m04_11_3", "m04_11_4", "m04_11_5", "m04_11_6", "m04_12_1", "m04_12_2", "m04_12_3", "m04_12_4", "m04_12_5", "m04_12_6", "m04_13_1", "m04_13_2", "m04_13_3", "m04_13_4", "m04_13_5", "m04_13_6", "m04_19a_1", "m04_19a_2", "m04_19a_3", "m04_19a_4", "m04_19a_5", "m04_19a_6", "m04_19b_1", "m04_19b_2", "m04_19b_3", "m04_19b_4", "m04_19b_5", "m04_19b_6", "m04_19c_1", "m04_19c_2", "m04_19c_3", "m04_19c_4", "m04_19c_5", "m04_19c_6", "m04_19d", "m04_21_01", "m04_21_02", "m04_21_03", "m04_21_04", "m04_21_05", "m04_21_06", "m04_21_07", "m04_21_08", "m04_21_09", "m04_21_10", "m04_21_11", "m04_21_12", "m04_21_13", "m04_21_14", "m04_21_15", "m04_21_16", "m04_21_17", "m04_21_18", "m04_21_19", "m04_21_20", "m04_22", "n02", "n03", "n04", "n05", "n06", "n07", "n08", "n09", "n10", "n11", "n12", "n13", "n14", "n15", "n16", "n17", "n18", "n19", "n20", "n21a", "n22a", "n23", "n24", "n25", "n26", "n27_1", "n27_2", "n27_3", "n27_4", "n27_5", "n27_6", "n27_7", "n28", "n29", "n33", "n34", "n35_1", "n35_2", "n35_3", "n35_4", "n35_5", "n36_1", "n36_2", "n36_3", "n36_4", "n36_5", "n37", "n38", "n39", "n40", "n40a", "j01", "j03", "j04_1", "j04_2", "j04_3", "j04_4", "j04_5", "j04_6", "j04_7", "j04_8", "j05_1", "j05_2", "j05_3", "j05_4", "j06", "j07", "j08", "j09", "j10_01", "j10_02", "j10_03", "j10_04", "j10_05", "j10_06", "j10_07", "j10_08", "j10_09", "j10_10", "j10_11", "j10_12", "j10_13", "j10_14", "j10_15", "j10_16", "j10_17", "j10_18", "j10_19", "j10_20", "j10_21", "j10_22", "j10_23", "j10_24", "k01_1", "k01_2", "k01_3", "k01_4", "k01_5", "k01_6", "k01_7", "k01_8", "k01_9", "k01_10", "k01_11", "k01_12", "k01_13", "k01_14", "k01_15", "k01_16", "k01_17", "k01_18", "k01_19", "k01_20", "k06_11", "e01", "e01a", "e01b", "e01c", "e02", "e03_b", "e04", "e04a", "e04b", "e04c", "e05d", "e05e", "e05f", "e06_01", "e06_02", "e06_03", "e06_04", "e06_05", "e06_06", "e06_07", "e06_08", "e06_09", "e06_10", "e06_11", "e06_12", "e06_13", "e06_14", "e08", "e09", "e10", "e11", "e13a", "e13b", "e14a", "e14b", "e15a", "e15b", "e16a", "e16b", "e17", "e18", "e20_1", "e20_2", "e20_3", "e20_4", "e20_5", "e20_6", "e20_7", "e21_1", "e21_2", "e21_3", "e21_4", "e21_5", "e21_6", "e21_7", "e22_1", "e22_2", "e22_3", "e22_4", "e22_5", "e22_6", "e22_7", "m05_01a", "m05_01b", "m05_02a", "m05_02b", "m05_03a", "m05_03b", "m05_04a", "m05_04b", "m05_05a", "m05_05b", "m05_06a", "m05_06b", "m05_07a", "m05_07b", "m05_08a", "m05_08b", "m05_09a", "m05_09b", "m05_10_01", "m05_10_02", "m05_10_03", "m05_10_04", "m05_10_05", "m05_10_06", "m05_10_07", "m05_10_08", "m05_10_09", "m05_10_10", "m05_10_11", "m05_10_12", "m05_10_13", "m05_11_01", "m05_11_02", "m05_11_03", "m05_11_04", "m05_11_05", "m05_11_06", "m05_11_07", "m05_11_08", "m05_11_09", "m05_11_10", "m05_11_11", "m05_11_12", "m05_12_01", "m05_12_02", "m05_12_03", "m05_12_04", "m05_12_05", "m05_12_06", "m05_12_07", "m05_12_08", "m05_12_09", "m05_12_10", "m05_12_11", "m05_12_12", "m05_12_13", "m0601a", "m0602a", "m06_02_checkbox", "m06_03_checkbox", "m01", "m02", "m03", "m04", "m05", "m06", "m07", "m08_a", "m09", "m10_a", "m11", "m12", "m03_2_01a", "m03_2_01b", "m03_2_02a", "m03_2_02b", "m03_2_03a", "m03_2_03b", "m03_2_04a", "m03_2_04b", "t_rg", "ofspgrow15", "ofspgrow16", "ofspgrow17", "ofspgrow18", "ofsp_prevseas_grown", "OFSPret0", "OFSPret1", "lowlandaccess", "Nampulacity", "Angoche", "Malema", "Meconta", "Mecuburi", "Mogovolas", "Moma", "Monapo", "Murrupula", "Rapale", "AltoMolocue", "Gile", "Gurue", "Mocuba", "Larde", "KnowVitA", "distexposure"


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

