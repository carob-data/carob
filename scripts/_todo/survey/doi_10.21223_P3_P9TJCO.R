# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: GROWING Baseline Survey (2022/2023)

This dataset presents baseline data collected by GROWING, which is being implemented in Northern Ghana by the International Potato Centre (CIP) and CARE International. The baseline data was collected in November 2022 and February 2023. The primary purpose of the dataset is to establish pre-intervention reference conditions for the project's key intervention areas. The project was designed to implement integrated interventions, including agriculture, nutrition, women's empowerment, marketing, financial inclusion, agro-processing, and small livestock rearing. Therefore, the baseline questionnaire was developed with a focus on the project's key performance indicators (KPIs) drawn from this implementation sector. The main purpose of the baseline data collection was to establish pre-intervention conditions for project participants, serve as a benchmark for annual performance monitoring and a point of comparison for longitudinal impact assessment.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/P9TJCO"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "International Potato Center, Tamale, Ghana; International Potato Center, Nairobi, Kenya",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: The primary unit of analysis for the GROWING Baseline Survey is the household; universe: The universe of the study consisted of:

Eligible households with mothers/caregivers living in selected communities within the six GROWING project districts in Northern Ghana.
The study covered:
6 districts and 180 communities, the Communities classified by:
irrigation potential (high/low)
treatment/control status
Eligible participants were identified through a household listing exercise.
From each community, eligible mothers/caregivers representing households were selected for the survey.; samplingProcedure: We used a multistage sampling to select households for participation in the survey. First, the regions were purposefully selected due to high levels of poverty, food insecurity, and gender inequity. Second, two districts in each of these regions were selected for project interventions. Third, within the six resulting districts, the 165 Community-based Health Planning and Services (CHPS) zones were classified as CHPS zones with high (47 in total) or low irrigation potential (118 in total) based on informant knowledge. Fourth, 36 CHPS zones were randomly selected from each of the two categories, for a total of 72 CHPS zones. Fifth, within each category (high or low irrigation potential), 18 CHPS zones were randomly assigned to treatment and 18 to a control group. Sixth, 180 communities were randomly selected for each combination of irrigation/non-irrigation and treatment/control groups. 
In each community, a listing exercise was applied to identify 40 eligible mothers/caregivers (each representing one household), of whom 10 were randomly selected to participate in the baseline survey. The total sample size is 1,800 households, of whom 1,080 are in the treatment group that receives the intervention in cycles 1 or 2, and 720 are in the control group that will participate in the intervention after an endline survey. Power calculations were used to determine a sample size of 1,800 households for this study to achieve the minimum detectable effect (MDE) set by the GROWING project team. The study is longitudinal, with the expectation that the same households will be revisited for the endline assessment.; collectionMode: Data were collected using structured household questionnaires: a census to list eligible participants and households; a household-level questionnaire; an anthropometric questionnaire; and a women's empowerment questionnaire (Pro-WEAI). These questionnaires were field-tested and digitised using Open Data Kit (ODK) and the ONA server for digital data collection. Face-to-face interviews were conducted using trained enumerators and field team supervisors. The project's monitoring and evaluation included field monitoring throughout baseline data collection and frequent quality checks. After data collection, baseline data were quality-checked, thoroughly cleaned, and processed. The analysis was carried out using descriptive statistics to compare variables of interest between the six GROWING districts and inferential statistics to compare differences in means of selected key variables, using STATA version 17. The baseline report from the analysis is available here on the CGSpace.",
		
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
	

	f1 <- ff[basename(ff) == "GROWING ANTHROPOMETRY Questionnaire Dictionary.xlsx"]
	f2 <- ff[basename(ff) == "GROWING Household Survey Questionnaire Dictionary.xlsx"]
	f3 <- ff[basename(ff) == "GROWING Pro-WEAI Survey Questionnaire Dictionary.xlsx"]
	f4 <- ff[basename(ff) == "C01bs_overview_nopii.dta"]
	f5 <- ff[basename(ff) == "C02bs_agriculture_incomge_generating_activity_nopii.dta"]
	f6 <- ff[basename(ff) == "C02bs_agriculture_nopii.dta"]
	f7 <- ff[basename(ff) == "C02bs_household_assets_Table_clean_NoPII.dta"]
	f8 <- ff[basename(ff) == "C02bs_m015_1_Crops_Details_Table_nopii.dta"]
	f9 <- ff[basename(ff) == "C02bs_m015_3_Crops_Revenue_Table_nopii.dta"]
	f10 <- ff[basename(ff) == "C02bs_m019_11_Crops_Marketing_Challenges_Table_nopii.dta"]
	f11 <- ff[basename(ff) == "C02bs_m04_04_SP_Production_Table_nopii.dta"]
	f12 <- ff[basename(ff) == "C02bs_m19_1_Agric_Services_Table_nopii.dta"]
	f13 <- ff[basename(ff) == "C02bs_m19_2_Post_Harvest_Services_Table_nopii.dta"]
	f14 <- ff[basename(ff) == "C03bs_nutrition_nopii.dta"]
	f15 <- ff[basename(ff) == "C04bs_empowered_nopii.dta"]
	f16 <- ff[basename(ff) == "C04bs_pro_weai_clean_nopii.dta"]
	f17 <- ff[basename(ff) == "C05bs_Attitudes towards gender equity_nopii.dta"]
	f18 <- ff[basename(ff) == "C06bs_Nutritional knowledge_nopii.dta"]
	f19 <- ff[basename(ff) == "GROWING Baseline Data - nopii.rar"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- haven::read_dta(f4)
	r5 <- haven::read_dta(f5)
	r6 <- haven::read_dta(f6)
	r7 <- haven::read_dta(f7)
	r8 <- haven::read_dta(f8)
	r9 <- haven::read_dta(f9)
	r10 <- haven::read_dta(f10)
	r11 <- haven::read_dta(f11)
	r12 <- haven::read_dta(f12)
	r13 <- haven::read_dta(f13)
	r14 <- haven::read_dta(f14)
	r15 <- haven::read_dta(f15)
	r16 <- haven::read_dta(f16)
	r17 <- haven::read_dta(f17)
	r18 <- haven::read_dta(f18)
	#r19 <- read.???(f19)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Variable name", "Description", "type", "Values/Units"


	d2 <- data.frame()
##r2: "Variable name", "Description", "Type", "Values/Units"


	d3 <- data.frame()
##r3: "Variable name", "Description", "type", "Values/Units"


	d1 <- data.frame(
		hhid = r1[["hhid"]],
		adm1 = carobiner::fix_name(r1[["region"]], "title"),
		adm2 = carobiner::fix_name(r1[["district"]], "title"),
		adm3 = carobiner::fix_name(r1[["community_name"]], "title"),
		treatment = r1[["treatment_status"]]
	)
##r1: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "wgt", "fpc_chps", "fpc_vill", "fpc_hh", "man_head_hh", "woman_head_hh", "age_hoh", "pre_school_hoh", "basic_education_hoh", "secondary_education_hoh", "tertiary_education_hoh", "non_formal_hoh", "never_attended_school_hoh", "educ_ref_woman", "pre_school_ref_woman", "basic_education_ref_woman", "secondary_education_ref_woman", "tertiary_education_ref_woman", "non_formal_ref_woman", "never_attended_school_ref_woman", "age_ref_women", "family_size", "dep_ratio", "working_pop", "non_working_pop", "male_female_ratio", "salaried_job_man", "salaried_job_woman", "land_area", "wealth_index_score", "wealth_index", "asset_index_score", "asset_index", "agric_access_score", "cond_enough_food", "cond_not_prefer", "cond_limit_var", "cond_not_want", "cond_small_meal", "cond_fewer_meal", "cond_no_food", "cond_sleep_hungry", "cond_day_night", "hfiap_secure", "hfiap_mildly", "hfiap_moderate", "hfiap_severe", "hfias", "M14_01Airconditioners", "M14_01Axes", "M14_01Bicycle", "M14_01Biogas", "M14_01Boreholeorwell", "M14_01Cartruck", "M14_01Cart", "M14_01Chaffcutterforfeed", "M14_01Cows", "M14_01Desktopcomputers", "M14_01Dripirriequip", "M14_01Droughtanimaldonkeyh", "M14_01Fans", "M14_01Gascooker", "M14_01Generator", "M14_01Goats", "M14_01Harrowtillerridgerw", "M14_01Hoe", "M14_01Ironselectricorbox", "M14_01Laptopcomputers", "M14_01macheteslashers", "M14_01Mechanicalwaterpump", "M14_01Milkingequipment", "M14_01Motorcyle", "M14_01Motorizedwaterpump", "M14_01Otheragroprocequ", "M14_01Otherirriequip", "M14_01Parabolicsatellites", "M14_01Pigs", "M14_01Plough", "M14_01Poshomill", "M14_01Poultry", "M14_01Rabbits", "M14_01Radiocassetteplayer", "M14_01Refrigerator", "M14_01Saw", "M14_01Sewingknitmach", "M14_01Sheep", "M14_01Sheller", "M14_01Sickle", "M14_01Solarpanels", "M14_01Spadeshovel", "M14_01Spraypumpbackpack", "M14_01Storagefacility", "M14_01TelephoneMobile", "M14_01Tractor", "M14_01Tv", "M14_01Washmachinedryer", "M14_01Watch", "M14_01WaterTank", "M14_01Wateringcan", "M14_01Weighingmachinescale", "M14_01Wheelbarrow"


	d2 <- data.frame(
		country = r2[["country"]],
		adm1 = carobiner::fix_name(r2[["region"]], "title"),
		adm2 = carobiner::fix_name(r2[["district"]], "title"),
		adm3 = carobiner::fix_name(r2[["community_name"]], "title"),
		treatment = r2[["treatment_status"]],
		hhid = r2[["hhid"]],
		crop = tolower(r2[["processing_nut_crop"]])
	)
##r2: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "community_irrigation_status", "Q170", "Q170a", "Q170b", "Q171", "Q171s", "Q172s", "Q174s", "Q175s", "Q176s", "woman_sale_revenue", "Q176w", "Q176s_revenue_check", "Q177s", "Q171w", "Q172w", "Q174w", "Q175w", "Q176w_revenue_check", "Q177w", "Q1710", "Q1711", "Q1711a", "Q1711b", "Q1711c", "Q1711c_revenue_check", "Q1712", "Q172s_encode", "Q1711_encode", "rice_processing", "animal_product", "sale_ingredients", "sheabutter", "sale_meals_snacks", "processing_nut_crop2", "rice_processing2", "animal_product2", "sale_ingredients2", "sheabutter2", "sale_meals_snacks2", "any_income_generating", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


	d3 <- data.frame(
		country = r3[["country"]],
		adm1 = carobiner::fix_name(r3[["region"]], "title"),
		adm2 = carobiner::fix_name(r3[["district"]], "title"),
		adm3 = carobiner::fix_name(r3[["community_name"]], "title"),
		treatment = r3[["treatment_status"]],
		hhid = r3[["hhid"]],
		year = r3[["m02_14a_year_hhh_born"]],
		crop = tolower(r3[["cultivate_nutritious_crop"]]),
		variety = r3[["moringa_varietiesLocal_variety"]],
		season = r3[["_16_3C_Length_of_dry_season"]]
	)
##r3: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "community_irrigation_status", "m02_06", "m02_08a", "m02_08b", "m02_08c", "m02_08", "m02_11", "m02_14a", "m02_14b", "m02_14c", "m02_15", "m02_17a", "m02_17a_check_yearborn", "m02_17b", "m02_17c", "m02_20", "m02_21a", "m02_21b", "m02_21c", "m02_22a", "m02_22b", "m02_22c", "m02_23a", "m02_23b", "m02_23c", "m02_24a", "m02_24b", "m02_24c", "m02_25", "m02_26", "m02_28a", "m02_28b", "m02_28c", "m02_28", "m02_29", "m02_30", "m02_30b", "m02_31a", "m02_31b", "m02_32a", "m02_32b", "m02_33a", "m02_33b", "m02_34a", "m02_34b", "m03_01", "m03_02a", "m03_02b", "m03_02c", "m03_02d", "m03_02e", "m03_02f", "m03_02g", "m03_02h", "m03_02i", "m03_02j", "m03_02k", "m03_02l", "m03_02m", "m03_02n", "m03_03a", "m03_03b", "m03_03c", "m03_04", "m03_05a", "m03_05b", "m03_06a", "m03_06b", "m03_06c", "m03_03check", "m03_07a", "m03_07b", "m03_07c", "m03_07d", "m03_07e", "m03_07f", "m03_07g", "m03_07h", "m03_07i", "m03_07j", "m03_08b", "m03_08a", "m03_08c", "m03_09b", "m03_09a", "m03_09c", "m03_10a", "m03_10b", "m03_10c", "m03_11", "m03_11a", "m03_11b", "m03_11c", "m03_11d", "m03_11e", "m03_11f", "m03_11g", "m03_11h", "m03_12", "m03_13a", "m03_13b", "m03_13c", "m03_13d", "roots_total_1yr", "ROOTS_1YR_COUNT_PROMT", "m03_14a", "m03_14b", "m03_14c", "m03_14d", "roots_total_3yr", "ROOTS_3YR_COUNT_PROMT", "m03_15a", "m03_15b", "m03_16a", "m03_16b", "m03_17a", "m03_17b", "m03_18a", "m03_18b", "m04_01", "m04_02a", "m04_02b", "m04_02", "m04_03a", "m04_03b", "m04_03", "m04_04a", "m04_04b", "m04_04c", "m04_04d", "m04_04", "RPT_PRODUCTION_SALES_count", "m04_20", "m04_21a", "m04_21b", "m04_21c", "m04_21d", "m04_21e", "m04_21f", "m04_21g", "m04_21h", "m04_21i", "m04_21j", "m04_21k", "m04_21l", "m04_21m", "m04_21n", "m04_21o", "m04_21p", "m04_21q", "m04_21r", "m04_21s", "m04_21t", "m04_21u", "m04_21v", "m04_22", "m04_23a", "m04_23b", "m04_24a", "m04_24b", "cultivate_nutritious_cropSoybea", "cultivate_nutritious_cropGround", "cultivate_nutritious_cropAmaran", "crop_amaranth_irrigate", "cultivate_nutritious_cropNone", "soyaebean_varieties", "soyaebean_varietiesLocal_variet", "soyaebean_varietiesJenguma", "soyaebean_varietiesFavour", "soyaebean_varietiesAfayak", "soyaebean_varietiesDontknow", "groundnut_varieties", "groundnut_varietiesLocal_variet", "groundnut_varietiesSarinut_2", "groundnut_varietiesChinese", "groundnut_varietiesYenyawoso", "groundnut_varietiesManipinta", "groundnut_varietiesDontknow", "amaranth_varieties", "amaranth_varietiesAmaranth_loca", "amaranth_varietiesAmaranth_impr", "amaranth_varietiesDontknow", "crops_details1_count", "grow_nutrious_plants", "grow_nutrious_plantsPapaya", "grow_nutrious_plantsMoringa", "grow_nutrious_plantsNone", "moringa_varieties", "moringa_varietiesImproved_varie", "moringa_varietiesDontknow", "papaya_varieties", "papaya_varietiesSolo", "papaya_varietiesVega", "papaya_varietiesRed_Royale", "papaya_varietiesSinta", "papaya_varietiesDontknow", "crops_details2_count", "earn_revenue_crops", "earn_revenue_cropsSoyabeans", "earn_revenue_cropsGroundnuts", "earn_revenue_cropsAmaranthrai", "earn_revenue_cropsAmaranthirr", "earn_revenue_cropsPapaya", "earn_revenue_cropsMoringa", "earn_revenue_cropsNone", "Revenues_from_crops_count", "_15_4a_seen_cavies", "_15_4b_rear_cavies", "_15_4c_when_start_cavies", "_15_4d_managing_cavies", "_15_4e1_cavies_consumption", "_15_4e2_sale_for_income", "_15_4e3_for_fun", "_15_4e4_other", "abandon_cavy_rearing", "since_abandoned", "Elderly_man", "Youth", "Elderly_woman", "No_difference_gender", "_16_1_Do_you_think_the_climate", "_16_2_Where_did_you_learn_and_u", "on_radio_tv", "personal_observation", "community_sensitization", "extension", "other_specify", "_16_2A_Specify", "_16_3A_Temperature", "_16_3B_Rainfall_amount", "_16_3D_onset_of_rain_late", "_16_3E_Cessation_of_the_rainfall", "_16_3F_Frequency_of_droughts", "_16_3G_Flooding_events", "_16_3H_Harmattan", "_16_3I_Soil_fertility", "_16_3J_trends_of_natural_forest", "_13_33a_radio", "_13_33b_tv", "_13_33c_communit_meeting", "_13_33d_fair", "_13_33e_extention", "_13_33f_friend", "_13_33g_other", "_16_4A_Caused_by_unkn_natural_oc", "_16_4B_Caused_by_the_gods", "_16_4C_Caused_by_deforestation", "_16_4D_Caused_by_bush_burning", "_16_4E_Caused_by_bad_griculture_", "_16_4F_Caused_by_emis_ustries_an", "_16_4G_Others_specify", "_16_5_Do_you_know_of_ture_CSA_pr", "_16_5_if_yes_csa", "_16_6A_Growing_early_uring_crop_", "_16_6B_Application_of_soybeans_w", "_16_6C_Application_of_compost_an", "_16_6D_Application_of_crop_resid", "_16_6E_Rotation_in_co_and_this_y", "_16_6F_Same_season_cr_crops_such", "_16_6G_Dry_season_irrigation", "_16_6H_Applying_backy_chniques_e", "_16_6I_Use_of_riverbe_or_recessi", "_16_6J_Use_of_aflasaf_o_prevent_", "_16_6K_Application_of_f_pests_an", "_16_6L_Use_of_improve_c_storage_", "_16_6M_Tiple_s", "_16_6N_Double_s", "_16_6O_others_specify", "m19_1a", "m19_1a1", "m19_1a1Improvedseedvariety", "m19_1a1ChemicalFertilizer", "m19_1a1Compostorotherorganic", "m19_1a1Biologicalfertilizeri", "m19_1a1Pesticidesorherbicide", "m19_1a1Harvestingorthreshing", "m19_1a1Irrigationpump", "m19_1a1otherSpecify", "service_type_count", "ma19_2", "m19_2", "m19_2Groundnut", "m19_2Soybeans", "m19_2Amranth", "m19_2Sweetpotato", "m19_2Other", "m19_2_other", "crop_type_count", "m19_3", "m19_3a", "m19_3aGovernmentextensionagen", "m19_3aNGOextensionagent", "m19_3aOthersspecify", "m19_3a_other", "extention_services_count", "m19_3d_ext_other", "m19_3e_ext_other", "m19_4_1", "m19_4_2", "m19_4_3", "m19_4_4", "m19_4_5", "m19_4_6", "m19_4_7", "m19_4_8", "m19_4_9", "m19_4_10", "m19_4_9_other", "m19_5_1", "m19_5_2", "m19_5_3", "m19_5_4", "m19_5_5", "m19_5_6", "m19_5_6_1", "m19_6a", "m19_6b", "m19_6c", "m19_6d1", "m19_6d2", "m19_6d3", "m19_6d4", "m19_6d5", "m19_6d6", "m19_6d7", "m19_6e", "m19_6e1", "m19_6e2", "m19_6e3", "m19_6e4", "m19_6e5", "m19_6e6", "m19_6e7", "m19_6f", "m19_6f1", "m19_6f2", "m19_6f3", "m19_6f4", "m19_6f5", "m19_6f6", "m19_6f7", "m19_6g", "m19_6h", "m19_6i", "m19_6i1", "m19_6i2", "m19_6i3", "m19_6i4", "m19_6i5", "m19_6i7", "m19_6i8", "m19_6i6", "m19_6i6a", "m19_6j", "m19_6j1", "m19_6j2", "m19_6j3", "m19_6j4", "m19_6j5", "m19_6j6", "m19_6j6a", "m19_6k", "m19_6k1", "m19_6l", "m19_6l1", "m19_6l1Maxibag", "m19_6l1Olonka", "m19_6l1Standardscales", "m19_6l1Othersspecify", "m19_6l1a", "m19_6m", "m19_7a_vsla_member", "m19_7a1_no", "m19_7a2_no", "m19_7a3_reason_not_member", "m19_7a3_reason_not_member_specif", "m19_7b_times_attend", "m19_7c_access_financial", "m19_7d_savings", "M197E1", "M197E2", "M197E3", "m19_7f_vsla_improvement", "m19_7g1", "m19_7g2", "m19_7g3", "m19_7g4", "m19_7g5", "m19_7g6", "m19_7g7", "m19_7g8", "m19_7g9", "m19_7g10", "m19_7g11", "m19_7g12", "m19_7g13", "m19_7g13a", "m19_7g13b", "m19_7h1", "m19_7h2", "m19_7h3", "m19_7h4", "m19_7h5", "m19_8a", "m19_8b", "m19_8c", "savings_check", "m19_8d1", "m19_8d2", "m19_8d3", "m19_8d4", "m19_8d5", "m19_8d6", "m19_8d7", "m19_8d8", "m19_8d9", "m19_8d10", "m19_8d10b", "m19_8e", "m19_9a", "m19_9b1", "m19_9b2", "m19_9b3", "m19_9b4", "m19_9b5", "m19_9c", "m19_9d", "m19_9e", "m19_10ayesno", "m19_10a1", "m19_10a2", "m19_10a3", "m19_10a4", "m19_10a5", "m19_10b", "m19_10bSoybeans", "m19_10bGroundnut", "m19_10bSweetpotato", "m19_10bAmranth", "m19_10bNoneofthem", "crop_sold1_count", "m1911A1", "m1911A1Soyabeans", "m1911A1Groundnuts", "m1911A1Sweetpotato", "m1911A1Amaranth", "m1911A1Noneofthem", "m1911A1OtherSpecify", "m1911A1_other", "market_challenges_count", "m19_12a", "m19_12b", "m19_12c", "m19_12d", "m19_12e", "m19_13a", "m19_13b", "m19_13c", "m19_13d", "m19_13e", "m19_13f", "m19_13g", "m19_13h", "m19_13i", "m19_13k", "m19_13l", "m19_14a", "m19_14b", "m19_14c", "m19_14d", "m19_14e", "m19_14f", "m19_14g", "m19_14h", "m19_14i", "m19_14i_text", "m19_15a", "m19_15b", "m19_15c", "m19_15d", "m19_15e", "m19_15f", "m19_16a", "m19_16b", "m19_16c", "m19_16d", "m19_16e", "m19_16f", "m19_16g", "m19_14h_other", "m19_16I", "m19_17a", "m19_17b1", "m19_17b2", "m19_17b3", "m19_17b4", "m19_17b5", "m19_18a", "m19_18b", "m19_18c", "m19_18d", "m19_18e", "m19_18f", "Q170", "Q170a", "Q170b", "Q171", "Q171s", "Q172s", "Q174s", "Q175s", "Q176s", "Q176s_revenue_check", "Q177s", "Q178s", "Q171w", "Q172w", "Q174w", "Q175w", "Q176w", "Q176w_revenue_check", "Q177w", "Q179", "Q1710", "Q1711", "Q1711a", "Q1711b", "Q1711c", "Q1711c_revenue_check", "Q1712", "M14_01", "M14_01Airconditioners", "M14_01Axes", "M14_01Bicycle", "M14_01Biogas", "M14_01Boreholeorwellonprope", "M14_01Cartruck", "M14_01Cart", "M14_01Chaffcutterforfeed", "M14_01Cows", "M14_01Desktopcomputers", "M14_01Dripirrigationequipment", "M14_01Droughtanimaldonkeyh", "M14_01Fans", "M14_01Gascooker", "M14_01Generator", "M14_01Goats", "M14_01Harrowtillerridgerw", "M14_01Hoe", "M14_01Ironselectricorbox", "M14_01Laptopcomputers", "M14_01macheteslashers", "M14_01Mechanicalwaterpump", "M14_01Milkingequipment", "M14_01Motorcyle", "M14_01Motorizedwaterpump", "M14_01Otheragroprocessingequ", "M14_01Otherirrigationequipmen", "M14_01Parabolicsatellites", "M14_01Pigs", "M14_01Plough", "M14_01Poshomill", "M14_01Poultrymaturebirdsonl", "M14_01Rabbits", "M14_01Radiocassetteplayer", "M14_01Refrigerator", "M14_01Saw", "M14_01Sewingknittingmachine", "M14_01Sheep", "M14_01Sheller", "M14_01Sickle", "M14_01Solarpanels", "M14_01Spadeshovel", "M14_01Spraypumpbackpack", "M14_01Storagefacilityforcrop", "M14_01TelephoneMobile", "M14_01Tractor", "M14_01Tv", "M14_01Washingmachinesordryer", "M14_01Watch", "M14_01WaterTank", "M14_01Wateringcan", "M14_01Weighingmachinescale", "M14_01Wheelbarrow", "Household_assets_count", "M14_06", "M14_06a", "M14_07", "M14_07a", "M14_08", "M14_08a", "M14_09", "M14_09a", "M14_10", "M14_11", "M14_11a", "M14_12", "M14_12a", "M14_13a", "M14_13b", "M14_14", "M14_14a", "M14_15a", "M14_15b", "M14_16", "M14_16a", "M14_17", "M14_17a", "M14_18", "m05_01a", "m05_01b", "m05_02a", "m05_02b", "m05_03a", "m05_03b", "m05_04a", "m05_04b", "m05_05a", "m05_05b", "m05_06a", "m05_06b", "m05_07a", "m05_07b", "m05_08a", "m05_08b", "m05_09a", "m05_09b", "m05_10a", "m05_10b", "m05_10c", "m05_10d", "m05_10e", "m05_10f", "m05_10g", "m05_10h", "m05_10i", "m05_10j", "m05_10k", "m05_10l", "m05_11a", "m05_11b", "m05_11c", "m05_11d", "m05_11e", "m05_11f", "m05_11g", "m05_11h", "m05_11i", "m05_11j", "m05_11k", "m05_11l", "m05_12a", "m05_12b", "m05_12c", "m05_12d", "m05_12e", "m05_12f", "m05_12g", "m05_12h", "m05_12i", "m05_12j", "m05_12k", "m05_12l", "m05_13", "m0603", "m0604", "m0605", "m06_01a", "m06_01b", "m06_01c", "m06_02a", "m06_02b", "m06_02c", "m06_03a", "m06_03b", "m06_03c", "m06_04a", "m06_04b", "m06_04c", "m06_05a", "m06_05b", "m06_05c", "m06_06a", "m06_06b", "m06_06c", "m06_07a", "m06_07b", "m06_07c", "m06_08a", "m06_08b", "m06_08c", "m06_09a", "m06_09b", "m06_09c", "m06_10a", "m06_10b", "m06_10c", "m06_11a", "m06_11b", "m06_11c", "m06_12a", "m06_12b", "m06_12c", "m06_13a", "m06_13b", "m06_13c", "m06_14a", "m06_14b", "m06_14c", "m06_15a", "m06_15b", "m06_15c", "m06_16a", "m06_16b", "m06_16c", "m06_17a", "m06_17b", "m06_17c", "m06_18a", "m06_18b", "m06_18c", "m06_19a", "m06_19b", "m06_19c", "m06_20a", "m06_20b", "m06_20c", "m06_21a", "m06_21b", "m06_22", "m06_23a", "m06_23b", "m06_23c", "m06_23d", "m06_24", "m06_24a", "m07a", "m07_01a", "m07_01b", "m07_02a", "m07_02b", "m07_03a", "m07_03b", "m07_04a", "m07_04b", "m07_05a", "m07_05b", "m07_06a", "m07_06b", "m07_07a", "m07_07b", "m07_08a", "m07_08b", "m07_09a", "m07_09b", "m07_10a", "m07_10b", "m07_11a", "m07_11b", "m07_12a", "m07_12b", "m07_13a", "m07_13b", "m07_14a", "m07_14b", "m07_15a", "m07_15b", "m07_16a", "m07_16b", "m07_17a", "m07_17b", "m07_18a", "m07_18b", "m07_19a", "m07_19b", "m07_20a", "m07_20b", "m07_21a", "m07_21b", "m07_22a", "m07_22b", "m07_23a", "m07_23b", "m07_24a", "m07_24b", "m07_25a", "m07_25b", "m07_26a", "m07_26b", "m07_27a", "m07_27b", "m07_28a", "m07_28b", "m07_29a", "m07_29b", "m07_30a", "m07_30b", "m07_31a", "m07_31b", "m07_32a", "m07_32b", "m07_33a", "m07_33b", "m07_34a", "m07_34b", "m07_34c", "m07_34d", "m07_35a", "m07_35b", "m07_36a", "m07_36b", "m07_36c", "m07_36d", "m07_37", "m07_37a", "M12_01B", "M12_02", "M12_03", "M12_04", "M12_05", "M12_06", "ML12_07", "ML12_08", "ML12_09", "ML12_10", "ML12_11", "ML12_12", "ML12_13", "ML12_14", "ML12_15", "ML12_16", "ML12_17", "ML12_18", "ML12_19", "M12_20", "M12_21", "M12_22", "M12_22a", "M12_23", "M12_24", "M12_24a", "M12_25", "M12_26", "M12_27", "M12_28", "M12_29", "M12_29a", "M12_30", "M12_31", "M12_32", "M12_321Betteroutput", "M12_322Lessdiseases", "M12_323Easiertocutcut", "M12_324Morevigorousgrowtha", "M12_325Other", "M12_32a", "M12_33", "M12_331Raisedbeds", "M12_332Flatbeds", "M12_333Ridges", "M12_334Mounds", "M12_335Fieldwithoutraising", "name_of_child", "Was_child_ever_breastfed", "_3_How_long_after_birth_was_n", "Hours", "Days", "_4_In_the_first_2_da_or_herbal_t", "_5_Was_name_of_chil_ing_the_day_", "_6_Did_name_of_chil_the_day_or_a", "_7A_Plain_water", "_7b_Infant_formula_such_as_Cer", "_7Bnum_If_Yes_in_7B_ORE_TIMES_RE", "_7c_Fresh_mild_tin_mild_powe", "_7Cnum_If_Yes_in_7C_ORE_TIMES_RE", "_7Cswt_If_Yes_in_7C_ovoured_type", "_7D_Milo", "_7E_Fruit_juice_fruit_drinks_sob", "_7F_Soft_drinks_or_m_ro_or_Malta", "_7G_Tea_coffee_herbal_drink", "_7Gswt_If_Yes_in_7G_the_drink_sw", "_7H_Clear_broth_or_clear_soup", "_7I_Any_other_liquids", "_7Iname_If_Yes_in_7I_hat_were_th", "_7Iswt_If_Yes_in_7I_Was_the_drin", "_8A_Brukina_or_drink_yogurt", "_8Anum_If_Yes_in_8A_kina_or_drin", "_8B_If_Yes_in_8A_Did_name_h", "_8Bswt_If_Yes_in_8B_voured_type_", "_9A_Bread_rice_waa_uo_or_rice_po", "_9B_Kenkey_banku_t_maize_or_Tom_", "_9C_Fufu_gari_koko_ain_or_sweet_", "_9D_Beans_or_bambara_beans", "_10A_Carrots_sweet_at_are_orange", "_10B_Cocoyam_leaves_leaves_or_bo", "_10C_Baobab_leaves_or_West_India", "_10D_Tomatoes_okro_ge_gourd_or_c", "_10E_Sweet_green_pep_cumber_or_m", "_11A_Mango_papaya_r_African_star", "_11B_Orange_or_tangerine", "_11C_Banana_pineapp_elon_apple_o", "_11D_Soursop_coconu_fruit_or_she", "_12A_Cakes_biscuits_n_toogbee_or", "_12B_Toffees_chocol_ce_cream_or_", "_13A_Eggs", "_13B_Cheese_curds_or_wagashi", "_13C_Gizzard_or_liver", "_13D_Sausages_or_corned_beef", "_13E_Beef_goat_sheep_liver_intes", "_13F_Port_grasscutt_rabbit_or_bu", "_13G_Chicken_gizzard_or_Guinea_f", "_13H_Fish_dried_fis_erring_crab_", "_13I_Termites", "_14A_groundnuts_kul_eri_soup_or_", "_14B_Packaged_yellow_to_chips_or", "_14C_Indomie", "_14D_French_fries_f_icken_or_fri", "_14E_Red_palm_oil", "_15A_Any_other_foods", "_15B_What_was_the_food", "_16A_Yesterday_did_rve_burgers_o", "_17A_How_many_times_did_solid", "date_day", "date_month", "date_year", "A_011_ref_child", "A_05_Child_ID", "A_07_gender", "A_08_age_child", "A_09_twin", "A_10_oedema", "A_11_mother_caregiver_ID", "A_13_pregnant", "A_14_if_yes", "A_15_estimated_weight_clothes_wo", "A_16_weight_mother", "A_17_weight_mother_child", "A_18_weight_child", "A_19_child_clothing", "A_20_child_weight_limits", "A_21_mother_height_1", "A_22_mother_height_2", "m02_28a_str", "m02_28b_str", "m02_28d", "m02_28dd", "m02_08a_str", "m02_08b_str", "m02_08d", "m02_08dd", "enddate_ft", "age_months", "_15_4c_when_start_cavies_corr", "_15_4d_manag_cavies_encode", "invid_id", "respondent_id", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


	d4 <- data.frame(
		adm1 = carobiner::fix_name(r4[["region_code"]], "title"),
		adm2 = carobiner::fix_name(r4[["district_code"]], "title"),
		adm3 = carobiner::fix_name(r4[["community_code"]], "title"),
		hhid = r4[["hhid"]]
	)
##r4: "sub_district_code", "chps_zone_code", "cycle", "this_assets_name", "this_assets_label", "M14_02_r", "M14_03_r", "M14_04_r", "M14_05_r", "M14_06_r", "instanceID", "_index"


	d5 <- data.frame(
		adm1 = carobiner::fix_name(r5[["region"]], "title"),
		adm2 = carobiner::fix_name(r5[["district"]], "title"),
		treatment = r5[["treatment_status"]],
		adm3 = carobiner::fix_name(r5[["community_irrigation_status"]], "title"),
		hhid = r5[["hhid"]],
		variety = r5[["this_variety_name"]],
		crop = tolower(r5[["crop_seed_source_r"]]),
		plot_id = r5[["crop_plot_size_r"]],
		season = r5[["crop_how_grow_rSameseasonrota"]],
		yield = r5[["crop_change_yield_r"]]
	)
##r5: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "community_name", "m02_06", "this_variety_label1", "crop_seed_source_rSeedsampling", "crop_seed_source_rPurchasedfro", "crop_seed_source_rPurchasedfor", "crop_seed_source_rExchanged", "crop_seed_source_rBorrowedloa", "crop_seed_source_rGift", "crop_seed_source_rOrganization", "crop_seed_source_rNoseedused", "crop_seed_source_rOtherspecif", "crop_seed_source_other_r", "crop_where_grow_r", "crop_where_grow_rNearhousebac", "crop_where_grow_rFlatlands", "crop_where_grow_rAroundriver", "crop_unit_r", "crop_unit_other", "crop_how_grow_r", "crop_how_grow_rSolecropping", "crop_how_grow_rIntercroppingm", "crop_how_grow_rother", "crop_other_specify_r", "crop_fertilizer_r", "crop_fertilizer_rOrganicmanur", "crop_fertilizer_rInorganicDAP", "crop_fertilizer_rBiofertilizer", "crop_fertilizer_rNouseofany", "crop_planting_month_r", "crop_planting_month_rJanuary", "crop_planting_month_rFebruary", "crop_planting_month_rMarch", "crop_planting_month_rApril", "crop_planting_month_rMay", "crop_planting_month_rJune", "crop_planting_month_rJuly", "crop_planting_month_rAugust", "crop_planting_month_rSeptember", "crop_planting_month_rOctober", "crop_planting_month_rNovember", "crop_planting_month_rDecember", "crop_purpose_r", "crop_when_start_r", "crop_change_coverage_r", "crop_reason_change_r", "crop_change_yield_r_new", "crop_change_yield_r_increase", "crop_change_yield_r_decrease", "crop_change_yield_r_nochange", "crop_manage_r", "plot_size_acres", "Soyabeans", "Groundnuts", "Amaranth_rainfed", "Amaranth_irrigated", "crop_purpose_r_both", "crop_purpose_r_consumption", "crop_purpose_r_income", "crop_change_coverage_r_new", "crop_change_coverage_r_increase", "crop_change_coverage_r_decrease", "crop_change_coverage_r_nochange", "Female", "Male", "Both_equally", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


	d6 <- data.frame(
		adm1 = carobiner::fix_name(r6[["region"]], "title"),
		adm2 = carobiner::fix_name(r6[["district"]], "title"),
		treatment = r6[["treatment_status"]],
		adm3 = carobiner::fix_name(r6[["community_irrigation_status"]], "title"),
		hhid = r6[["hhid"]],
		crop = tolower(r6[["this_revenue_crop_name"]])
	)
##r6: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "community_name", "m02_06", "this_revenue_crop_label", "crop_total_revenue", "crop_total_revenue_check", "who_control_revenue", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


	d7 <- data.frame(
		adm1 = carobiner::fix_name(r7[["region"]], "title"),
		adm2 = carobiner::fix_name(r7[["district"]], "title"),
		treatment = r7[["treatment_status"]],
		adm3 = carobiner::fix_name(r7[["community_irrigation_status"]], "title"),
		hhid = r7[["hhid"]]
	)
##r7: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "community_name", "m02_06", "this_cro_name", "this_cro_label", "m19_11a", "m19_11b", "m19_11c", "m19_11d", "m19_11e", "m19_11f", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


	d8 <- data.frame(
		adm1 = carobiner::fix_name(r8[["region"]], "title"),
		adm2 = carobiner::fix_name(r8[["district"]], "title"),
		treatment = r8[["treatment_status"]],
		adm3 = carobiner::fix_name(r8[["community_irrigation_status"]], "title"),
		hhid = r8[["hhid"]],
		plot_id = r8[["plot_num"]]
	)
##r8: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "community_name", "m02_06", "m04_05", "m04_06", "m04_07a", "total_area_ha", "m04_07a_meter", "m04_07a_acre", "m04_07a_ha", "m04_07a_are", "m04_07a_timad", "m04_07b", "m04_08", "m04_09", "m04_10", "m04_11", "m04_12", "m04_13", "m04_14a", "m04_14a_min", "m04_14a_mjr", "m04_14b", "m04_14b_min", "m04_14b_mjr", "m04_14c", "m04_14c_min", "m04_14c_mjr", "m04_14d", "m04_14d_min", "m04_14d_mjr", "m04_14e", "m04_14e_min", "m04_14e_mjr", "m04_14f", "m04_14f_min", "m04_14f_mjr", "m04_14g", "m04_14g_min", "m04_14g_mjr", "m04_14h", "m04_14h_min", "m04_14h_mjr", "m04_14i", "m04_14i_min", "m04_14i_mjr", "m04_14j", "m04_14j_min", "m04_14j_mjr", "m04_14k", "m04_14k_min", "m04_14k_mjr", "m04_14l", "m04_14l_min", "m04_14l_mjr", "tot_minor_months", "tot_major_months", "m04_15", "m04_15a", "m04_15a_day", "m04_15a_week", "m04_15a_month", "m04_15a_time", "times_har_per_mjr_month", "m04_16", "m04_16a", "amt_kgs_major_big", "amt_kgs_major_total", "mjr_prod_kgs_factor", "m04_17", "m04_17a", "m04_17a_day", "m04_17a_week", "m04_17a_month", "m04_17a_time", "times_har_per_min_month", "m04_18", "m04_18a", "amt_kgs_minor", "amt_kgs_minor_total", "min_prod_kgs_factor", "m04_19a", "m04_19b", "sold_kgs", "m04_19b1", "m04_19c", "total_harvest_kgs", "tonnes_per_hectare", "prop_all_sold", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


	d9 <- data.frame(
		adm1 = carobiner::fix_name(r9[["region"]], "title"),
		adm2 = carobiner::fix_name(r9[["district"]], "title"),
		treatment = r9[["treatment_status"]],
		adm3 = carobiner::fix_name(r9[["community_irrigation_status"]], "title"),
		hhid = r9[["hhid"]]
	)
##r9: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "community_name", "m02_06", "this_service_name", "this_service_label", "m19_1b", "m19_1c", "m19_1c1", "m19_1d", "m19_1dGovernmentextensionrese", "m19_1dFarmerscooperativesorp", "m19_1dPrivateserviceproviders", "m19_1dLocalseedfeedmultiplie", "m19_1dOthersspecify", "m19_1d_other", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


	d10 <- data.frame(
		adm1 = carobiner::fix_name(r10[["region"]], "title"),
		adm2 = carobiner::fix_name(r10[["district"]], "title"),
		treatment = r10[["treatment_status"]],
		adm3 = carobiner::fix_name(r10[["community_irrigation_status"]], "title"),
		hhid = r10[["hhid"]],
		season = r10[["m19_3cDryseasongardening"]]
	)
##r10: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "community_name", "m02_06", "this_extension_name", "this_extension_label", "m19_3b", "m19_3c", "m19_3cRegularaccesstoanexte", "m19_3cTrainingintheuseofim", "m19_3cTraininginfertilizerus", "m19_3cTraininginimprovedcult", "m19_3cTraininginpestmanageme", "m19_3cTraininginstoragetechn", "m19_3cTraininginmarketing", "m19_3cProvisionofimprovedsee", "m19_3cProvisionofinputsequip", "m19_3cFarmmanagement", "m19_3cStoragefacilitybuilding", "m19_3cOtherdryseasonactiviti", "m19_3c1", "m19_3d", "m19_3d1", "m19_3e", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


	d11 <- data.frame(
		hhid = r11[["hhid"]],
		adm1 = carobiner::fix_name(r11[["region"]], "title"),
		adm2 = carobiner::fix_name(r11[["district"]], "title"),
		adm3 = carobiner::fix_name(r11[["community_name"]], "title"),
		treatment = r11[["treatment_status"]]
	)
##r11: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "wgt", "fpc_chps", "fpc_vill", "fpc_hh", "cdd", "CDD_GrainRootsTuber", "CDD_VitaminARichFoods", "CDD_OtherFruitsVegetables", "CDD_FleshFoods", "CDD_Egg", "CDD_PulsesLegumesNuts", "CDD_MilkProducts", "CDD_OilFat", "CDD_BIOFORTIFIED_STAPLES", "HDD", "HDD_Cereals", "HDD_WhiteTuberRoots", "HDD_Vegetables", "HDD_Fruits", "HDD_Meat", "HDD_Egg", "HDD_FishSeaFood", "HDD_LegumesNutsSeed", "HDD_MilkProduct", "HDD_OilFats", "HDD_SugarHoney", "HDD_Miscellaneous", "HDD_BIOFORTIFIED_STAPLES", "mddw", "mddw_grains_roottub_plant", "mddw_pulses", "mddw_nuts_seeds", "mddw_dairy", "mddw_meat_fish", "mddw_eggs", "mddw_leafy_veg", "mddw_vitA_fruit_veg", "mddw_other_veg", "mddw_other_fruits", "mddw_biofort_stap", "anim_VA_intake_child", "total_VA_intake_child", "total_VA_intake_caregiver", "anim_VA_intake_caregiver", "caregiver_green_leaves_1", "caregiver_pumpkin_leaves_1", "caregiver_sp_leaves_1", "caregiver_amaranth_leaves_1", "caregiver_palmoil_1", "caregiver_carrots_1", "caregiver_mangoes_1", "caregiver_squash_1", "caregiver_papaya_1", "caregiver_ofsp_1", "caregiver_yfsp_1", "caregiver_passion_1", "caregiver_eggs_1", "caregiver_fish_1", "caregiver_liver_1", "caregiver_butter_1", "caregiver_liveroil_1", "caregiver_margarine_1", "caregiver_weaning_1", "caregiver_instform_1", "caregiver_sugar_1", "caregiver_green_leaves_3", "caregiver_pumpkin_leaves_3", "caregiver_sp_leaves_3", "caregiver_amaranth_leaves_3", "caregiver_palmoil_3", "caregiver_carrots_3", "caregiver_mangoes_3", "caregiver_squash_3", "caregiver_papaya_3", "caregiver_ofsp_3", "caregiver_yfsp_3", "caregiver_passion_3", "caregiver_eggs_3", "caregiver_fish_3", "caregiver_liver_3", "caregiver_butter_3", "caregiver_liveroil_3", "caregiver_margarine_3", "caregiver_weaning_3", "caregiver_instform_3", "caregiver_sugar_3", "child_green_leaves_1", "child_pumpkin_leaves_1", "child_sp_leaves_1", "child_amaranth_leaves_1", "child_palmoil_1", "child_carrots_1", "child_mangoes_1", "child_squash_1", "child_papaya_1", "child_ofsp_1", "child_yfsp_1", "child_passion_1", "child_eggs_1", "child_fish_1", "child_liver_1", "child_butter_1", "child_liveroil_1", "child_margarine_1", "child_weaning_1", "child_instform_1", "child_sugar_1", "child_green_leaves_3", "child_pumpkin_leaves_3", "child_sp_leaves_3", "child_amaranth_leaves_3", "child_palmoil_3", "child_carrots_3", "child_mangoes_3", "child_squash_3", "child_papaya_3", "child_ofsp_3", "child_yfsp_3", "child_passion_3", "child_eggs_3", "child_fish_3", "child_liver_3", "child_butter_3", "child_liveroil_3", "child_margarine_3", "child_weaning_3", "child_instform_3", "child_sugar_3", "WAZ", "underwe_sev_child", "underwe_mod_child", "underwe_norm_child", "bmi_women_non_preg", "underwe_women_non_preg", "norm_women_non_preg", "overweight_women_non_preg", "obese_women_non_preg", "bmi_women_non_preg_postpa", "underwe_women_non_preg_postpa", "norm_women_non_preg_postpa", "overweight_women_non_preg_postpa", "obese_women_non_preg_postpa", "evbf", "eibf", "ebf2d", "ebf", "mixmf", "cbf", "isssf", "mdd", "mmf", "mmff", "mad", "eff", "swb", "ufc", "zvf", "bof"


	d12 <- data.frame(
		hhid = r12[["hhid"]],
		adm2 = carobiner::fix_name(r12[["district"]], "title"),
		adm3 = carobiner::fix_name(r12[["community_name"]], "title"),
		treatment = r12[["treatment_status"]]
	)
##r12: "mid", "chps_zone", "wgt", "fpc_chps", "fpc_vill", "fpc_hh", "potential_for_irrigation", "dahh", "sex", "hh_type", "autonomy_inc", "selfeff", "never_violence", "respect", "feelinputdecagr", "assetownership", "work_balance", "credit_accdec", "incomecontrol", "mobility", "groupmember", "group_inf", "sum_adeq", "empowered", "achieve_emp_avg", "no_achieve_emp_avg", "emp_score", "emp_score_disemp", "avg_emp_score_disemp", "ci", "hh_ineq_2", "gender_parity", "achieve_GP_avg", "noachieve_GP_avg", "GPI", "GPI_gap", "I_GPI", "PROWEAI_3DE", "disemp_index", "PROWEAI", "GPI_distr", "PROWEAI_distr", "threeDE_distr", "threeDE_irri", "GPI_irri", "PROWEAI_irri"


	d13 <- data.frame(
		hhid = r13[["hhid"]],
		adm2 = carobiner::fix_name(r13[["district"]], "title"),
		adm3 = carobiner::fix_name(r13[["community_name"]], "title"),
		treatment = r13[["treatment_status"]],
		adm1 = carobiner::fix_name(r13[["region"]], "title")
	)
##r13: "mid", "chps_zone", "potential_for_irrigation", "sex", "hh_type", "autonomy_inc", "selfeff", "never_violence", "respect", "feelinputdecagr", "assetownership", "work_balance", "credit_accdec", "incomecontrol", "mobility", "groupmember", "group_inf", "women_land_self", "women_assets_own", "men_assets_own", "ownland", "indiv_id", "respondent_id", "sub_district", "int_alone", "cycle", "community_irrigation_status", "rand", "rand1", "rand2", "rand3", "rand4", "g2_02_id1_a", "g2_02_id2_a", "g2_02_id3_a", "g2_02_id4_a", "g2_02_id5_a", "g2_02_id6_a", "g2_02_id7_a", "g2_02_id8_a", "g2_02_id9_a", "g2_02_id10_a", "g2_02_id36_a", "g2_02_id37_a", "g2_02_38_a", "g2_02a_calc", "ss_g2_03a", "g2_03a", "g2_04a", "g2_05a", "g2_06a", "g2_07a", "g2_02_id1_b", "g2_02_id2_b", "g2_02_id3_b", "g2_02_id4_b", "g2_02_id5_b", "g2_02_id6_b", "g2_02_id7_b", "g2_02_id8_b", "g2_02_id9_b", "g2_02_id10_b", "g2_02_id36_b", "g2_02_id37_b", "g2_02_38_b", "g2_02b_calc", "ss_g2_03b", "g2_03b", "g2_04b", "g2_05b", "g2_06b", "g2_07b", "g2_02_id1_c", "g2_02_id2_c", "g2_02_id3_c", "g2_02_id4_c", "g2_02_id5_c", "g2_02_id6_c", "g2_02_id7_c", "g2_02_id8_c", "g2_02_id9_c", "g2_02_id10_c", "g2_02_id36_c", "g2_02_id37_c", "g2_02_38_c", "g2_02c_calc", "ss_g2_03c", "g2_03c", "g2_04c", "g2_05c", "g2_06c", "g2_07c", "g2_02_id1_d", "g2_02_id2_d", "g2_02_id3_d", "g2_02_id4_d", "g2_02_id5_d", "g2_02_id6_d", "g2_02_id7_d", "g2_02_id8_d", "g2_02_id9_d", "g2_02_id10_d", "g2_02_id36_d", "g2_02_id37_d", "g2_02_38_d", "g2_02d_calc", "ss_g2_03d", "g2_03d", "g2_04d", "g2_05d", "g2_06d", "g2_07d", "g2_02_id1_e", "g2_02_id2_e", "g2_02_id3_e", "g2_02_id4_e", "g2_02_id5_e", "g2_02_id6_e", "g2_02_id7_e", "g2_02_id8_e", "g2_02_id9_e", "g2_02_id10_e", "g2_02_id36_e", "g2_02_id37_e", "g2_02_38_e", "g2_02e_calc", "ss_g2_03e", "g2_03e", "g2_04e", "g2_05e", "g2_06e", "g2_07e", "g2_02_id1_f", "g2_02_id2_f", "g2_02_id3_f", "g2_02_id4_f", "g2_02_id5_f", "g2_02_id6_f", "g2_02_id7_f", "g2_02_id8_f", "g2_02_id9_f", "g2_02_id10_f", "g2_02_id36_f", "g2_02_id37_f", "g2_02_38_f", "g2_02f_calc", "ss_g2_03f", "g2_03f", "g2_04f", "g2_05f", "g2_06f", "g2_07f", "g2_02_id1_g", "g2_02_id2_g", "g2_02_id3_g", "g2_02_id4_g", "g2_02_id5_g", "g2_02_id6_g", "g2_02_id7_g", "g2_02_id8_g", "g2_02_id9_g", "g2_02_id10_g", "g2_02_id36_g", "g2_02_id37_g", "g2_02_38_g", "g2_02g_calc", "ss_g2_03g", "g2_03g", "g2_04g", "g2_05g", "g2_06g", "g2_02_id1_h", "g2_02_id2_h", "g2_02_id3_h", "g2_02_id4_h", "g2_02_id5_h", "g2_02_id6_h", "g2_02_id7_h", "g2_02_id8_h", "g2_02_id9_h", "g2_02_id10_h", "g2_02_id36_h", "g2_02_id37_h", "g2_02_38_h", "g2_02h_calc", "ss_g2_03h", "g2_03h", "g2_04h", "g2_05h", "g2_06h", "g2_02_id1_i", "g2_02_id2_i", "g2_02_id3_i", "g2_02_id4_i", "g2_02_id5_i", "g2_02_id6_i", "g2_02_id7_i", "g2_02_id8_i", "g2_02_id9_i", "g2_02_id10_i", "g2_02_id36_i", "g2_02_id37_i", "g2_02_38_i", "g2_02i_calc", "ss_g2_03i", "g2_03i", "g2_04i", "g2_05i", "g2_02_id1_j", "g2_02_id2_j", "g2_02_id3_j", "g2_02_id4_j", "g2_02_id5_j", "g2_02_id6_j", "g2_02_id7_j", "g2_02_id8_j", "g2_02_id9_j", "g2_02_id10_j", "g2_02_id36_j", "g2_02_id37_j", "g2_02_38_j", "g2_02j_calc", "ss_g2_03j", "g2_03j", "g2_04j", "g2_05j", "g3_01", "g3_021", "g3_022", "g3_023", "g3_024", "g3_025", "g3_026", "g3_027", "g3_028", "g3_029", "g3_0210", "g3_0236", "g3_0237", "g3_0238", "g3_02_calc", "g3_03", "g3_041", "g3_042", "g3_043", "g3_044", "g3_045", "g3_046", "g3_047", "g3_048", "g3_049", "g3_0410", "g3_0436", "g3_0437", "g3_0438", "g3_04_calc", "g3_05_old", "g3_06_a", "g3_07a", "g3_06_b", "g3_07b", "g3_06_c", "g3_07c", "g3_06_d", "g3_07d", "g3_06_e", "g3_07e", "g3_06_f", "g3_07f", "g3_06_g", "g3_07g", "g3_06_h", "g3_07h", "g3_06_i", "g3_07i", "g3_06_j", "g3_07j", "g3_06_k", "g3_07k", "g3_06_l", "g3_07l", "g3_06_m", "g3_07m", "g3_08a", "g3_09a", "g3_10_id1_a", "g3_10_id2_a", "g3_10_id3_a", "g3_10_id4_a", "g3_10_id5_a", "g3_10_id6_a", "g3_10_id7_a", "g3_10_id8_a", "g3_10_id9_a", "g3_10_id10_a", "g3_10_id36_a", "g3_10_id37_a", "g3_10a38", "g3_10a_calc", "g3_12_id1_a", "g3_12_id2_a", "g3_12_id3_a", "g3_12_id4_a", "g3_12_id5_a", "g3_12_id6_a", "g3_12_id7_a", "g3_12_id8_a", "g3_12_id9_a", "g3_12_id10_a", "g3_12_id36_a", "g3_12_id37_a", "g3_12a38", "g3_12a_calc", "g3_13a1", "g3_13a2", "g3_13a3", "g3_13a4", "g3_13a5", "g3_13a6", "g3_13a7", "g3_13a8", "g3_13a9", "g3_13a10", "g3_13a36", "g3_13a37", "g3_13a38", "g3_13a_calc", "g3_08b", "g3_09b", "g3_10_id1_b", "g3_10_id2_b", "g3_10_id3_b", "g3_10_id4_b", "g3_10_id5_b", "g3_10_id6_b", "g3_10_id7_b", "g3_10_id8_b", "g3_10_id9_b", "g3_10_id10_b", "g3_10_id36_b", "g3_10_id37_b", "g3_10b38", "g3_10b_calc", "g3_12_id1_b", "g3_12_id2_b", "g3_12_id3_b", "g3_12_id4_b", "g3_12_id5_b", "g3_12_id6_b", "g3_12_id7_b", "g3_12_id8_b", "g3_12_id9_b", "g3_12_id10_b", "g3_12_id36_b", "g3_12_id37_b", "g3_12b38", "g3_12b_calc", "g3_13b1", "g3_13b2", "g3_13b3", "g3_13b4", "g3_13b5", "g3_13b6", "g3_13b7", "g3_13b8", "g3_13b9", "g3_13b10", "g3_13b36", "g3_13b37", "g3_13b38", "g3_13b_calc", "g3_08c", "g3_09c", "g3_10_id1_c", "g3_10_id2_c", "g3_10_id3_c", "g3_10_id4_c", "g3_10_id5_c", "g3_10_id6_c", "g3_10_id7_c", "g3_10_id8_c", "g3_10_id9_c", "g3_10_id10_c", "g3_10_id36_c", "g3_10_id37_c", "g3_10c38", "g3_10c_calc", "g3_12_id1_c", "g3_12_id2_c", "g3_12_id3_c", "g3_12_id4_c", "g3_12_id5_c", "g3_12_id6_c", "g3_12_id7_c", "g3_12_id8_c", "g3_12_id9_c", "g3_12_id10_c", "g3_12_id36_c", "g3_12_id37_c", "g3_12c38", "g3_12c_calc", "g3_13c1", "g3_13c2", "g3_13c3", "g3_13c4", "g3_13c5", "g3_13c6", "g3_13c7", "g3_13c8", "g3_13c9", "g3_13c10", "g3_13c36", "g3_13c37", "g3_13c38", "g3_13c_calc", "g3_08d", "g3_09d", "g3_10_id1_d", "g3_10_id2_d", "g3_10_id3_d", "g3_10_id4_d", "g3_10_id5_d", "g3_10_id6_d", "g3_10_id7_d", "g3_10_id8_d", "g3_10_id9_d", "g3_10_id10_d", "g3_10_id36_d", "g3_10_id37_d", "g3_10d38", "g3_10d_calc", "g3_12_id1_d", "g3_12_id2_d", "g3_12_id3_d", "g3_12_id4_d", "g3_12_id5_d", "g3_12_id6_d", "g3_12_id7_d", "g3_12_id8_d", "g3_12_id9_d", "g3_12_id10_d", "g3_12_id36_d", "g3_12_id37_d", "g3_12d38", "g3_12d_calc", "g3_13d1", "g3_13d2", "g3_13d3", "g3_13d4", "g3_13d5", "g3_13d6", "g3_13d7", "g3_13d8", "g3_13d9", "g3_13d10", "g3_13d36", "g3_13d37", "g3_13d38", "g3_13d_calc", "g3_08e", "g3_09e", "g3_10_id1_e", "g3_10_id2_e", "g3_10_id3_e", "g3_10_id4_e", "g3_10_id5_e", "g3_10_id6_e", "g3_10_id7_e", "g3_10_id8_e", "g3_10_id9_e", "g3_10_id10_e", "g3_10_id36_e", "g3_10_id37_e", "g3_10e38", "g3_10e_calc", "g3_12_id1_e", "g3_12_id2_e", "g3_12_id3_e", "g3_12_id4_e", "g3_12_id5_e", "g3_12_id6_e", "g3_12_id7_e", "g3_12_id8_e", "g3_12_id9_e", "g3_12_id10_e", "g3_12_id36_e", "g3_12_id37_e", "g3_12e38", "g3_12e_calc", "g3_13e1", "g3_13e2", "g3_13e3", "g3_13e4", "g3_13e5", "g3_13e6", "g3_13e7", "g3_13e8", "g3_13e9", "g3_13e10", "g3_13e36", "g3_13e37", "g3_13e38", "g3_13e_calc", "g3_08f", "g3_09f", "g3_10_id1_f", "g3_10_id2_f", "g3_10_id3_f", "g3_10_id4_f", "g3_10_id5_f", "g3_10_id6_f", "g3_10_id7_f", "g3_10_id8_f", "g3_10_id9_f", "g3_10_id10_f", "g3_10_id36_f", "g3_10_id37_f", "g3_10f38", "g3_10f_calc", "g3_12_id1_f", "g3_12_id2_f", "g3_12_id3_f", "g3_12_id4_f", "g3_12_id5_f", "g3_12_id6_f", "g3_12_id7_f", "g3_12_id8_f", "g3_12_id9_f", "g3_12_id10_f", "g3_12_id36_f", "g3_12_id37_f", "g3_12f38", "g3_12f_calc", "g3_13f1", "g3_13f2", "g3_13f3", "g3_13f4", "g3_13f5", "g3_13f6", "g3_13f7", "g3_13f8", "g3_13f9", "g3_13f10", "g3_13f36", "g3_13f37", "g3_13f38", "g3_13f_calc", "g3_14", "ut0", "u1", "ut_1ot", "ut1", "ucc_01", "u2", "ut_2ot", "ut2", "ucc_12", "u3", "ut_3ot", "ut3", "ucc_23", "u4", "ut_4ot", "ut4", "ucc_34", "u5", "ut_5ot", "ut5", "ucc_45", "u6", "ut_6ot", "ut6", "ucc_56", "u7", "ut_7ot", "ut7", "ucc_67", "u8", "ut_8ot", "ut8", "ucc_78", "u9", "ut_9ot", "ut9", "ucc_89", "u10", "ut_10ot", "ut10", "ucc_910", "u11", "ut_11ot", "ut11", "ucc_1011", "u12", "ut_12ot", "ut12", "ucc_1112", "u13", "ut_13ot", "ut13", "ucc_1213", "u14", "ut_14ot", "ut14", "ucc_1314", "u15", "ut_15ot", "ut15", "ucc_1415", "u16", "ut_16ot", "ut16", "ucc_1516", "u17", "ut_17ot", "ut17", "ucc_1617", "u18", "ut_18ot", "ut18", "ucc_1718", "u19", "ut_19ot", "ut19", "ucc_1819", "u20", "ut_20ot", "ut20", "ucc_1920", "u21", "ut_21ot", "ut21", "ucc_2021", "u22", "ut_22ot", "ut22", "ucc_2122", "u23", "ut23", "ucc_2223", "u24", "ut24", "ucc_2324", "u25", "ut_25ot", "ut25", "ucc_2425", "u26", "ut_26ot", "ut26", "ucc_2526", "u27", "ut_27ot", "ut27", "ucc_2627", "u28", "ut_28ot", "ut28", "ucc_2728", "u29", "ut_29ot", "ut29", "ucc_2829", "u30", "ut_30ot", "ut30", "ucc_2930", "u31", "ut_31ot", "ut31", "ucc_3031", "g403", "g5_03a", "g5_04a", "g5_05a", "g5_06a", "g5_07a", "g5_03b", "g5_04b", "g5_05b", "g5_06b", "g5_07b", "g5_03c", "g5_04c", "g5_05c", "g5_06c", "g5_07c", "g5_03d", "g5_04d", "g5_05d", "g5_06d", "g5_07d", "g5_03e", "g5_04e", "g5_05e", "g5_06e", "g5_07e", "g5_03f", "g5_04f", "g5_05f", "g5_06f", "g5_07f", "g5_03g", "g5_04g", "g5_05g", "g5_06g", "g5_07g", "g5_03h", "g5_04h", "g5_05h", "g5_06h", "g5_07h", "g5_03hp", "g5_04hp", "g5_05hp", "g5_06hp", "g5_07hp", "g5_03i", "g5_03i_oth", "g5_04i", "g5_05i", "g5_06i", "g5_07i", "g6_01", "g6_02", "g6_03", "g6_04", "g6_05", "g6_06", "g6_07", "g6_08", "g6_09c1", "g6_09c2", "g6_09c3", "g6_09c4", "g6_09c5", "g6_09c6", "g6_09c7", "g6_09c8", "g6_09c9", "g6_09c10", "g6_09c36", "g6_09c37", "g6_09c38", "g6_09c_calc", "g6_10c", "g6_11c", "g6_11cIfIhavecompanyegr", "g6_11cIfIcanarrangemyowne", "g6_11cIfIfollowpurdahdre", "g6_11cWouldneverbeallowedto", "g6_11cOtherspecify", "g6_11c_ot", "g6_12c", "g6_09e1", "g6_09e2", "g6_09e3", "g6_09e4", "g6_09e5", "g6_09e6", "g6_09e7", "g6_09e8", "g6_09e9", "g6_09e10", "g6_09e36", "g6_09e37", "g6_09e38", "g6_09e_calc", "g6_10e", "g6_11e", "g6_11eIfIhavecompanyegr", "g6_11eIfIcanarrangemyowne", "g6_11eIfIfollowpurdahdre", "g6_11eWouldneverbeallowedto", "g6_11eOtherspecify", "g6_11e_ot", "g6_12e", "g6_09h1", "g6_09h2", "g6_09h3", "g6_09h4", "g6_09h5", "g6_09h6", "g6_09h7", "g6_09h8", "g6_09h9", "g6_09h10", "g6_09h36", "g6_09h37", "g6_09h38", "g6_09h_calc", "g6_10h", "g6_11h", "g6_11hIfIhavecompanyegr", "g6_11hIfIcanarrangemyowne", "g6_11hIfIfollowpurdahdre", "g6_11hWouldneverbeallowedto", "g6_11hOtherspecify", "g6_11h_ot", "g6_12h", "g7_02a", "g7_03a", "g7_04a", "g7_05a", "g7_06a", "g7_02cf", "g7_03cf", "g7_04cf", "g7_05cf", "g81_a1_01", "g81_a1_01_g8_02", "g81_a1_01_g8_03", "g81_a2_01", "g81_a2_01_g8_02", "g81_a2_01_g8_03", "g81_a3_01", "g81_a3_01_g8_02", "g81_a3_01_g8_03", "g81_a4_01", "g81_a4_01_g8_02", "g81_a4_01_g8_03", "g81_b1_01", "g81_b1_01_g8_02", "g81_b1_01_g8_03", "g81_b2_01", "g81_b2_01_g8_02", "g81_b2_01_g8_03", "g81_b3_01", "g81_b3_01_g8_02", "g81_b3_01_g8_03", "g81_b4_01", "g81_b4_01_g8_02", "g81_b4_01_g8_03", "g81_d1_01", "g81_d1_01_g8_02", "g81_d1_01_g8_03", "g81_d2_01", "g81_d2_01_g8_02", "g81_d2_01_g8_03", "g81_d3_01", "g81_d3_01_g8_02", "g81_d3_01_g8_03", "g81_d4_01", "g81_d4_01_g8_02", "g81_d4_01_g8_03", "g82_a1_01", "g82_a1_01_g8_02", "g82_a1_01_g8_03", "g82_a2_01", "g82_a2_01_g8_02", "g82_a2_01_g8_03", "g82_a3_01", "g82_a3_01_g8_02", "g82_a3_01_g8_03", "g82_a4_01", "g82_a4_01_g8_02", "g82_a4_01_g8_03", "g82_b1_01", "g82_b1_01_g8_02", "g82_b1_01_g8_03", "g82_b2_01", "g82_b2_01_g8_02", "g82_b2_01_g8_03", "g82_b3_01", "g82_b3_01_g8_02", "g82_b3_01_g8_03", "g82_b4_01", "g82_b4_01_g8_02", "g82_b4_01_g8_03", "g82_d1_01", "g82_d1_01_g8_02", "g82_d1_01_g8_03", "g82_d2_01", "g82_d2_01_g8_02", "g82_d2_01_g8_03", "g82_d3_01", "g82_d3_01_g8_02", "g82_d3_01_g8_03", "g82_d4_01", "g82_d4_01_g8_02", "g82_d4_01_g8_03", "g83_a1_01", "g83_a1_01_g8_02", "g83_a1_01_g8_03", "g83_a2_01", "g83_a2_01_g8_02", "g83_a2_01_g8_03", "g83_a3_01", "g83_a3_01_g8_02", "g83_a3_01_g8_03", "g83_a4_01", "g83_a4_01_g8_02", "g83_a4_01_g8_03", "g83_b1_01", "g83_b1_01_g8_02", "g83_b1_01_g8_03", "g83_b2_01", "g83_b2_01_g8_02", "g83_b2_01_g8_03", "g83_b3_01", "g83_b3_01_g8_02", "g83_b3_01_g8_03", "g83_b4_01", "g83_b4_01_g8_02", "g83_b4_01_g8_03", "g83_d1_01", "g83_d1_01_g8_02", "g83_d1_01_g8_03", "g83_d2_01", "g83_d2_01_g8_02", "g83_d2_01_g8_03", "g83_d3_01", "g83_d3_01_g8_02", "g83_d3_01_g8_03", "g83_d4_01", "g83_d4_01_g8_02", "g83_d4_01_g8_03", "g84_a1_01", "g84_a1_01_g8_02", "g84_a1_01_g8_03", "g84_a2_01", "g84_a2_01_g8_02", "g84_a2_01_g8_03", "g84_a3_01", "g84_a3_01_g8_02", "g84_a3_01_g8_03", "g84_a4_01", "g84_a4_01_g8_02", "g84_a4_01_g8_03", "g84_b1_01", "g84_b1_01_g8_02", "g84_b1_01_g8_03", "g84_b2_01", "g84_b2_01_g8_02", "g84_b2_01_g8_03", "g84_b3_01", "g84_b3_01_g8_02", "g84_b3_01_g8_03", "g84_b4_01", "g84_b4_01_g8_02", "g84_b4_01_g8_03", "g84_d1_01", "g84_d1_01_g8_02", "g84_d1_01_g8_03", "g84_d2_01", "g84_d2_01_g8_02", "g84_d2_01_g8_03", "g84_d4_01", "g84_d4_01_g8_02", "g84_d4_01_g8_03", "g84_d3_01", "g84_d3_01_g8_02", "g84_d3_01_g8_03", "g81_b1_01a", "g8_04_b", "g8_04_c", "g81_b1_01d", "g81_b1_01e", "g8_04_f", "g81_b1_03", "g8_04_h", "g81_c1_01", "g81_c1_02", "g81_c1_03", "g91_a1_01", "g91_a1_02", "g91_a1_03", "g91_a1_04", "g91_a1_05", "hhid_checked", "ut_23ot", "ut_24ot", "hhcount", "indiv_by_hh", "no_hh", "g2_01_a", "g2_01_b", "g2_01_c", "g2_01_d", "g2_01_e", "g2_01_f", "g2_01_g", "g2_01_h", "partact_a", "partact_b", "partact_c", "partact_d", "partact_e", "partact_f", "partact_g", "partact_h", "partactagr", "partactagnonag", "g2_02_id_others_a", "g2_02_id_others_b", "g2_02_id_others_c", "g2_02_id_others_d", "g2_02_id_others_e", "g2_02_id_others_f", "g2_02_id_others_g", "g2_02_id_others_h", "self_a", "self_b", "self_c", "self_d", "self_e", "self_f", "self_g", "self_h", "g2_03_a", "g2_03_b", "g2_03_c", "g2_03_d", "g2_03_e", "g2_03_f", "g2_03_g", "g2_03_h", "inputdec_a", "inputdec_b", "inputdec_c", "inputdec_d", "inputdec_e", "inputdec_f", "inputdec_g", "inputdec_h", "g2_04_a", "g2_04_b", "g2_04_c", "g2_04_d", "g2_04_e", "g2_04_f", "g2_04_g", "g2_04_h", "g2_04_i", "g2_04_j", "feelmakedec_a", "feelmakedec_b", "feelmakedec_c", "feelmakedec_d", "feelmakedec_e", "feelmakedec_f", "feelmakedec_g", "feelmakedec_h", "g2_02_id_others_i", "g2_02_id_others_j", "self_i", "self_j", "feelmakedec_i", "feelmakedec_j", "someinput_prod_a", "someinput_prod_b", "someinput_prod_c", "someinput_prod_d", "someinput_prod_e", "someinput_prod_f", "someinput_prod_g", "someinput_prod_h", "someinput_prod", "cont_inputprod", "someinput_prod_ratio", "g3_05", "g3_07_a", "g3_07_b", "g3_07_c", "g3_07_d", "g3_07_e", "g3_07_f", "g3_07_g", "g3_07_h", "g3_07_i", "g3_07_j", "g3_07_k", "g3_07_l", "g3_07_m", "own_a", "own_b", "own_c", "own_d", "own_e", "own_f", "own_g", "own_h", "own_i", "own_j", "own_k", "own_l", "own_m", "own_sum", "selfjointown_a", "selfjointown_b", "selfjointown_c", "selfjointown_d", "selfjointown_e", "selfjointown_f", "selfjointown_g", "selfjointown_h", "selfjointown_i", "selfjointown_j", "selfjointown_k", "selfjointown_l", "selfjointown_m", "assetcount", "cont_assets", "g3_08_a", "g3_08_b", "g3_08_c", "g3_08_d", "g3_08_e", "g3_08_f", "g3_09_a", "g3_09_b", "g3_09_c", "g3_09_d", "g3_09_e", "g3_09_f", "g3_10_id_others_a", "g3_10_id_others_b", "g3_10_id_others_c", "g3_10_id_others_d", "g3_10_id_others_e", "g3_10_id_others_f", "g3_12_id_others_a", "g3_12_id_others_b", "g3_12_id_others_c", "g3_12_id_others_d", "g3_12_id_others_e", "g3_12_id_others_f", "creditaccess_a", "creditaccess_b", "creditaccess_c", "creditaccess_d", "creditaccess_e", "creditaccess_f", "creditaccess", "credituse_a", "credituse_b", "credituse_c", "credituse_d", "credituse_e", "credituse_f", "credituse", "creditdec_a", "creditdec_b", "creditdec_c", "creditdec_d", "creditdec_e", "creditdec_f", "creditusedec_a", "creditusedec_c", "creditusedec_b", "creditusedec_d", "creditusedec_e", "creditusedec_f", "creditsum", "account", "cont_credit", "g2_07_a", "g2_07_b", "g2_07_c", "g2_07_d", "g2_07_e", "g2_07_f", "g2_07_g", "g2_07_h", "g2_06_a", "g2_06_b", "g2_06_c", "g2_06_d", "g2_06_e", "g2_06_f", "someinput_output_a", "someinput_output_b", "someinput_output_c", "someinput_output_d", "someinput_output_e", "someinput_output_f", "someinput_income_a", "someinput_income_b", "someinput_income_c", "someinput_income_d", "someinput_income_e", "someinput_income_f", "someinput_income_g", "someinput_income_h", "someinput_both_a", "someinput_both_b", "someinput_both_c", "someinput_both_d", "someinput_both_e", "someinput_both_f", "someinput_sum", "cont_income", "someinput_ratio", "g8_01_d2", "g8_01_d3", "g8_01_d4", "rai_2", "rai_3", "rai_4", "cont_autonomy", "g5_01_a", "g5_01_b", "g5_01_c", "g5_01_d", "g5_01_e", "g5_01_f", "g5_01_g", "g5_01_h", "g5_01_i", "g5_03_a", "g5_03_b", "g5_03_c", "g5_03_d", "g5_03_e", "g5_03_f", "g5_03_g", "g5_03_h", "g5_03_i", "group_a", "group_b", "group_c", "group_d", "group_e", "group_f", "group_g", "group_h", "group_i", "groupmember_a", "groupmember_b", "groupmember_c", "groupmember_d", "groupmember_e", "groupmember_f", "groupmember_g", "groupmember_h", "groupmember_i", "groupsum", "groupmembersum", "cont_group", "g5_05_a", "g5_05_b", "g5_05_c", "g5_05_d", "g5_05_e", "g5_05_f", "g5_05_g", "g5_05_h", "g5_05_i", "group_inf_a", "group_inf_b", "group_inf_c", "group_inf_d", "group_inf_e", "group_inf_f", "group_inf_g", "group_inf_h", "group_inf_i", "group_inf_sum", "cont_group_inf", "visit_city", "visit_market", "visit_family", "visit_friend", "visit_health", "visit_public", "visitweekly_city", "visitmonthly_city", "visitweekly_market", "visitmonthly_market", "visitweekly_family", "visitmonthly_family", "visitweekly_health", "visitmonthly_health", "visitweekly_public", "visitmonthly_public", "visitweekly", "visitmonthly", "cont_mobility", "respect_relation", "respect_resp", "trust", "disagree", "g9_01_c", "g9_01_d", "g9_01_e", "g9_01_a", "g9_01_b", "violence_okay_a", "violence_okay_b", "violence_okay_c", "violence_okay_d", "violence_okay_e", "ngse", "cont_selfeff", "u_work_01", "u_work_12", "u_work_23", "u_work_34", "u_work_45", "u_work_56", "u_work_67", "u_work_78", "u_work_89", "u_work_910", "u_work_1011", "u_work_1112", "u_work_1213", "u_work_1314", "u_work_1415", "u_work_1516", "u_work_1617", "u_work_1718", "u_work_1819", "u_work_1920", "u_work_2021", "u_work_2122", "u_work_2223", "u_work_2324", "u_work_2425", "u_work_2526", "u_work_2627", "u_work_2728", "u_work_2829", "u_work_2930", "u_work_3031", "utdiff_01", "utdiff_12", "utdiff_23", "utdiff_34", "utdiff_45", "utdiff_56", "utdiff_67", "utdiff_78", "utdiff_89", "utdiff_910", "utdiff_1011", "utdiff_1112", "utdiff_1213", "utdiff_1314", "utdiff_1415", "utdiff_1516", "utdiff_1617", "utdiff_1718", "utdiff_1819", "utdiff_1920", "utdiff_2021", "utdiff_2122", "utdiff_2223", "utdiff_2324", "utdiff_2425", "utdiff_2526", "utdiff_2627", "utdiff_2728", "utdiff_2829", "utdiff_2930", "utdiff_3031", "hours_total", "hours_work_01", "hours_work_12", "hours_work_23", "hours_work_34", "hours_work_45", "hours_work_56", "hours_work_67", "hours_work_78", "hours_work_89", "hours_work_910", "hours_work_1011", "hours_work_1112", "hours_work_1213", "hours_work_1314", "hours_work_1415", "hours_work_1516", "hours_work_1617", "hours_work_1718", "hours_work_1819", "hours_work_1920", "hours_work_2021", "hours_work_2122", "hours_work_2223", "hours_work_2324", "hours_work_2425", "hours_work_2526", "hours_work_2627", "hours_work_2728", "hours_work_2829", "hours_work_2930", "hours_work_3031", "workload", "cont_workload", "g3_03_encoded", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


	d14 <- data.frame(
		hhid = r14[["hhid"]],
		adm1 = carobiner::fix_name(r14[["region"]], "title"),
		adm2 = carobiner::fix_name(r14[["district"]], "title"),
		adm3 = carobiner::fix_name(r14[["community_name"]], "title"),
		treatment = r14[["treatment_status"]]
	)
##r14: "sub_district", "chps_zone", "potential_for_irrigation", "wgt", "fpc_chps", "fpc_vill", "fpc_hh", "atti_m19_18a", "atti_m19_18b", "atti_m19_18c", "atti_m19_18d", "atti_m19_18e", "atti_m19_18f", "gendequi_score", "low_atti_gendequi", "mod_atti_gendequi", "high_atti_gendequi"


	d15 <- data.frame(
		country = r15[["country"]],
		adm1 = carobiner::fix_name(r15[["region"]], "title"),
		adm2 = carobiner::fix_name(r15[["district"]], "title"),
		adm3 = carobiner::fix_name(r15[["community"]], "title"),
		treatment = r15[["treatment_status"]],
		hhid = r15[["hhid"]],
		K_fertilizer = r15[["k_01a_mother_baby_skin"]]
	)
##r15: "sub_district", "chps_zone", "cycle", "potential_for_irrigation", "community_irrigation_status", "community_name", "respondent_id", "g1_03", "sex", "k_01a_mother_baby_skin_mark", "k_01b_breastfeed", "k_01b_breastfeed_mark", "k_01c_take_vitamin_a", "k_01c_take_vitamin_a_mark", "k_01d_herbs_water", "k_02_yellowish_milk", "yellowish_milk_score", "k_03_breastfeed_regularly", "breastfeed_regularly_score", "k_04_how_much_more", "how_much_more_score", "k_05_water_age", "k_05_unit", "k_06_given_foods", "k_06_unit", "k_07_sweetpotato", "k_07_unit", "k_08_no_specialpro_feedign", "no_specialpro_feedign_score", "k_09a", "enriched_porridge_score", "k_09b", "k_09c", "k_09d", "k_09d_specify", "k_10_feeding_times_aday", "feeding_times_aday_score", "k_11_18months_feeding", "k18months_feeding_score", "k_12_cannot_talk_hungry", "k_12_cannot_talk_Fussines", "k_12_cannot_talk_Fussines_mark", "k_12_cannot_talk_Bringsh", "k_12_cannot_talk_Bringsh_mark", "k_12_cannot_talk_Opening", "k_12_cannot_talk_Opening_mark", "k_12_cannot_talk_Walkcra", "k_12_cannot_talk_Walkcra_mark", "k_12_cannot_talk_hungryOther", "K_12a", "k_13_different_foods", "k_13_different_Energygivi", "k_13_different_Energygivi_mark", "k_13_different_Bodybuildi", "k_13_different_Bodybuildi_mark", "k_13_different_Protective", "k_13_different_Protective_mark", "k_13_different_sLegumesand", "k_13_different_sLegumesand_mark", "k_13_different_foodsOther", "k_13a", "k_14_refuse_to_eat", "refuse_to_eat_score", "k_15_heard_vitaminA", "heard_vitaminA_score", "k_16a", "k_16a_mark", "k_16b", "k_16b_mark", "k_16c", "k_16c_specify", "k_17_vitaminA_source", "vitA_sourceOFSP", "vitA_sourceOFSP_mark", "vitA_sourceMango", "vitA_sourceMango_mark", "vitA_sourcePalmoil", "vitA_sourcePalmoil_mark", "vitA_sourceEggs", "vitA_sourceEggs_mark", "vitA_sourcePawpaw", "vitA_sourcePawpaw_mark", "vitA_sourceMeat", "vitA_sourceMeat_mark", "vitA_sourceMilk", "vitA_sourceMilk_mark", "vitA_sourceDarkgreen", "vitA_sourceDarkgreen_mark", "vitA_sourceLiver", "vitA_sourceLiver_mark", "k_17_vitaminA_sourceOther", "k_17_vitaminA_sourceDonotknow", "k_18_mineral", "mineral_score", "k_19_source_calcium", "calcMaintainsth", "calcMaintainsth_mark", "calcSupportshar", "calcSupportshar_mark", "calcAidsmuscle", "calcAidsmuscle_mark", "k_19_source_calciumOther", "k_19_source_calcium_specify", "k_20_calcium_source", "calcisourceMoringaLeav", "calcisourceMoringaLeav_mark", "calcsourceAmaranth", "calcsourceAmaranth_mark", "calcisourceMilk", "calcisourceMilk_mark", "calcisourceYoghurt", "calcisourceYoghurt_mark", "calcisourceCheese", "calcisourceCheese_mark", "calcsourceOkra", "calcsourceOkra_mark", "calcisourceCowpeas", "calcisourceCowpeas_mark", "calcisourceFish", "calcisourceFish_mark", "immediate_baby_birth_score", "water_age_score", "given_foods_score", "sweetpotato_score", "signs_child_hungry_score", "healthy_food_score", "Vitamin_a_importance_score", "vitA_source_score", "calcium_role_score", "calcium_sourc_score", "nutrtional_knowledge_score", "wgt", "fpc_chps", "fpc_vill", "fpc_hh"


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

