# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: CC 4.1. Urban consumer preference study

This data set details information collected on preferred characteristics of urban consumers for boiled sweetpotato and banana.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/RJVRRL"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Bioversity International - Bioversity",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Indidividual respondent; universe: entebbe and kampala; timeMethod: Real; dataCollector: Sarah Mayanja ( CIP ); samplingProcedure: Random sampling",
		
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
	

	f1 <- ff[basename(ff) == "Data V2.xlsx"]
	f2 <- ff[basename(ff) == "Data_V1.xlsx"]
	f3 <- ff[basename(ff) == "xlsForm_V1.xlsx"]
	f4 <- ff[basename(ff) == "xlsForm_V2.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Demand segments for RTB prod...")
	r1b <- carobiner::read.excel(f1, sheet="matooke_quality")
	r1c <- carobiner::read.excel(f1, sheet="matooke_quality_prep")
	r1d <- carobiner::read.excel(f1, sheet="matooke_less_pref_buy")
	r1e <- carobiner::read.excel(f1, sheet="matooke_less_pref_prep")
	r1f <- carobiner::read.excel(f1, sheet="matooke_less_pref_cons")
	r1g <- carobiner::read.excel(f1, sheet="potato_quality")
	r1h <- carobiner::read.excel(f1, sheet="potato_quality_prep")
	r1i <- carobiner::read.excel(f1, sheet="potato_less_pref_buy")
	r1j <- carobiner::read.excel(f1, sheet="potato_less_pref_prep")
	r1k <- carobiner::read.excel(f1, sheet="potato_less_pref_cons")
	r1l <- carobiner::read.excel(f1, sheet="matooke_forms_strg_affected")
	r1m <- carobiner::read.excel(f1, sheet="potatoe_forms_strg_affected")
	r2a <- carobiner::read.excel(f2, sheet="Demand segments for RTB prod...")
	r2b <- carobiner::read.excel(f2, sheet="matooke_quality")
	r2c <- carobiner::read.excel(f2, sheet="matooke_quality_prep")
	r2d <- carobiner::read.excel(f2, sheet="matooke_less_pref_buy")
	r2e <- carobiner::read.excel(f2, sheet="matooke_less_pref_prep")
	r2f <- carobiner::read.excel(f2, sheet="matooke_less_pref_cons")
	r2g <- carobiner::read.excel(f2, sheet="potato_quality")
	r2h <- carobiner::read.excel(f2, sheet="potato_quality_prep")
	r2i <- carobiner::read.excel(f2, sheet="potato_less_pref_buy")
	r2j <- carobiner::read.excel(f2, sheet="potato_less_pref_prep")
	r2k <- carobiner::read.excel(f2, sheet="potato_less_pref_cons")
	r2l <- carobiner::read.excel(f2, sheet="matooke_forms_strg_affected")
	r2m <- carobiner::read.excel(f2, sheet="potatoe_forms_strg_affected")
	r3a <- carobiner::read.excel(f3, sheet="survey")
	r3b <- carobiner::read.excel(f3, sheet="choices")
	r3c <- carobiner::read.excel(f3, sheet="settings")
	r4a <- carobiner::read.excel(f4, sheet="survey")
	r4b <- carobiner::read.excel(f4, sheet="choices")
	r4c <- carobiner::read.excel(f4, sheet="settings")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		adm2 = carobiner::fix_name(r1a[["district"]], "title"),
		location = r1a[["village"]],
		P_fertilizer = r1a[["p_sensory_cons_clr"]],
		variety = r1a[["m_quality_buy.Variety"]],
		rep = r1a[["m_quality_prep"]],
		soil_texture = tolower(r1a[["m_less_pref_prep.Peel_texture"]]),
		latitude = r1a[["_GPS_latitude"]],
		longitude = r1a[["_GPS_longitude"]],
		elevation = r1a[["_GPS_altitude"]]
	)
##r1a: "data", "start", "end", "today", "consent", "ID", "division", "other_division", "parish", "other_parish", "code_name", "sex", "education", "age", "tribe", "other_tribe", "marital_status", "religion", "occupation", "self_employment", "other_occupation", "relationship_to_head", "other_relationship", "has_children", "note_1", "Q1", "Q1.1", "Q1.2", "Q1.3", "Q1.4", "Q1.5", "Q1.6", "Q1.7", "Q1_Other", "Q1_label", "Q1a", "Q1b", "Q1c", "Q1d", "Q1e", "Q1f", "Q1g", "Q1a_reason", "m_sensory_cons_clr", "m_sensory_cons_clr.1", "m_sensory_cons_clr.2", "m_sensory_cons_clr.3", "m_sensory_cons_clr.4", "m_sensory_cons_clr.5", "m_sensory_cons_clr_des", "Q1b_reason", "m_sensory_cons_flvr", "m_sensory_cons_flvr.1", "m_sensory_cons_flvr.2", "m_sensory_cons_flvr.3", "m_sensory_cons_flvr.4", "m_sensory_cons_flvr.5", "m_sensory_cons_flvr_des", "Q1c_reason", "m_sensory_cons_txt", "m_sensory_cons_txt.1", "m_sensory_cons_txt.2", "m_sensory_cons_txt.3", "m_sensory_cons_txt.4", "m_sensory_cons_txt.5", "m_sensory_cons_txt_des", "Q1d_reason", "m_sensory_cons_arm", "m_sensory_cons_arm.1", "m_sensory_cons_arm.2", "m_sensory_cons_arm.3", "m_sensory_cons_arm.4", "m_sensory_cons_arm.5", "m_sensory_cons_arm_des", "Q1e_reason", "m_sensory_cons_frmp", "m_sensory_cons_frmp.1", "m_sensory_cons_frmp.2", "m_sensory_cons_frmp.3", "m_sensory_cons_frmp.4", "m_sensory_cons_frmp.5", "m_sensory_cons_frmp_des", "Q1f_reason", "m_sensory_cons_tst", "m_sensory_cons_tst.1", "m_sensory_cons_tst.2", "m_sensory_cons_tst.3", "m_sensory_cons_tst.4", "m_sensory_cons_tst.5", "m_sensory_cons_tst_des", "Q1g_reason", "m_sensory_cons_othr", "m_sensory_cons_othr.1", "m_sensory_cons_othr.2", "m_sensory_cons_othr.3", "m_sensory_cons_othr.4", "m_sensory_cons_othr.5", "m_sensory_cons_othr_des", "m_spouse_consume", "m_same_xtics_as_spouse", "m_same_xtics_as_spouse_diff", "Q2", "Q2.1", "Q2.2", "Q2.3", "Q2.4", "Q2.5", "Q2.6", "Q2.7", "Q2_Other", "Q2_label", "Q2a", "Q2b", "Q2c", "Q2d", "Q2e", "Q2f", "Q2g", "m_children_consume", "m_same_xtics_as_children", "m_same_xtics_as_child_diff", "Q3", "Q3.1", "Q3.2", "Q3.3", "Q3.4", "Q3.5", "Q3.6", "Q3.7", "Q3_Other", "Q3_label", "Q3a", "Q3b", "Q3c", "Q3d", "Q3e", "Q3f", "Q3g", "note_2", "Q4", "Q4.1", "Q4.2", "Q4.3", "Q4.4", "Q4.5", "Q4.6", "Q4.7", "Q4_Other", "Q4_label", "Q4a", "Q4b", "Q4c", "Q4d", "Q4e", "Q4f", "Q4g", "Q4a_reason", "p_sensory_cons_clr.1", "p_sensory_cons_clr.2", "p_sensory_cons_clr.3", "p_sensory_cons_clr.4", "p_sensory_cons_clr.5", "p_sensory_cons_clr_des", "Q4b_reason", "p_sensory_cons_flvr", "p_sensory_cons_flvr.1", "p_sensory_cons_flvr.2", "p_sensory_cons_flvr.3", "p_sensory_cons_flvr.4", "p_sensory_cons_flvr.5", "p_sensory_cons_flvr_des", "Q4c_reason", "p_sensory_cons_txt", "p_sensory_cons_txt.1", "p_sensory_cons_txt.2", "p_sensory_cons_txt.3", "p_sensory_cons_txt.4", "p_sensory_cons_txt.5", "p_sensory_cons_txt_des", "Q4d_reason", "p_sensory_cons_arm", "p_sensory_cons_arm.1", "p_sensory_cons_arm.2", "p_sensory_cons_arm.3", "p_sensory_cons_arm.4", "p_sensory_cons_arm.5", "p_sensory_cons_arp_des", "Q4e_reason", "p_sensory_cons_frmp", "p_sensory_cons_frmp.1", "p_sensory_cons_frmp.2", "p_sensory_cons_frmp.3", "p_sensory_cons_frmp.4", "p_sensory_cons_frmp.5", "p_sensory_cons_frmp_des", "Q4f_reason", "p_sensory_cons_tst", "p_sensory_cons_tst.1", "p_sensory_cons_tst.2", "p_sensory_cons_tst.3", "p_sensory_cons_tst.4", "p_sensory_cons_tst.5", "p_sensory_cons_tst_des", "Q4g_reason", "p_sensory_cons_othr", "p_sensory_cons_othr.1", "p_sensory_cons_othr.2", "p_sensory_cons_othr.3", "p_sensory_cons_othr.4", "p_sensory_cons_othr.5", "p_sensory_cons_othr_des", "p_spouse_consume", "p_same_xtics_as_spouse", "p_same_xtics_as_spouse_diff", "Q5", "Q5.1", "Q5.2", "Q5.3", "Q5.4", "Q5.5", "Q5.6", "Q5.7", "Q5_Other", "Q5_label", "Q5a", "Q5b", "Q5c", "Q5d", "Q5e", "Q5f", "Q5g", "p_children_consume", "p_same_xtics_as_children", "p_same_xtics_as_child_diff", "Q6", "Q6.1", "Q6.2", "Q6.3", "Q6.4", "Q6.5", "Q6.6", "Q6.7", "Q6_Other", "Q6_label", "Q6a", "Q6b", "Q6c", "Q6d", "Q6e", "Q6f", "Q6g", "note_3", "Q7", "Q7.1", "Q7.2", "Q7.3", "Q7.4", "Q7.5", "Q7.6", "Q7_Other", "m_forms_adult", "m_forms_adult.Fresh", "m_forms_adult.Boiled", "m_forms_adult.Steamed", "m_forms_adult.Fried", "m_forms_adult.Roasted", "m_forms_adult.Others", "m_forms_adult.None", "m_forms_children", "m_forms_children.Fresh", "m_forms_children.Boiled", "m_forms_children.Steamed", "m_forms_children.Fried", "m_forms_children.Roasted", "m_forms_children.Others", "m_forms_children.None", "Q7_label", "Q7a", "Q7b", "Q7c", "Q7d", "Q7e", "Q7f", "note_4", "Q8_label", "Q8a", "Q8b", "Q8c", "Q8d", "Q8e", "Q8f", "m_decision_mbr", "m_other_member", "m_decision_age", "m_decision_sex", "m_a_trendfresh", "m_c_trendfresh", "m_a_trendboiled", "m_c_trendboiled", "m_a_trendsteamed", "m_c_trendsteamed", "m_a_trendfried", "m_c_trendfried", "m_a_trendroasted", "m_c_trendroasted", "m_a_trendothers", "m_c_trendothers", "m_form_bought", "m_form_bought.Fresh", "m_form_bought.Boiled", "m_form_bought.Steamed", "m_form_bought.Fried", "m_form_bought.Roasted", "m_form_bought.Others", "m_form_bought.None", "m_form_bought_othr", "Q9", "Q9.1", "Q9.2", "Q9.3", "Q9.4", "Q9.5", "Q9.6", "Q9_Other", "p_forms_adult", "p_forms_adult.Fresh", "p_forms_adult.Boiled", "p_forms_adult.Steamed", "p_forms_adult.Fried", "p_forms_adult.Roasted", "p_forms_adult.Others", "p_forms_adult.None", "p_forms_children", "p_forms_children.Fresh", "p_forms_children.Boiled", "p_forms_children.Steamed", "p_forms_children.Fried", "p_forms_children.Roasted", "p_forms_children.Others", "p_forms_children.None", "Q9_label", "Q9a", "Q9b", "Q9c", "Q9d", "Q9e", "Q9f", "note_5", "Q10_label", "Q10a", "Q10b", "Q10c", "Q10d", "Q10e", "Q10f", "p_decision_mbr", "p_other_member", "p_decision_age", "p_decision_sex", "p_a_trendfresh", "p_c_trendfresh", "p_a_trendboiled", "p_c_trendboiled", "p_a_trendsteamed", "p_c_trendsteamed", "p_a_trendfried", "p_c_trendfried", "p_a_trendroasted", "p_c_trendroasted", "p_a_trendothers", "p_c_trendothers", "p_form_bought", "p_form_bought.Fresh", "p_form_bought.Boiled", "p_form_bought.Steamed", "p_form_bought.Fried", "p_form_bought.Roasted", "p_form_bought.Others", "p_form_bought.None", "p_form_bought_othr", "m_decision_how", "p_decision_how", "challenges_decision", "challenges_descrb", "diff_meal", "diff_meal_circum", "note_6", "m_taboo", "m_taboo1", "m_taboo1_age", "m_taboo1_sex", "m_taboo1_sex.1", "m_taboo1_sex.0", "m_taboo1_apply", "m_taboo2_yes", "m_taboo2", "m_taboo2_age", "m_taboo2_sex", "m_taboo2_sex.1", "m_taboo2_sex.0", "m_taboo2_apply", "m_taboo3_yes", "m_taboo3", "m_taboo3_age", "m_taboo3_sex", "m_taboo3_sex.1", "m_taboo3_sex.0", "m_taboo3_apply", "note_7", "p_taboo", "p_taboo1", "p_taboo1_age", "p_taboo1_sex", "p_taboo1_sex.1", "p_taboo1_sex.0", "p_taboo1_apply", "p_taboo2_yes", "p_taboo2", "p_taboo2_age", "p_taboo2_sex", "p_taboo2_sex.1", "p_taboo2_sex.0", "p_taboo2_apply", "p_taboo3_yes", "p_taboo3", "p_taboo3_age", "p_taboo3_sex", "p_taboo3_sex.1", "p_taboo3_sex.0", "p_taboo3_apply", "note_8", "m_quality_buy", "m_quality_buy.Maturity", "m_quality_buy.Freshness", "m_quality_buy.Finger_size", "m_quality_buy.Bunch_size", "m_quality_buy.Compact_bunch", "m_quality_buy.Size_of_the_peduncle", "m_quality_buy.Peel_colour_appearance", "m_quality_buy.Finger_shape", "m_quality_buy.Amount_of_sap", "m_quality_buy.Flesh_colour", "m_quality_buy.Shelf_life", "m_quality_buy.Market_price", "m_quality_buy.Cultural_attachment", "m_quality_prep.Easy_to_peel", "m_quality_prep.Amount_of_sap", "m_quality_prep.not_browning_drying", "m_quality_prep.Flesh_colour", "m_quality_prep.Fruit_detachability", "m_quality_prep.Cookability", "m_quality_prep.Finger_size", "m_quality_prep.Aroma_smell", "m_quality_prep.Easy_mash", "m_quality_prep.Shape_fingers", "m_quality_prep.Freshness", "m_quality_prep.Bruise_status", "note_9", "m_less_pref_buy", "m_less_pref_buy.Small_fingers", "m_less_pref_buy.Yellow_as_if_ripe", "m_less_pref_buy.Pale_on_top", "m_less_pref_buy.black_spots_on_top_of_fingers", "m_less_pref_buy.Varieties", "m_less_pref_buy.Ripen_fast", "m_less_pref_buy.Big_fatty_peel", "m_less_pref_buy.Light_weight", "m_less_pref_buy.Finger_shape", "m_less_pref_buy_other", "m_less_pref_prep", "m_less_pref_prep.Fingers_black_hard_middle", "m_less_pref_prep.sappy_and_astringent_taste_when_raw", "m_less_pref_prep.cooks_for_long", "m_less_pref_prep.hard_to_mash", "m_less_pref_prep.Aroma_smell", "m_less_pref_prep.Hard_to_peel", "m_less_pref_prep_other", "m_less_pref_cons", "m_less_pref_cons.Colour", "m_less_pref_cons.Does_not_stick_together", "m_less_pref_cons.Cools_fast", "m_less_pref_cons.Hard_when_cooked", "m_less_pref_cons.sticky_in_the_mouth", "m_less_pref_cons.Watery_taste", "m_less_pref_cons.Black_spots_when_cooked", "m_less_pref_cons_other", "note_10", "p_quality_buy", "p_quality_buy.Variety_cultivar", "p_quality_buy.Size_of_tuber", "p_quality_buy.Peel_colour", "p_quality_buy.Peel_firmness", "p_quality_buy.Freshness", "p_quality_buy.Amount_of_sap", "p_quality_buy.Market_price", "p_quality_buy.Shelf_life", "p_quality_buy.No_sprouting", "p_quality_buy.Peel_texture", "p_quality_prep", "p_quality_prep.Easy_to_peel", "p_quality_prep.Amount_of_sap", "p_quality_prep.Flesh_colour", "p_quality_prep.Cookability", "p_quality_prep.Tuber_size", "p_quality_prep.Doesnt_change_colour", "p_quality_prep.firmness", "p_quality_prep.arom_smell", "p_quality_prep.Easyto_mash", "p_quality_prep.tuber_shape", "p_quality_prep.freshness", "p_quality_prep.damage_status", "note_11", "p_less_pref_buy", "p_less_pref_buy.Grown_in_swampy_areas", "p_less_pref_buy.Rotten", "p_less_pref_buy.Has_black_spots_on_the_peel", "p_less_pref_buy.Infected_roots", "p_less_pref_buy.white_fleshed", "p_less_pref_buy.Pale_yellow_flesh", "p_less_pref_buy.Peel_texture", "p_less_pref_buy.Dead_buds", "p_less_pref_buy.Oozing_milky_substance", "p_less_pref_buy.Light_weight", "p_less_pref_buy.Tuber_shape", "p_less_pref_buy.Varieties", "p_less_pref_buy_other", "p_less_pref_prep", "p_less_pref_prep.Has_black_spots_on_flesh", "p_less_pref_prep.takes_up_water_when_you_boil_it", "p_less_pref_prep.Does_not_cook_well", "p_less_pref_prep.White_pale_yellow_fleshed", "p_less_pref_prep.Aroma_smell", "p_less_pref_prep.Taste", "p_less_pref_prep.Not_mashey", "p_less_pref_prep.Purple_rings", "p_less_pref_prep.Difficult_to_peel", "p_less_pref_prep_other", "p_less_pref_cons", "p_less_pref_cons.Has_fibre", "p_less_pref_cons.soggy_when_eaten", "p_less_pref_cons.Soft", "p_less_pref_cons.Taste", "p_less_pref_cons.Does_not_cook_well", "p_less_pref_cons.Aroma", "p_less_pref_cons_other", "note_12", "store_matooke", "m_days_store", "m_days_store_effect", "m_forms_strg_affected", "m_forms_strg_affected.Fresh", "m_forms_strg_affected.Boiled", "m_forms_strg_affected.Steamed", "m_forms_strg_affected.Fried", "m_forms_strg_affected.Roasted", "m_forms_strg_affected.others", "m_forms_strg_affected_other", "store_potatoes", "p_days_store", "p_days_store_effect", "p_forms_strg_affected", "p_forms_strg_affected.Fresh", "p_forms_strg_affected.Boiled", "p_forms_strg_affected.Steamed", "p_forms_strg_affected.Fried", "p_forms_strg_affected.Roasted", "p_forms_strg_affected.others", "p_forms_strg_affected_other", "GPS", "_GPS_precision", "thankyou", "_id", "_uuid", "_submission_time", "_validation_status", "_index"


	d1b <- data.frame()
##r1b: "m_qualityb", "Specific_m_quality_buy", "m_sensory_buy", "m_sensory_buy.1", "m_sensory_buy.2", "m_sensory_buy.3", "m_sensory_buy.4", "m_sensory_buy.5", "m_quality_buy_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1c <- data.frame(
		rep = r1c[["m_quality_prepn"]]
	)
##r1c: "Specific_m_quality_prep", "m_sensory_prep", "m_sensory_prep.1", "m_sensory_prep.2", "m_sensory_prep.3", "m_sensory_prep.4", "m_sensory_prep.5", "m_quality_prep_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1d <- data.frame()
##r1d: "m_less_pref", "Specific_m_less_pref_buy", "m_less_pref_sensory_buy", "m_less_pref_sensory_buy.1", "m_less_pref_sensory_buy.2", "m_less_pref_sensory_buy.3", "m_less_pref_sensory_buy.4", "m_less_pref_sensory_buy.5", "m_less_pref_buy_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1e <- data.frame(
		rep = r1e[["m_less_pref_prepn"]]
	)
##r1e: "Specific_m_less_pref_prep", "m_less_pref_sensory_prep", "m_less_pref_sensory_prep.1", "m_less_pref_sensory_prep.2", "m_less_pref_sensory_prep.3", "m_less_pref_sensory_prep.4", "m_less_pref_sensory_prep.5", "m_less_pref_prep_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1f <- data.frame()
##r1f: "m_less_pref_consn", "Specific_m_less_pref_cons", "m_less_pref_sensory_cons", "m_less_pref_sensory_cons.1", "m_less_pref_sensory_cons.2", "m_less_pref_sensory_cons.3", "m_less_pref_sensory_cons.4", "m_less_pref_sensory_cons.5", "m_less_pref_cons_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1g <- data.frame(
		P_fertilizer = r1g[["p_qualityb"]]
	)
##r1g: "Specific_p_quality_buy", "p_sensory_buy", "p_sensory_buy.1", "p_sensory_buy.2", "p_sensory_buy.3", "p_sensory_buy.4", "p_sensory_buy.5", "p_quality_buy_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1h <- data.frame(
		rep = r1h[["p_quality_prepn"]],
		P_fertilizer = r1h[["p_quality_prepn"]]
	)
##r1h: "Specific_p_quality_prep", "p_sensory_prep", "p_sensory_prep.1", "p_sensory_prep.2", "p_sensory_prep.3", "p_sensory_prep.4", "p_sensory_prep.5", "p_quality_prep_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1i <- data.frame(
		P_fertilizer = r1i[["p_less_prefb"]]
	)
##r1i: "Specific_p_less_pref_buy", "p_less_pref_sensory_buy", "p_less_pref_sensory_buy.1", "p_less_pref_sensory_buy.2", "p_less_pref_sensory_buy.3", "p_less_pref_sensory_buy.4", "p_less_pref_sensory_buy.5", "p_less_pref_buy_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1j <- data.frame(
		rep = r1j[["p_less_pref_prepn"]],
		P_fertilizer = r1j[["p_less_pref_prepn"]]
	)
##r1j: "Specific_p_less_pref_prep", "p_less_pref_sensory_prep", "p_less_pref_sensory_prep.1", "p_less_pref_sensory_prep.2", "p_less_pref_sensory_prep.3", "p_less_pref_sensory_prep.4", "p_less_pref_sensory_prep.5", "p_less_pref_prep_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1k <- data.frame(
		P_fertilizer = r1k[["p_less_pref_consn"]]
	)
##r1k: "Specific_p_less_pref_cons", "p_less_pref_sensory_cons", "p_less_pref_sensory_cons.1", "p_less_pref_sensory_cons.2", "p_less_pref_sensory_cons.3", "p_less_pref_sensory_cons.4", "p_less_pref_sensory_cons.5", "p_less_pref_cons_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1l <- data.frame(
		soil_texture = tolower(r1l[["m_xtcs_strg_affected.texture"]]),
		rep = r1l[["m_xtcs_strg_affected.form_preparation"]]
	)
##r1l: "m_forms_strg_affected_slctd", "specifc_m_forms_strg_affected", "m_days_strg_affected", "m_xtcs_strg_affected", "m_xtcs_strg_affected.colour", "m_xtcs_strg_affected.flavour", "m_xtcs_strg_affected.aroma_smell", "m_xtcs_strg_affected.taste", "m_xtcs_strg_affected.others", "m_xtcs_strg_affected_other", "m_xtcs_strg_affected_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d1m <- data.frame(
		P_fertilizer = r1m[["p_forms_strg_affected_slctd"]],
		soil_texture = tolower(r1m[["p_xtcs_strg_affected.texture"]]),
		rep = r1m[["p_xtcs_strg_affected.form_preparation"]]
	)
##r1m: "specifc_p_forms_strg_affected", "p_days_strg_affected", "p_xtcs_strg_affected", "p_xtcs_strg_affected.colour", "p_xtcs_strg_affected.flavour", "p_xtcs_strg_affected.aroma_smell", "p_xtcs_strg_affected.taste", "p_xtcs_strg_affected.others", "p_xtcs_strg_affected_other", "p_xtcs_strg_affected_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2a <- data.frame(
		adm2 = carobiner::fix_name(r2a[["district"]], "title"),
		location = r2a[["village"]],
		P_fertilizer = r2a[["p_sensory_cons_clr"]],
		variety = r2a[["m_quality_buy.Variety"]],
		rep = r2a[["m_quality_prep"]],
		soil_texture = tolower(r2a[["m_less_pref_prep.Peel_texture"]]),
		latitude = r2a[["_GPS_latitude"]],
		longitude = r2a[["_GPS_longitude"]],
		elevation = r2a[["_GPS_altitude"]]
	)
##r2a: "start", "end", "today", "consent", "ID", "division", "other_division", "parish", "other_parish", "code_name", "sex", "education", "age", "tribe", "other_tribe", "marital_status", "religion", "occupation", "self_employment", "other_occupation", "relationship_to_head", "other_relationship", "has_children", "note_1", "Q1", "Q1.1", "Q1.2", "Q1.3", "Q1.4", "Q1.5", "Q1.6", "Q1.7", "Q1_Other", "Q1_label", "Q1a", "Q1b", "Q1c", "Q1d", "Q1e", "Q1f", "Q1g", "Q1a_reason", "m_sensory_cons_clr", "m_sensory_cons_clr.1", "m_sensory_cons_clr.2", "m_sensory_cons_clr.3", "m_sensory_cons_clr.4", "m_sensory_cons_clr.5", "m_sensory_cons_clr_des", "Q1b_reason", "m_sensory_cons_flvr", "m_sensory_cons_flvr.1", "m_sensory_cons_flvr.2", "m_sensory_cons_flvr.3", "m_sensory_cons_flvr.4", "m_sensory_cons_flvr.5", "m_sensory_cons_flvr_des", "Q1c_reason", "m_sensory_cons_txt", "m_sensory_cons_txt.1", "m_sensory_cons_txt.2", "m_sensory_cons_txt.3", "m_sensory_cons_txt.4", "m_sensory_cons_txt.5", "m_sensory_cons_txt_des", "Q1d_reason", "m_sensory_cons_arm", "m_sensory_cons_arm.1", "m_sensory_cons_arm.2", "m_sensory_cons_arm.3", "m_sensory_cons_arm.4", "m_sensory_cons_arm.5", "m_sensory_cons_arm_des", "Q1e_reason", "m_sensory_cons_frmp", "m_sensory_cons_frmp.1", "m_sensory_cons_frmp.2", "m_sensory_cons_frmp.3", "m_sensory_cons_frmp.4", "m_sensory_cons_frmp.5", "m_sensory_cons_frmp_des", "Q1f_reason", "m_sensory_cons_tst", "m_sensory_cons_tst.1", "m_sensory_cons_tst.2", "m_sensory_cons_tst.3", "m_sensory_cons_tst.4", "m_sensory_cons_tst.5", "m_sensory_cons_tst_des", "Q1g_reason", "m_sensory_cons_othr", "m_sensory_cons_othr.1", "m_sensory_cons_othr.2", "m_sensory_cons_othr.3", "m_sensory_cons_othr.4", "m_sensory_cons_othr.5", "m_sensory_cons_othr_des", "m_spouse_consume", "m_same_xtics_as_spouse", "m_same_xtics_as_spouse_diff", "Q2", "Q2.1", "Q2.2", "Q2.3", "Q2.4", "Q2.5", "Q2.6", "Q2.7", "Q2_Other", "Q2_label", "Q2a", "Q2b", "Q2c", "Q2d", "Q2e", "Q2f", "Q2g", "m_children_consume", "m_same_xtics_as_children", "m_same_xtics_as_child_diff", "Q3", "Q3.1", "Q3.2", "Q3.3", "Q3.4", "Q3.5", "Q3.6", "Q3.7", "Q3_Other", "Q3_label", "Q3a", "Q3b", "Q3c", "Q3d", "Q3e", "Q3f", "Q3g", "note_2", "Q4", "Q4.1", "Q4.2", "Q4.3", "Q4.4", "Q4.5", "Q4.6", "Q4.7", "Q4_Other", "Q4_label", "Q4a", "Q4b", "Q4c", "Q4d", "Q4e", "Q4f", "Q4g", "Q4a_reason", "p_sensory_cons_clr.1", "p_sensory_cons_clr.2", "p_sensory_cons_clr.3", "p_sensory_cons_clr.4", "p_sensory_cons_clr.5", "p_sensory_cons_clr_des", "Q4b_reason", "p_sensory_cons_flvr", "p_sensory_cons_flvr.1", "p_sensory_cons_flvr.2", "p_sensory_cons_flvr.3", "p_sensory_cons_flvr.4", "p_sensory_cons_flvr.5", "p_sensory_cons_flvr_des", "Q4c_reason", "p_sensory_cons_txt", "p_sensory_cons_txt.1", "p_sensory_cons_txt.2", "p_sensory_cons_txt.3", "p_sensory_cons_txt.4", "p_sensory_cons_txt.5", "p_sensory_cons_txt_des", "Q4d_reason", "p_sensory_cons_arm", "p_sensory_cons_arm.1", "p_sensory_cons_arm.2", "p_sensory_cons_arm.3", "p_sensory_cons_arm.4", "p_sensory_cons_arm.5", "p_sensory_cons_arp_des", "Q4e_reason", "p_sensory_cons_frmp", "p_sensory_cons_frmp.1", "p_sensory_cons_frmp.2", "p_sensory_cons_frmp.3", "p_sensory_cons_frmp.4", "p_sensory_cons_frmp.5", "p_sensory_cons_frmp_des", "Q4f_reason", "p_sensory_cons_tst", "p_sensory_cons_tst.1", "p_sensory_cons_tst.2", "p_sensory_cons_tst.3", "p_sensory_cons_tst.4", "p_sensory_cons_tst.5", "p_sensory_cons_tst_des", "Q4g_reason", "p_sensory_cons_othr", "p_sensory_cons_othr.1", "p_sensory_cons_othr.2", "p_sensory_cons_othr.3", "p_sensory_cons_othr.4", "p_sensory_cons_othr.5", "p_sensory_cons_othr_des", "p_spouse_consume", "p_same_xtics_as_spouse", "p_same_xtics_as_spouse_diff", "Q5", "Q5.1", "Q5.2", "Q5.3", "Q5.4", "Q5.5", "Q5.6", "Q5.7", "Q5_Other", "Q5_label", "Q5a", "Q5b", "Q5c", "Q5d", "Q5e", "Q5f", "Q5g", "p_children_consume", "p_same_xtics_as_children", "p_same_xtics_as_child_diff", "Q6", "Q6.1", "Q6.2", "Q6.3", "Q6.4", "Q6.5", "Q6.6", "Q6.7", "Q6_Other", "Q6_label", "Q6a", "Q6b", "Q6c", "Q6d", "Q6e", "Q6f", "Q6g", "note_3", "Q7", "Q7.1", "Q7.2", "Q7.3", "Q7.4", "Q7.5", "Q7.6", "Q7_Other", "m_forms_adult", "m_forms_adult.Fresh", "m_forms_adult.Boiled", "m_forms_adult.Steamed", "m_forms_adult.Fried", "m_forms_adult.Roasted", "m_forms_adult.Others", "m_forms_adult.None", "m_forms_children", "m_forms_children.Fresh", "m_forms_children.Boiled", "m_forms_children.Steamed", "m_forms_children.Fried", "m_forms_children.Roasted", "m_forms_children.Others", "m_forms_children.None", "Q7_label", "Q7a", "Q7b", "Q7c", "Q7d", "Q7e", "Q7f", "note_4", "Q8_label", "Q8a", "Q8b", "Q8c", "Q8d", "Q8e", "Q8f", "m_decision_mbr", "m_other_member", "m_decision_age", "m_decision_sex", "m_a_trendfresh", "m_c_trendfresh", "m_a_trendboiled", "m_c_trendboiled", "m_a_trendsteamed", "m_c_trendsteamed", "m_a_trendfried", "m_c_trendfried", "m_a_trendroasted", "m_c_trendroasted", "m_a_trendothers", "m_c_trendothers", "m_form_bought", "m_form_bought.Fresh", "m_form_bought.Boiled", "m_form_bought.Steamed", "m_form_bought.Fried", "m_form_bought.Roasted", "m_form_bought.Others", "m_form_bought.None", "m_form_bought_othr", "Q9", "Q9.1", "Q9.2", "Q9.3", "Q9.4", "Q9.5", "Q9.6", "Q9_Other", "p_forms_adult", "p_forms_adult.Fresh", "p_forms_adult.Boiled", "p_forms_adult.Steamed", "p_forms_adult.Fried", "p_forms_adult.Roasted", "p_forms_adult.Others", "p_forms_adult.None", "p_forms_children", "p_forms_children.Fresh", "p_forms_children.Boiled", "p_forms_children.Steamed", "p_forms_children.Fried", "p_forms_children.Roasted", "p_forms_children.Others", "p_forms_children.None", "Q9_label", "Q9a", "Q9b", "Q9c", "Q9d", "Q9e", "Q9f", "note_5", "Q10_label", "Q10a", "Q10b", "Q10c", "Q10d", "Q10e", "Q10f", "p_decision_mbr", "p_other_member", "p_decision_age", "p_decision_sex", "p_a_trendfresh", "p_c_trendfresh", "p_a_trendboiled", "p_c_trendboiled", "p_a_trendsteamed", "p_c_trendsteamed", "p_a_trendfried", "p_c_trendfried", "p_a_trendroasted", "p_c_trendroasted", "p_a_trendothers", "p_c_trendothers", "p_form_bought", "p_form_bought.Fresh", "p_form_bought.Boiled", "p_form_bought.Steamed", "p_form_bought.Fried", "p_form_bought.Roasted", "p_form_bought.Others", "p_form_bought.None", "p_form_bought_othr", "m_decision_how", "p_decision_how", "challenges_decision", "challenges_descrb", "diff_meal", "diff_meal_circum", "note_6", "m_taboo", "m_taboo1", "m_taboo1_age", "m_taboo1_sex", "m_taboo1_sex.1", "m_taboo1_sex.0", "m_taboo1_apply", "m_taboo2_yes", "m_taboo2", "m_taboo2_age", "m_taboo2_sex", "m_taboo2_sex.1", "m_taboo2_sex.0", "m_taboo2_apply", "m_taboo3_yes", "m_taboo3", "m_taboo3_age", "m_taboo3_sex", "m_taboo3_sex.1", "m_taboo3_sex.0", "m_taboo3_apply", "note_7", "p_taboo", "p_taboo1", "p_taboo1_age", "p_taboo1_sex", "p_taboo1_sex.1", "p_taboo1_sex.0", "p_taboo1_apply", "p_taboo2_yes", "p_taboo2", "p_taboo2_age", "p_taboo2_sex", "p_taboo2_sex.1", "p_taboo2_sex.0", "p_taboo2_apply", "p_taboo3_yes", "p_taboo3", "p_taboo3_age", "p_taboo3_sex", "p_taboo3_sex.1", "p_taboo3_sex.0", "p_taboo3_apply", "note_8", "m_quality_buy", "m_quality_buy.Maturity", "m_quality_buy.Freshness", "m_quality_buy.Finger_size", "m_quality_buy.Bunch_size", "m_quality_buy.Compact_bunch", "m_quality_buy.Size_of_the_peduncle", "m_quality_buy.Peel_colour_appearance", "m_quality_buy.Finger_shape", "m_quality_buy.Amount_of_sap", "m_quality_buy.Flesh_colour", "m_quality_buy.Shelf_life", "m_quality_buy.Market_price", "m_quality_buy.Cultural_attachment", "m_quality_prep.Easy_to_peel", "m_quality_prep.Amount_of_sap", "m_quality_prep.not_browning_drying", "m_quality_prep.Flesh_colour", "m_quality_prep.Fruit_detachability", "m_quality_prep.Cookability", "m_quality_prep.Finger_size", "m_quality_prep.Aroma_smell", "m_quality_prep.Easy_mash", "m_quality_prep.Shape_fingers", "m_quality_prep.Freshness", "m_quality_prep.Bruise_status", "note_9", "m_less_pref_buy", "m_less_pref_buy.Small_fingers", "m_less_pref_buy.Yellow_as_if_ripe", "m_less_pref_buy.Pale_on_top", "m_less_pref_buy.black_spots_on_top_of_fingers", "m_less_pref_buy.Varieties", "m_less_pref_buy.Ripen_fast", "m_less_pref_buy.Big_fatty_peel", "m_less_pref_buy.Light_weight", "m_less_pref_buy.Finger_shape", "m_less_pref_buy_other", "m_less_pref_prep", "m_less_pref_prep.Fingers_black_hard_middle", "m_less_pref_prep.sappy_and_astringent_taste_when_raw", "m_less_pref_prep.cooks_for_long", "m_less_pref_prep.hard_to_mash", "m_less_pref_prep.Aroma_smell", "m_less_pref_prep.Hard_to_peel", "m_less_pref_prep_other", "m_less_pref_cons", "m_less_pref_cons.Colour", "m_less_pref_cons.Does_not_stick_together", "m_less_pref_cons.Cools_fast", "m_less_pref_cons.Hard_when_cooked", "m_less_pref_cons.sticky_in_the_mouth", "m_less_pref_cons.Watery_taste", "m_less_pref_cons.Black_spots_when_cooked", "m_less_pref_cons_other", "note_10", "p_quality_buy", "p_quality_buy.Variety_cultivar", "p_quality_buy.Size_of_tuber", "p_quality_buy.Peel_colour", "p_quality_buy.Peel_firmness", "p_quality_buy.Freshness", "p_quality_buy.Amount_of_sap", "p_quality_buy.Market_price", "p_quality_buy.Shelf_life", "p_quality_buy.No_sprouting", "p_quality_buy.Peel_texture", "p_quality_prep", "p_quality_prep.Easy_to_peel", "p_quality_prep.Amount_of_sap", "p_quality_prep.Flesh_colour", "p_quality_prep.Cookability", "p_quality_prep.Tuber_size", "p_quality_prep.Doesnt_change_colour", "p_quality_prep.firmness", "p_quality_prep.arom_smell", "p_quality_prep.Easyto_mash", "p_quality_prep.tuber_shape", "p_quality_prep.freshness", "p_quality_prep.damage_status", "note_11", "p_less_pref_buy", "p_less_pref_buy.Grown_in_swampy_areas", "p_less_pref_buy.Rotten", "p_less_pref_buy.Has_black_spots_on_the_peel", "p_less_pref_buy.Infected_roots", "p_less_pref_buy.white_fleshed", "p_less_pref_buy.Pale_yellow_flesh", "p_less_pref_buy.Peel_texture", "p_less_pref_buy.Dead_buds", "p_less_pref_buy.Oozing_milky_substance", "p_less_pref_buy.Light_weight", "p_less_pref_buy.Tuber_shape", "p_less_pref_buy.Varieties", "p_less_pref_buy_other", "p_less_pref_prep", "p_less_pref_prep.Has_black_spots_on_flesh", "p_less_pref_prep.takes_up_water_when_you_boil_it", "p_less_pref_prep.Does_not_cook_well", "p_less_pref_prep.White_pale_yellow_fleshed", "p_less_pref_prep.Aroma_smell", "p_less_pref_prep.Taste", "p_less_pref_prep.Not_mashey", "p_less_pref_prep.Purple_rings", "p_less_pref_prep.Difficult_to_peel", "p_less_pref_prep_other", "p_less_pref_cons", "p_less_pref_cons.Has_fibre", "p_less_pref_cons.soggy_when_eaten", "p_less_pref_cons.Soft", "p_less_pref_cons.Taste", "p_less_pref_cons.Does_not_cook_well", "p_less_pref_cons.Aroma", "p_less_pref_cons_other", "note_12", "store_matooke", "m_days_store", "m_days_store_effect", "m_forms_strg_affected", "m_forms_strg_affected.Fresh", "m_forms_strg_affected.Boiled", "m_forms_strg_affected.Steamed", "m_forms_strg_affected.Fried", "m_forms_strg_affected.Roasted", "m_forms_strg_affected.others", "m_forms_strg_affected_other", "store_potatoes", "p_days_store", "p_days_store_effect", "p_forms_strg_affected", "p_forms_strg_affected.Fresh", "p_forms_strg_affected.Boiled", "p_forms_strg_affected.Steamed", "p_forms_strg_affected.Fried", "p_forms_strg_affected.Roasted", "p_forms_strg_affected.others", "p_forms_strg_affected_other", "GPS", "_GPS_precision", "thankyou", "_id", "_uuid", "_submission_time", "_validation_status", "_index"


	d2b <- data.frame()
##r2b: "m_qualityb", "Specific_m_quality_buy", "m_sensory_buy", "m_sensory_buy.1", "m_sensory_buy.2", "m_sensory_buy.3", "m_sensory_buy.4", "m_sensory_buy.5", "m_quality_buy_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2c <- data.frame(
		rep = r2c[["m_quality_prepn"]]
	)
##r2c: "Specific_m_quality_prep", "m_sensory_prep", "m_sensory_prep.1", "m_sensory_prep.2", "m_sensory_prep.3", "m_sensory_prep.4", "m_sensory_prep.5", "m_quality_prep_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2d <- data.frame()
##r2d: "m_less_pref", "Specific_m_less_pref_buy", "m_less_pref_sensory_buy", "m_less_pref_sensory_buy.1", "m_less_pref_sensory_buy.2", "m_less_pref_sensory_buy.3", "m_less_pref_sensory_buy.4", "m_less_pref_sensory_buy.5", "m_less_pref_buy_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2e <- data.frame(
		rep = r2e[["m_less_pref_prepn"]]
	)
##r2e: "Specific_m_less_pref_prep", "m_less_pref_sensory_prep", "m_less_pref_sensory_prep.1", "m_less_pref_sensory_prep.2", "m_less_pref_sensory_prep.3", "m_less_pref_sensory_prep.4", "m_less_pref_sensory_prep.5", "m_less_pref_prep_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2f <- data.frame()
##r2f: "m_less_pref_consn", "Specific_m_less_pref_cons", "m_less_pref_sensory_cons", "m_less_pref_sensory_cons.1", "m_less_pref_sensory_cons.2", "m_less_pref_sensory_cons.3", "m_less_pref_sensory_cons.4", "m_less_pref_sensory_cons.5", "m_less_pref_cons_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2g <- data.frame(
		P_fertilizer = r2g[["p_qualityb"]]
	)
##r2g: "Specific_p_quality_buy", "p_sensory_buy", "p_sensory_buy.1", "p_sensory_buy.2", "p_sensory_buy.3", "p_sensory_buy.4", "p_sensory_buy.5", "p_quality_buy_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2h <- data.frame(
		rep = r2h[["p_quality_prepn"]],
		P_fertilizer = r2h[["p_quality_prepn"]]
	)
##r2h: "Specific_p_quality_prep", "p_sensory_prep", "p_sensory_prep.1", "p_sensory_prep.2", "p_sensory_prep.3", "p_sensory_prep.4", "p_sensory_prep.5", "p_quality_prep_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2i <- data.frame(
		P_fertilizer = r2i[["p_less_prefb"]]
	)
##r2i: "Specific_p_less_pref_buy", "p_less_pref_sensory_buy", "p_less_pref_sensory_buy.1", "p_less_pref_sensory_buy.2", "p_less_pref_sensory_buy.3", "p_less_pref_sensory_buy.4", "p_less_pref_sensory_buy.5", "p_less_pref_buy_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2j <- data.frame(
		rep = r2j[["p_less_pref_prepn"]],
		P_fertilizer = r2j[["p_less_pref_prepn"]]
	)
##r2j: "Specific_p_less_pref_prep", "p_less_pref_sensory_prep", "p_less_pref_sensory_prep.1", "p_less_pref_sensory_prep.2", "p_less_pref_sensory_prep.3", "p_less_pref_sensory_prep.4", "p_less_pref_sensory_prep.5", "p_less_pref_prep_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2k <- data.frame(
		P_fertilizer = r2k[["p_less_pref_consn"]]
	)
##r2k: "Specific_p_less_pref_cons", "p_less_pref_sensory_cons", "p_less_pref_sensory_cons.1", "p_less_pref_sensory_cons.2", "p_less_pref_sensory_cons.3", "p_less_pref_sensory_cons.4", "p_less_pref_sensory_cons.5", "p_less_pref_cons_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2l <- data.frame(
		soil_texture = tolower(r2l[["m_xtcs_strg_affected.texture"]]),
		rep = r2l[["m_xtcs_strg_affected.form_preparation"]]
	)
##r2l: "m_forms_strg_affected_slctd", "specifc_m_forms_strg_affected", "m_days_strg_affected", "m_xtcs_strg_affected", "m_xtcs_strg_affected.colour", "m_xtcs_strg_affected.flavour", "m_xtcs_strg_affected.aroma_smell", "m_xtcs_strg_affected.taste", "m_xtcs_strg_affected.others", "m_xtcs_strg_affected_other", "m_xtcs_strg_affected_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d2m <- data.frame(
		P_fertilizer = r2m[["p_forms_strg_affected_slctd"]],
		soil_texture = tolower(r2m[["p_xtcs_strg_affected.texture"]]),
		rep = r2m[["p_xtcs_strg_affected.form_preparation"]]
	)
##r2m: "specifc_p_forms_strg_affected", "p_days_strg_affected", "p_xtcs_strg_affected", "p_xtcs_strg_affected.colour", "p_xtcs_strg_affected.flavour", "p_xtcs_strg_affected.aroma_smell", "p_xtcs_strg_affected.taste", "p_xtcs_strg_affected.others", "p_xtcs_strg_affected_other", "p_xtcs_strg_affected_rsn", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "_submission__submission_time", "_submission__validation_status"


	d3a <- data.frame(
		rep = r3a[["repeat_count"]]
	)
##r3a: "type", "name", "label", "required", "relevant", "appearance", "choice_filter", "constraint", "constraint_message", "calculation"


	d3b <- data.frame()
##r3b: "list_name", "name", "label", "rank_limit"


	d3c <- data.frame()
##r3c: "id_string", "version"


	d4a <- data.frame(
		rep = r4a[["repeat_count"]]
	)
##r4a: "type", "name", "label", "hint", "required", "relevant", "appearance", "choice_filter", "constraint", "constraint_message", "calculation"


	d4b <- data.frame()
##r4b: "list_name", "name", "label", "rank_limit"


	d4c <- data.frame()
##r4c: "id_string", "version"


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

