# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Homa Bay preschooler caregiver Survey-data

Background: Persistent prevalence of high malnutrition in poor households in developing countries calls for enhancement of cost-effective nutrition interventions among the vulnerable groups. One responsive way is to promote regular consumption of home-grown biofortified foods, particularly in the micronutrient-deficient groups. Previous nutrition interventions have targeted adults with behavior change education, but have rarely explored the potential of nutrition education of preschoolers as change agents. Objectives: This study sought to assess the effect of nutrition education targeting preschool children and their caregivers on their consumption of vitamin A–biofortified orange-fleshed sweetpotato (OFSP) in rural farm households in Homa Bay County, Kenya. Methods: A total of 431 preschooler-caregiver pairs from 15 village-level clusters were recruited into a randomized controlled trial. The sample was randomized into 1 control (3 villages) and 3 treatment groups (4 villages each). Treatments involved channeling nutrition education to preschoolers through their learning materials (preschooler treatment); the caregivers through their mobile phones (caregiver treatment); and to both preschoolers and their caregivers simultaneously (integrated treatment). Baseline and follow-up household-level surveys were conducted with the caregivers, and consumption data were collected from the preschoolers using a child dietary diversity register. Class teachers sought 24-h consumption recalls of the preschoolers for 19 consecutive schooldays. Results: The results of a zero-inflated Poisson regression showed that the phone-mediated and multichanneled nutrition education approaches significantly increased the number of days of OFSP consumption. The integrated nutrition education approach significantly increased the preschoolers' likelihood to consume OFSP, number of OFSP consumption days, and likelihood to consume it more than once per week by 11%, 77%, and 20%, respectively. Conclusions: Nutrition education through OFSP-branded preschoolers' learning materials and phone-mediated messages provides effective nudges to the caregivers to feed their preschoolers regularly with OFSP. This could have implications for realizing sustainable nutrition programs in biofortified crop-growing areas.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/KJY3XI"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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
	

	f1 <- ff[basename(ff) == "Coordonne_es_GPS_Adamaoua (Dataset_from_Baseline_survey).csv"]
	f2 <- ff[basename(ff) == "Homa_Bay_ECD_CAREGIVERS_SURVEY_DATA (Homa_Bay_ECD_CAREGIVERS_SURVEY_DATA) (1).dta"]
	f3 <- ff[basename(ff) == "DATA_ADAMAOUA (Dataset_from_Baseline_survey).sav"]
	f4 <- ff[basename(ff) == "DATA_ADAMAOUA_PG (Dataset_from_Baseline_survey).sav"]
	f5 <- ff[basename(ff) == "DATA_ADAMAOUA_TECH (Dataset_from_Baseline_survey).sav"]

	r1 <- read.csv(f1)
	r2 <- haven::read_dta(f2)
	#r3 <- read.???(f3)
	#r4 <- read.???(f4)
	#r5 <- read.???(f5)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		N_fertilizer = r1[["N"]]
	)
##r1: "X.1", "X", "Y", "X.2", "E"


	d1 <- data.frame(
		rep = r1[["have_prepared_ofsp"]],
		hhid = r1[["FARMER_GRP"]],
		plot_id = r1[["ofsp_new_plot"]],
		location = r1[["gps_locationlatitude"]],
		latitude = r1[["gps_locationlatitude"]],
		longitude = r1[["gps_locationlongitude"]],
		elevation = r1[["gps_locationaltitude"]],
		year = r1[["SPOUSE_EDU_YEARS"]]
	)
##r1: "center_name", "baseline_partpnt", "haveachild", "enum_name", "quid", "scounty_name", "study_grp", "study_no", "scounty_namer", "child_name", "child_gender", "child_grade", "child_age", "caregiver_name", "caregiver_name2", "resp_contact", "ofsp_awareness", "ofsp_knowledge_source0", "ofsp_knowledge_source_otr", "grew_ofsp", "rsn_nt_plntd", "rsn_nt_plntd_othr", "growing_ofsp", "vines_source", "other_vinessource", "first_grow", "HARVESTED_OFSP", "hvst_freq", "sm", "ms", "ls", "hvst_for", "total_sales", "price", "quantity", "units", "first_time_cooked", "cking_mthd0", "cking_mthdother", "cking_mthd_rank", "no_ofsp_meals_pr_wk", "no_ofsp_meals_pr_wk1", "fed_ofsp_to_child", "first_time_fed", "rsn_feeding_ofsp0", "no_ate_last_wk", "note_b1", "note_b2", "b201", "b201othr", "b202", "b202othr", "b203", "b203othr", "b2040", "b2041", "b204othr", "b205", "b205othr", "note_b3", "GOT_TRTMNT", "TRTMENT_TYPE", "trtmnt_type_othr", "recall", "recall_focus", "recall_focus_othr", "outstanding", "evaluate_dlvry_mde", "generated_table_list_label_104", "reserved_name_for_field_list_lab", "be30a", "be30b", "be30c", "be30d", "be30e", "be30f", "be30g", "be30i", "freq_meeting", "frmr_grp_focus", "sp_acc_cooking0", "othsp_acc_cooking", "OFSP_IN_MKT", "nmbr_oftimes_ofsp_in_mkt1", "nmbr_oftimes_ofsp_in_mkt2", "DVM_AWARE", "DST_DVM", "TRADITIONAL_BLF", "traditional_belief", "held_trad_belief", "reasonbelief", "household_commtn_wealth0", "other_comtn_asset", "child_decsn_food_purc", "child_decsn_food_plant", "child_decsn_food_prd", "GREW_WYFSP", "harvested_wyfsp", "freq_wyfsp_meal", "price_ofsp", "price_wysp", "price_wysp1", "note_d1", "note_d1a", "mrng", "mmrng", "lunch", "aftnoon", "eveng", "generated_table_list_label_166", "v123", "d01", "d02", "d03", "d04", "d05", "d06", "d07", "d08", "d09", "d10", "d11", "d12", "d13", "d14", "d15", "d16", "d17", "d18", "d19", "f33", "f33a", "f33b", "generated_table_list_label_195", "v147", "f34a", "f34b", "f34c", "f34d", "school_feed", "food_school", "food_at_ecd", "plan_diet", "plan_diet_cnsd", "good_eating", "note_f1", "generated_table_list_label_215", "v160", "fa01", "fa02", "fa03", "fa04", "fa05", "fa06", "fa07", "fa08", "fa09", "generated_table_list_label_228", "v171", "faa01", "faa02", "faa03", "faa04", "faa05", "faa06", "faa07", "faa08", "faa09", "begin_g", "ng1", "ng2", "generated_table_list_label_244", "v185", "g101", "g102", "g103", "g104", "g105", "g106", "g107", "g108", "g109", "generated_table_list_label_256", "v196", "g201", "g202", "g203", "g204", "heard_va", "va_source", "va_ben0", "va_ben_otr", "va_ben_ttl", "va_ben_scr", "va_mst_nd0", "va_mst_nd1", "va_mst_nd_otr", "va_mst_nd_ttl", "va_mst_nd_scr", "va_prgnt_wmn", "va_sources0", "va_sources1", "va_sources2", "va_sources_otr", "va_sources_ttl", "va_sourcesscr", "ofp_va_source", "spcf_attde", "generated_table_list_label_283", "v222", "g401", "g402", "g403", "g404", "g405", "g406", "g407", "g408", "g409", "g410", "g411", "generated_table_list_label_296", "v235", "g41201", "g41202", "g41203", "g41204", "g41205", "g41206", "g41207", "g41208", "g41209", "g41210", "g41211", "generated_table_list_label_310", "v248", "g501", "g502", "g503", "g504", "g505", "g506", "g507", "g508", "g509", "g5100", "g5101", "generated_table_list_label_323", "v261", "g511", "g512", "g513", "g514", "g515", "g516", "g517", "g518", "g519", "generated_table_list_label_335", "v272", "g61_01", "g61_02", "g61_03", "g61_04", "g61_05", "g61_06", "g61_07", "g61_08", "g61_09", "g61_09_othr", "generated_table_list_label_348", "v284", "g62_01", "g62_02", "g62_03", "g62_04", "g62_05", "g62_06", "g62_07", "g62_08", "g62_09", "g62_09_othr", "g519_gp", "GENDER_CAREGIVERS", "CAREGIVERS_AGE", "MARITAL_STATUS", "RLTP_HHH", "rltnsp_to_hhhd_other", "rltnsp_to_child", "rltnsp_to_child_other", "OCCUP_CAREGIVER", "SPOUSE_OCCUP", "spouse_edu_level", "generated_table_list_label_397", "h101", "h102", "h103", "h104", "h105", "h106", "h107", "h108", "h109", "h110", "h110a", "HHSIZE", "generated_table_list_label_411", "h121", "h122", "h123", "h124", "h125", "h126", "farm_size_old", "farm_size_new", "gps_locationaccuracy", "gps_location1latitude", "gps_location1longitude", "gps_location1altitude", "gps_location1accuracy", "gps_location2latitude", "gps_location2longitude", "gps_location2altitude", "gps_location2accuracy", "gps_location3latitude", "gps_location3longitude", "gps_location3altitude", "gps_location3accuracy", "gps_location4latitude", "gps_location4longitude", "gps_location4altitude", "gps_location4accuracy", "end", "finish_time", "instanceid", "vines_source0", "va_mst_nd2", "va_sources3", "g5102", "ofsp_knowledge_source1", "cking_mthd1", "cking_mthd2", "v359", "b2042", "va_ben1", "va_ben2", "g510", "rsn_feeding_ofsp1", "b2043", "sp_acc_cooking1", "household_commtn_wealth1", "rsn_feeding_ofsp", "cking_mthd3", "rsn_feeding_ofsp2", "recall_focus0", "va_ben", "va_mst_nd", "va_sources", "ofsp_knowledge_source2", "va_ben3", "household_commtn_wealth2", "b2044", "recall_focus1", "g5103", "va_sources4", "sp_acc_cooking2", "rsn_feeding_ofsp3", "va_ben4", "va_sources5", "va_ben5", "cking_mthd4", "g5104", "recall_focus2", "cking_mthd", "cking_mthd5", "va_mst_nd3", "recall_focus3", "rsn_feeding_ofsp4", "va_mst_nd4", "recall_focus4", "vines_source1", "edulevl", "MARRIED", "SPOUSE_EDU_LEVEL", "check", "OPLOTSIZE", "NPLOTSIZE", "SPOUSE_EDU", "bbe30a", "bbe30b", "bbe30c", "bbe30d", "bbe30e", "bbe30f", "bbe30g", "bbe30i", "gg401", "gg402", "gg403", "gg404", "gg405", "gg406", "gg407", "gg408", "gg409", "gg410", "gg411", "ffaa01", "ffaa02", "ffaa03", "ffaa04", "ffaa05", "ffaa06", "ffaa07", "ffaa08", "ffaa09", "PRICE_OFSP", "PRICE_WFSP", "PRICE_YFSP", "under5", "RANDOM", "lnHHSIZE", "SQRT_HHSIZE", "SQRT_below_5", "EVER_GROWN_OFSP", "DST_MARKET", "DST_HF", "DST_CHV", "GROWING_WYFSP", "WYFSP_HRVST_TIME", "HEARD_VA", "LSIZE", "CAREGIVER_OCCUPATION", "EDU_LEVEL", "MNTHLY_INC", "HFIAS_CAT", "IN_SPOUSE_EDU", "EHFIAS_SCORE", "HFIAS_SCORE", "faaa01", "faaa02", "faaa03", "faaa04", "faaa05", "faaa06", "faaa07", "faaa08", "faaa09", "HFIA_CAT", "sex_HHH", "MNTHLY_EXPEN", "baseline", "_est_orth"


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

