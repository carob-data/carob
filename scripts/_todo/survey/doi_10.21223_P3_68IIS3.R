# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline household survey for the 2016 OFDA sweetpotato project in southern Mozambique. This script uses the household location module. hh_id is unique.
# The codebook codes household-head sex as 1 = Masculino and 2 = Feminino. Province, district, administrative post, and village are columns.
# Country is Mozambique in the geographic coverage. Altitude is meters.

## ISSUES
# Latitude and longitude are not decimal degrees. Digit length and minutes are inconsistent, so they were not converted.
# Admin and village spellings are as recorded and were not matched to GADM.
# Interview dates are stored as integers of mixed length.
# missing variables: latitude, longitude

## NOT PROCESSED
# Person names.
# Locality, which sits between the administrative post and the village.
# The other modules (crops, nutrition, climate, livestock, labour, food security) and the version 4 copies.
# Codebook and the Stata log.
# Analise STATA Folha de formulas.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Area_Total_&_Cultivated_Highland_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Area_Total_&_Cultivated_Lowland_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Area_Total_&_Cultivated_Lowland_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Association_Membership_1_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Association_Membership_1_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Association_Membership_Type_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Association_Membership_Type_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_Climatic_Events_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_CYCLONES_HH_Damages_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_CYCLONES_Mitigation_Measures_After_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_CYCLONES_Mitigation_Measures_Before_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_CYCLONES_Occurence_SP_affected_Receive_Info_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_CYCLONES_SP_Damages_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_DROUGHT_HH_Damages_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_DROUGHT_Mitigation_Measures_After_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_DROUGHT_Mitigation_Measures_Before_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_DROUGHT_Occurence_SP_affected_Receive_Info_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_DROUGHT_SP_Damages_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_FLOODS_HH_Damages_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_FLOODS_Mitigation_Measures_After_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_FLOODS_Mitigation_Measures_Before_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_FLOODS_Mitigation_Measures_Before_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_FLOODS_Occurence_SP_affected_Receive_Info_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CC_FLOODS_SP_Damages_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Codebook for the Mz OFDA Baseline Survey 2016.log: documentation or a codebook, not an observation table.
# CONSER_PROCESS_HH_Drying_SP_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CONSER_PROCESS_HH_Know_OFSP_Processing_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CONSER_PROCESS_HH_OFSP_Processing_Methods_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CONSER_PROCESS_HH_OFSP_Processing_Practice_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CONSER_PROCESS_HH_Store_SP_&_Storage_Methods_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# CONSER_PROCESS_OFSP_Processing_Methods_practice_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Coodbook for thr Database_Baseline Survey OFDA Mozambique 2016_South.txt: not read. columns:                                            Database_Baseline_South. Observation records are missing from d until this file is standardized.
# Crop_Production_&_Area_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Crop_Production_&_Area_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Extension_Access_1_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Extension_Access_1_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Extension_Services_Provider_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Extension_Services_Provider_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# FOOD_INSECURE_last_30_days_part_2_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# FOOD_INSECURE_last_30_days_part_2_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# FOOD_INSECURE_last_30_days_part_I_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# FOOD_INSECURE_last_30_days_part_I_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# FOOD_SECURITY_Food_group_intake_7_days_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# FOOD_SECURITY_Nr_meals_bought_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# FOOD_SECURITY_Sold_Food_or_Assets_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# HH_Characteristics_&_Composition_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# HH_Characteristics_&_Composition_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# HH_CONDITIONS_Diseases_Affecting_Adults_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# HH_CONDITIONS_Diseases_Affecting_Children_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# HH_CONDITIONS_Type_Source_Water_Power_v45.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# HH_location_&_Quality_control_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# KAP_Attitude_&_Perceptions_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# KAP_Knowledge_&_Pests_1_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# KAP_Knowledge_&_Pests_2_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# KAP_Pests_Mitigation_Methods_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# KAP_Practices_1_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# KAP_Reasons_for_Good_Planting_Material_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# LABOUR_hire_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# LABOUR_hire_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# LIVESTOCK_Ownership_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# LIVESTOCK_Ownership_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_Food_intake_last_24_hrs_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_Food_intake_last_7_days_v5.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# NUTRITION_Meals_max_min_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_Name_of_HH_Member_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_OFSP_intake_last_24_hrs_qty_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_OFSP_intake_last_24_hrs_source_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_OFSP_intake_last_7_days_qty_period_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_OFSP_intake_last_7_days_source_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_Receive_Info_Diet_1_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_Receive_Info_Diet_1_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# NUTRITION_Receive_Info_Diet_Source_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFDA_PROJECT_Benefits_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFDA_PROJECT_HH_Knowledge_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFDA_PROJECT_Purpose_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Buyers_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_HH_Buying_1_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_HH_Monthly_Demand_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Money_Use_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Production_Criteria_Variety_Selection_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Production_Number_of_Varieties_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Production_Source_of_Vines_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Production_Source_of_Vines_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Production_Varieties_Name_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Production_Vines_Dissemination_1_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Production_Vines_Dissemination_Individual_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Production_Vines_Dissemination_Organizations_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Reasons_Leave_Cropping_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Reasons_Leave_Cropping_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Reasons_Never_Cropping_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Reasons_Never_Cropping_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Sale_&_Decision_to_Sale_&_Marketplace_&_Sellers_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# OFSP_Vines_Availability_&_Sales_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_&_OFSP_Harvest_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_&_OFSP_New_Planting_Material_1_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_&_OFSP_New_Planting_Material_2_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_&_OFSP_Planting_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_&_OFSP_Planting_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_&_OFSP_Prices_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_&_OFSP_Producer_&_Planted_Area_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_&_OFSP_Producer_&_Planted_Area_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_&_OFSP_Sale_Gift_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_Production_Field_Owner_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_Roots_Production_1_v4.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# SP_Roots_Production_1_v5.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Dataset for: Baseline Survey for the OFDA 2016 Mitigating Drought Impacts on Livelihoods in Mozambique through Resilient, Nutritious Sweetpotato Project in the South of Mozambique
"

	uri <- "doi:10.21223/P3/68IIS3"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "HH_location_&_Quality_control_v5.dta"]
	r <- haven::read_dta(f)
	txt <- function(x) {
		if (inherits(x, "haven_labelled") || is.factor(x)) {
			x <- haven::as_factor(x)
		}
		x <- as.character(x)
		bad <- !is.na(x) & is.na(iconv(x, from = "UTF-8", to = "UTF-8"))
		if (any(bad)) {
			x[bad] <- iconv(x[bad], from = "CP1252", to = "UTF-8")
		}
		trimws(x)
	}
	sex <- c("Masculino" = "male", "Feminino" = "female")[txt(r$gender_hh_head)]

	d <- data.frame(
		trial_id = "OFDA_Mozambique_baseline_2016",
		hhid = txt(r$hh_id),
		country = "Mozambique",
		adm1 = carobiner::fix_name(txt(r$prov), "title"),
		adm2 = carobiner::fix_name(txt(r$dist), "title"),
		adm3 = carobiner::fix_name(txt(r$pa), "title"),
		location = carobiner::fix_name(txt(r$ald), "title"),
		crop = "sweetpotato",
		sex = unname(sex),
		elevation = as.numeric(r$alt),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
