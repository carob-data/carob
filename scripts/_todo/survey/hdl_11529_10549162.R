# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey of agricultural-lime use on farm subplots in Rwanda, 2022 season A. The title and geographic coverage are Rwanda.
# Dry grain or tuber weight is kg per subplot. Yield is that weight divided by subplot area in ha. yield_isfresh is FALSE. The fresh column is a dry-equivalent and is not used.
# Harvest values -99 and 99999 are missing codes. -99 occurs on 12 major-crop rows and 99999 on 3.
# Lime quantity is kg applied over the last three cropping seasons, with 0 when none was applied. lime is that total divided by the current subplot area, so it is not a single-season rate.
# DAP is 18 percent N and 20.1 percent elemental P. Urea is 46 percent N. NPS, NPS with boron or zinc, and NPK blends have no stated grade and are not added to N, P, or K.
# Herbicide quantity is kg or litres, not kg of active ingredient, so only herbicide_used is stored.
# Plot location names are the text CONFIDENTIAL and are not stored.
# Subplot area is in hectares. It is not copied to plot_area, which is square metres for experimental plots.
# Fertilizer, lime, and herbicide were recorded once for the subplot. An intercrop row carries the same subplot totals. They were not split between the two crops.
# Fallow, planted trees, other fruits, other vegetables, and other fodders are not stored.

## ISSUES
# English province names (Northern, Southern, Western) are not spelling variants of the GADM adm1 names (Amajyaruguru, Amajyepfo, Iburengerazuba), so adm1 is left as recorded and is not part of the coordinate lookup. The four district names match GADM adm2 and are unique in Rwanda.
# write_files: out of bounds: yield (0, 465000), lime (0, 87500), N_fertilizer (0, 4020), P_fertilizer (0, 4489)
# write_files: low yield (tons not kg?): cabbage
# write_files: NA detected: yield
# Lime is a three-season total divided by the current subplot area, so some rates exceed a single-season bound. Yield outliers are left as recorded. Harvest codes -99 and 99999 are missing, which leaves yield NA.

## NOT PROCESSED
# Costs, labour, seed source, stress scores, soil descriptions, and payment methods.
# QUANTITY_OF_FERTILIZER_NPK_USE_O and QUANTITY_OF_BLENDED_FERTILIZER_N, because the nutrient grades are not stated.
# The fresh-harvest columns.
# Module_01.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "CELL1", "VILLAGE", "NAME_OF_HOUSEHOLD_HEAD", "SEX_OF_HOUSEHOLD_HEAD", "IS_THE_RESPONDENT_HH_HEAD_1_YES_", "NAME_OF_THE_RESPONDENT_INCLUDE_G", "SEX_OF_RESPONDENT", .... Observation records are missing from d until this file is standardized.
# Module_02.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "ID_CODE", "NAME_OF_HOUSEHOLD_MEMBERS_OF_AGE", "SEX", "RELATION_TO_THE_HOUSEHOLD_HEAD", "AGE_IN_COMPLETE_YEARS", "MARITAL_STATUS", "EDUCATION_IN_YEARS", .... Observation records are missing from d until this file is standardized.
# Module_03.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "VARIABLE_CODE", "INSTITUTION_TYPE", "ARE_YOU_CURRENTLY_A_MEMBER_OF_AN", "YEAR_JOINED_E_C", "HOW_MUCH_INPUT_DO_YOU_HAVE_IN_MA", "IS_YOUR_SPOUSE_A_MEMBER_OF_ANY_O", "YEAR_YOUR_SPOUSE_JOINED_THE_GROU", .... Observation records are missing from d until this file is standardized.
# Module_04.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "DISTANCE_VILLAGEMARK", "DISTANCE_TO_THE_NEAREST_MAIN_MAR", "DISTANCE_TO_THE_NEAREST_SOURCE_O", "DISTANCE_TO_THE_NEAREST_SOURCE_0", "DISTANCE_TO_THE_NEAREST_SOURCE_1", "DISTANCE_TO_THE_NEAREST_AGRICULT", "DISTANCE_TO_THE_NEAREST_CREDIT_S", .... Observation records are missing from d until this file is standardized.
# Module_05.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "HOW_MANY_VILLAGEIN", "MANY_RELATIVE", "MANY_NONRELA", "MANY_OUTSIDERELA", "MANY_OUTSIDENONREL", "FRIENDS_OR_RELATIVES_LEADERSH", "DO_YOU_THINK_YOU_CAN_RELY_ON_GOV", .... Observation records are missing from d until this file is standardized.
# Module_06.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "HAVE_YOU_EVER_USED_LIME_TO_TREAT", "WHEN_WAS_THE_FIRST_TIME_YOU_APPL", "IF_YES_TO_4_1_HOW_DID_YOU_GET_LI", "DID_YOU_APPLY_LIME_ON_YOUR_FARM_", "DID_YOU_DISCONTINUED_TO_APLYING_", "IF_DISCONTINUED_LIME_USE_MAIN_RE", "IF_DISCONTINUED_LIME_USE_MAIN_R0", .... Observation records are missing from d until this file is standardized.
# Module_07.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "YEAR_EC", "YEAR", "APPLIED_LIME", "GRADELIME", "AMOUNT_APPLIED", "PLOTS_SIZE_TREATED_IN_HA", "TOTAL_COST", .... Observation records are missing from d until this file is standardized.
# Module_08.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "DID_YOU_FIRST_HEAR_ABOUT_THE_BEN", "HOW_DID_YOU_FIRST_LEARN_ABOUT_LI", "SPECF", "WHERE_DID_YOU_GET_THE_FIRST_LIME", "SPECFIOTH", "WILL_YOU_CONTINUE_USING_LIME_IN_", "IF_NO_TO_4_5_1ST_RANK_MAIN_REASO", .... Observation records are missing from d until this file is standardized.
# Module_09.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "HAVE_YOU_HEARD_ABOUT_SOIL_ACIDIT", "IF_YES_WHO_TOLD_YOU_ABOUT_THE_PR", "WHICH_YEAR_DID_YOU_LEARN_ABOUT_S", "IS_THERE_A_GOVERNMENT_PRIVATE_LI", "IF_YES_A10_1_4_WHERE_IS_THE_CLOS", "WHAT_IS_THE_APPROXIMATE_DISTANCE", "DO_YOU_KNOW_THE_PRICE_OF_LIME_AT", .... Observation records are missing from d until this file is standardized.
# Module_10.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "S_NO", "ISSUE", "WHAT_IS_YOUR_CURRENT_OPINION_OF_". Observation records are missing from d until this file is standardized.
# Module_11.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "LANDHOLDING", "SUBPLOTS", "SUBPGRE3". Observation records are missing from d until this file is standardized.
# Module_13.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "U1", "U2", "U3", "U5", "U6", "U7", "U8", .... Observation records are missing from d until this file is standardized.
# Module_14.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "CROP1", "MARKET_SOLD", "MONTH_SOLD", "WHO_SOLD", "WHO_MAKE_THE_DECISION_TO_SALE", "WHO_MAKE_THE_DECISION_TO_SPEND", "QUANTITY_SOLD", .... Observation records are missing from d until this file is standardized.
# Module_15.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "CROP", "SEASON1", "BURNT_IN_THE_FIELD", "USED_AS_FIREWOOD", "LEFT_ON_LAND_FOR_SOIL_FERTILITY", "FEED_FOR_LIVESTOCK", "USED_FOR_CONSTRUCTION", .... Observation records are missing from d until this file is standardized.
# Module_16.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "S_NO2", "CONSTRAINTS_ISSUE", "IS_CONSTRAINT_AN_ISSUE_IN_YOUR_C". Observation records are missing from d until this file is standardized.
# Module_17.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "RANK_THE_FIRST_MOST_IMPORTANT_CO", "RANK_THE_SECOND_MOST_IMPORTANT_C", "RANK_THE_FIRST_MOST_IMPORTANT_C0", "RANK_THE_4TH_MOST_IMPORTANT_CONS", "RANK_THE_5TH_MOST_IMPORTANT_CONS". Observation records are missing from d until this file is standardized.
# Module_18.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "S_NO4", "REASON_FOR_LOAN", "DID_YOU_NEED_CREDIT", "WHY_DID_YOU_NOT_NEED_CREDIT", "DID_YOU_RECEIVE_CREDIT", "WHY_DID_YOU_NOT_RECEIVE_CREDIT_1", "WHY_DID_YOU_NOT_RECEIVE_CREDIT_2", .... Observation records are missing from d until this file is standardized.
# Module_19.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "S_NO5", "ISSUE1", "DID_YOU_NEED_EXTENSION_TRAINING_", "WHY_DID_YOU_NOT_NEED_TRAINING_AD", "DID_YOU_RECEIVE_EXTENSION_TRAINI", "WHO_IN_THE_HOUSEHOLD_RECEIVED_EX", "DO_YOU_FEEL_THAT_YOU_GOT_ADEQUAT", .... Observation records are missing from d until this file is standardized.
# Module_20.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "S_NO6", "ASSET_TYPE", "DOES_THE_HOUSEHOLD_OWN", "NO_OWNED", "AVERAGE_CURRENT_VALUE". Observation records are missing from d until this file is standardized.
# Module_21.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "S_NO7", "ANIMAL_TYPE", "DOES_THE_HOUSEHOLD_OWN1", "NO_OWNED1", "AVERAGE_VALUE", "DID_THE_HHLD_SELL", "QUANTITY_SOLD_NUMBER_R_KG_LIT", .... Observation records are missing from d until this file is standardized.
# Module_22.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "S_NO8", "ANIMAL_TYPE1", "DID_THE_HHLD_SELL1", "QUANTITY_SOLD_NUMBER_R_KG_LIT1", "AVERAGE_SELLING_PRICE_RWF1". Observation records are missing from d until this file is standardized.
# Module_23.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "COST_OF_FEED_FOR_ANIMAL_IN_THE_L", "COST_OF_LABOUR_FOR_ANIMAL_IN_THE", "COST_OF_VETERINARY_CARE_FOR_ANIM", "COST_OF_ARTIFICIAL_INSEMINATION_", "COST_OF_SALT_FOR_ANIMAL_IN_THE_L", "OTHER_COST". Observation records are missing from d until this file is standardized.
# Module_24.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "S_NO9", "INCOME_SOURCE", "WHO_EARNED_INCOME", "CASH_RWF", "IN_KIND_CASH_EQUIVALENT_RWF", "TOTAL_INCOME". Observation records are missing from d until this file is standardized.
# Module_25.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "PLEASE_ASK_THE_RESPONDENT_IF_HE_", "WILLING". Observation records are missing from d until this file is standardized.
# Module_26.csv: not read. columns: "", "ACID_LIME_DIC_ID", "PROVNC", "DISTRICT_WOREDA", "SECTOR_1", "PLEASEASK", "ACIDUSEDLIME", "FARM_SIZE_HA", "WILLINGNESS_TO_PAY_AT_FULL_COST_", "IF_ANSWER_TO_H1_IS_YES_KEEP_ON_A", "IF_ANSWER_TO_H3_IS_NO_KEEP_ON_SU", "MAXMINREASON", .... Observation records are missing from d until this file is standardized.
# Variable_description.csv: not read. columns: "", "module", "variable", "description". Observation records are missing from d until this file is standardized.
# Module_01.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_02.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_03.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_04.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_05.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_06.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_07.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_08.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_09.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_10.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_11.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_12.dta: same table as the file of this name that is read.
# Module_13.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_14.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_15.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_16.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_17.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_18.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_19.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_20.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_21.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_22.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_23.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_24.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_25.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module_26.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# variable_description.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Agricultural Lime Profitability in Rwanda (2022)

Household survey of lime use and crop harvest on farm subplots in Rwanda.
"

	uri <- "hdl:11529/10549162"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "crop",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Module_12.csv"]
	r <- read.csv(f)

	crop_of <- c(
		"Irish potato"="potato", "Maize"="maize", "Common beans"="common bean",
		"Wheat"="wheat", "Faba/horse bean"="faba bean", "Sweet potato"="sweetpotato",
		"Field peas"="pea", "Sorghum"="sorghum", "Chick-peas"="chickpea",
		"Banana"="banana", "Cabbage"="cabbage", "Barley"="barley", "Cassava"="cassava"
	)
	part_of <- c(
		potato="tubers", maize="grain", "common bean"="seed", wheat="seed",
		"faba bean"="seed", sweetpotato="roots", pea="seed", sorghum="grain",
		chickpea="seed", banana="fruit", cabbage="leaves", barley="grain", cassava="roots"
	)

	kg <- function(v) {
		z <- as.numeric(v)
		z[z %in% c(-99, 99999)] <- NA_real_
		z
	}
	area <- as.numeric(r$SUB_PLOT_AREA_HA)
	area[!is.na(area) & area <= 0] <- NA_real_
	rate <- function(v) kg(v) / area

	dap <- rate(r$QUANDAP)
	urea <- rate(r$QUANTITY_OF_FERTILIZER_UREA_USED)
	hq <- kg(r$QUANTITY_OF_HERBICIDE_USED_ON_TH)
	crop <- unname(crop_of[as.character(r$MAIN_CROP_GROWN_ON_SUB_PLOT)])
	part <- unname(part_of[crop])

	d <- data.frame(
		country = "Rwanda",
		adm1 = carobiner::fix_name(r$PROVNC, "title"),
		adm2 = carobiner::fix_name(r$DISTRICT_WOREDA, "title"),
		trial_id = as.character(r$ACID_LIME_DIC_ID),
		hhid = as.character(r$ACID_LIME_DIC_ID),
		plot_id = paste(r$PLOT_ID_START_WITH_ONE_WITH_LIME, r$SUB_PLOT_ID, crop, sep="_"),
		crop = crop,
		variety = as.character(r$VARIETY_GROWN_ON_SUB_PLOT),
		yield_part = part,
		treatment = crop,
		yield = kg(r$TOTAL_HARVESTED_PER_SUB_PLOT_MA0) / area,
		yield_isfresh = FALSE,
		lime = rate(r$QUANTITY_APPLIED_DURING_THE_LAST),
		N_fertilizer = dap * 0.18 + urea * 0.46,
		P_fertilizer = dap * 0.201,
		herbicide_used = ifelse(is.na(hq), NA, hq > 0),
		intercrops = as.character(r$CROP_INTERCROPPED_ON_SUB_PLOT),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d$variety[d$variety %in% c("", "NA")] <- NA_character_
	d$intercrops[d$intercrops %in% c("", "NA", "None", "none")] <- NA_character_

	icrop <- unname(crop_of[d$intercrops])
	iok <- !is.na(icrop) & !is.na(kg(r$TOTAL_HARVESTED_PER_SUB_PLOT_IF0))
	extra <- d[iok, ]
	extra$plot_id <- paste(r$PLOT_ID_START_WITH_ONE_WITH_LIME[iok], r$SUB_PLOT_ID[iok], extra$crop, sep="_")
	extra$crop <- icrop[iok]
	extra$yield_part <- unname(part_of[extra$crop])
	extra$treatment <- extra$crop
	extra$variety <- NA_character_
	extra$yield <- (kg(r$TOTAL_HARVESTED_PER_SUB_PLOT_IF0) / area)[iok]
	extra$intercrops <- NA_character_
	d$intercrops <- icrop
	d <- rbind(d, extra)
	d <- d[!is.na(d$crop) & !is.na(d$treatment), ]
	d$copy <- ave(seq_len(nrow(d)), d$trial_id, d$plot_id, d$crop, FUN=seq_along)
	d$plot_id[d$copy > 1] <- paste(d$plot_id[d$copy > 1], d$copy[d$copy > 1], sep="_")
	d$copy <- NULL

	## GADM point-radius centroid of the district. English province names are not used as the key.
	geo <- data.frame(
		adm2 = c("Burera", "Nyamagabe", "Nyaruguru", "Ngororero"),
		longitude = c(29.8266, 29.4699, 29.5170, 29.5695),
		latitude = c(-1.4662, -2.4114, -2.6944, -1.8778),
		geo_uncertainty = c(22162, 25330, 25561, 23866),
		geo_source = "GADM 4.1, adm2"
	)
	d <- merge(d, geo, by="adm2", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
