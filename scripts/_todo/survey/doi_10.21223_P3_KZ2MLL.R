# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline nutrition survey in Tigray, Ethiopia, 2011. Ethiopia and Tigray are in the title and in geographic coverage. 150 households.
# Zone labels in the file are SOUTHEREN, EASTEREN, and CENTRAL. Wereda and sex labels are in the file. Member 01 is the household head on every row. Sex is that person.
# Household size is the count of family-member sex fields that are filled.
# The .dta and the .sav are the same 150 by 568 table. The .dta is used.

## ISSUES
# No latitude or longitude.
# Interview year is 3 for every row and the month is 10 or 11, so a calendar date was not built. The description says 28 April to 12 May 2011.
# Zone names are the English labels in the file. GADM uses other spellings for the Tigray zones.
# Ten village values are the codes 01, 02, and 03. Those rows use the kebele name as the location.
# missing variables: latitude, longitude
# unknown variables: survey_group_

## NEW VARIABLES
# survey_group_: participant or control household, from FARMTYPE

## NOT PROCESSED
# Names of household heads.
# Kebele or tabia (A03B). The labels are in the file and were not assigned to an admin level.
# Crops, yields, assets, income, and food-consumption columns in this table.
# 09142011  Data for Baseline Nutrition Project Tigray.sav (same table), the data dictionary, and the questionnaire.
# Data Dictionary for Tigray Nutrition Project.xlsx: documentation or a codebook, not an observation table.
# Questionnaire for Baseline survey of Tigray Nutrition project.xlsx: design, layout, or protocol sheet, not plot or survey records.


carob_script <- function(path) {

"
Dataset for: Alleviation of Food Insecurity and Malnutrition in Tigray, Ethiopia, through Promotion of Potato and Sweetpotato: Baseline Nutrition Project

The baseline survey for the project Alleviation of Food Insecurity and Malnutrition in Tigray, Ethiopia, through Promotion of Potato and Sweetpotato, Nutrition Project was conducted from 28th April to 12th 2011. The data was collected from 150 households (HH) (100 participant and 50 non-participant households) in ten rural Kebeles of five districts. HH-level information was collected with regard to socio-economic, agronomic and nutritional indicators focusing on HH asset holding, type of crops grown, yields per hectare, sources of income, contribution of different crops to HH income, food supply and utilization. Descriptive statistics (mean, medium, percentage, standard deviation, T-test and chi-square test) and graphs were used to analyze the data. 
The key findings of the baseline survey are. In terms of socio-economic indicators participant and non-participant HH were similar, however, the analysis showed that non-participants HH faced more frequent food shortages during the last 12 months; in particular during the months of July, August and September. In addition, a larger proportion of the non-participant HH belonged to the low to intermediate wealth group. This suggests that non-participants are a more vulnerable group in terms of food security and this will need to be considered in the analysis of the final impact survey data. Potato was grown by around 23% of HH whereas sweet-potato is basically a new crop in the area, grown by less than 7% of HH. Area coverage of potato as compared to other crops was relatively low.  Potato and sweetpotato yields obtained by participants and non-participants were similar. On average a HH produced 17.4 quintal of potato per year, of which approximately 93% was sold. The remaining produce was used for seed and home consumption purposes which indicate that potato is mainly considered as a cash crop.  With regard to the frequency of food consumption, no significant difference between participants and  non-participants  was  found  with  regard  to  cereals,  tubers,  fruit, meat/fish and milk. However, the average milk consumption was significantly higher among children of non-participant HH. The food consumption frequency data was used to calculate a Food Consumption Score (FCS), grouping children aged 6 to 59 months and lactating mother into three categories of food consumption (poor, borderline and acceptable). While 67% of the children in non-participant HH had an acceptable food consumption, this was the case only for 18% of the children of participant HH. This indicates that children from non- participant HH were better fed than children of participant HH. In contrast, breast feeding mothers in the participant group scored better than mothers in the non- participant group. A Dietary Diversity Index (DDI) was calculated to assess diet quality of children aged 6 to 23 months of age and the entire HH. No significant differences between participant and non-participant HH were found for both children and entire HH. However, the findings suggest a significant difference between both groups about the percentage of HH that achieved the minimum DDI (i.e. 81% participant and 72% non- participant HH).
"

	uri <- "doi:10.21223/P3/KZ2MLL"
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
		carob_completion = 10,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "09142011  Data for Baseline Nutrition Project Tigray-1.dta"]
	r <- haven::read_dta(f)
	zone <- as.character(haven::as_factor(r$A01B))
	zone <- c("SOUTHEREN" = "Southern", "EASTEREN" = "Eastern", "CENTRAL" = "Central")[zone]
	loc <- carobiner::fix_name(as.character(haven::zap_labels(r$A04A)), "title")
	keb <- carobiner::fix_name(as.character(haven::as_factor(r$A03B)), "title")
	i <- loc %in% c("01", "02", "03")
	loc[i] <- keb[i]
	sexcols <- grep("^FM_SEX_", names(r), value = TRUE)
	nmem <- rep(0, nrow(r))
	for (nm in sexcols) {
		nmem <- nmem + as.integer(!is.na(haven::zap_labels(r[[nm]])))
	}

	d <- data.frame(
		trial_id = "Tigray_nutrition_2011",
		hhid = as.character(haven::zap_labels(r$HHNO)),
		country = "Ethiopia",
		adm1 = "Tigray",
		adm2 = unname(zone),
		adm3 = carobiner::fix_name(as.character(haven::as_factor(r$A02B)), "title"),
		location = loc,
		hh_size = as.numeric(nmem),
		sex = tolower(as.character(haven::as_factor(r$FM_SEX_1))),
		survey_group_ = tolower(as.character(haven::as_factor(r$FARMTYPE))),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
