# R script for "carob"
# license: GPL (>=3)

## NOTES
# SUSTAIN baseline household survey in Rwanda, 2014-2015. Rwanda is the geographic coverage. Province, district, and sector labels are in the household file. The sector is the finest place that still has a name.
# Sex is the household head. The value labels are Male and Female.
# The description states a randomized controlled trial. The arm labels are in tr_group. One label spells nutrition as nutriion. That spelling is kept.
# Cell and village names, and GPS, were removed in the published file. The readme says so.

## ISSUES
# No latitude or longitude.
# Province labels are Southern, Northern, and Eastern. They were not checked against GADM.
# missing variables: latitude, longitude
# unknown variables: survey_group_

## NEW VARIABLES
# survey_group_: randomized treatment arm, from tr_group

## NOT PROCESSED
# The transfer, consumption, and other household columns in SUSTAIN_Baseline_HHSurvey2015_HH_level_v11.dta.
# SUSTAIN_Baseline_HHSurvey2015_CROP_level_v11.dta, SUSTAIN_Baseline_HHSurvey2015_PLOT_level_v11.dta, SUSTAIN_Baseline_HHSurvey2015_INDIVIDUAL_Level_v11.dta, and SUSTAIN_Baseline_LISTING_Data2015_v11.dta.
# Anonymized cell and village codes. The pdf metadata and the Word questionnaires were not in the files returned by get_data.
# SUSTAIN_README.txt: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Replication data for: Impact Evaluation of the Scaling Up Sweetpotato Through Agriculture and Nutrition (SUSTAIN) Project in Rwanda: Baseline Household Survey and Listing Data, 2014-2015

Vitamin A deficiency (VAD) is a widespread health problem among young children and pregnant women in many countries in Africa. One effective and low cost solution to fight VAD is to introduce a biofortified staple food crop that is rich in vitamin A, such as the beta-carotene-rich orange fleshed Sweetpotato (OFSP). Scaling Up Sweetpotato Through Agriculture and Nutrition (SUSTAIN) is a project led by the International Potato Center (CIP) with support from the U.K. Department for International Development to deliver the benefits of OFSP to millions of farming households and thousands of urban and rural consumers in Kenya, Malawi, Mozambique, and Rwanda by taking an integrated agriculture-nutrition-marketing approach. As an independent evaluation partner, Michigan State University had designed a randomized-controlled trial (RCT) field experiment to test the effectiveness of different combinations of the integrated agriculture-nutrition-market approach of promoting the adoption and consumption of OFSP at scale. The research design consisted of randomly assigning project villages to one of the following six treatment arms: 1) Base model: All the components of the integrated Agriculture, Nutrition and Marketing approach; 2) Base model less nutrition counseling; 2) Base model less marketing; 4) Base model less nutrition counseling and marketing; 5) Base model, but only one season; and 6) Base model with the second time vine distribution at a higher price. The evaluation was designed to address the following research questions: 1) Which combinations of market promotion and nutrition information dissemination are most cost effective for promoting production and consumption of OFSP by a large number of beneficiaries (scale) to meet their nutritional needs (intensity) over time (sustainability)? 2) Which factors are important in determining OFSP adoption by farmers, and their consumption among children and women? Do the different promotion strategies influence them differently? 3) How do the effects of various project components involving training and knowledge dissemination (e.g., nutritional information, market information) differ across different recipient and household characteristics (e.g., gender, age, education of recipient of training and of household head)? This dataset is an output of household surveys of potential project beneficiaries conducted in 8 districts in Rwanda in 2014-2015 by Michigan State University to serve as a baseline for the impact evaluation.
"

	uri <- "doi:10.21223/P3/MKWO5O"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "randomized controlled trial",
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 8,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "SUSTAIN_Baseline_HHSurvey2015_HH_level_v11.dta"]
	r <- haven::read_dta(f)
	sec <- carobiner::fix_name(as.character(haven::as_factor(r$scode)), "title")

	d <- data.frame(
		trial_id = "SUSTAIN_Rwanda_2015",
		hhid = trimws(as.character(r$hhid)),
		country = "Rwanda",
		adm1 = carobiner::fix_name(as.character(haven::as_factor(r$pcode)), "title"),
		adm2 = carobiner::fix_name(as.character(haven::as_factor(r$dcode)), "title"),
		adm3 = sec,
		location = sec,
		sex = tolower(as.character(haven::as_factor(r$a18))),
		survey_group_ = trimws(as.character(haven::as_factor(r$tr_group))),
		date = as.character(r$surveydate),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
