# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mama SASHA cross-sectional endline, March to May 2014. Household id, district, village, elevation, and interview date are in MAMA_SASHA_end_pg1_hh_id.dta.
# The description says Bungoma and Busia, Kenya. The title does not name a country, geographic coverage is empty, and there is no country column, so country is not set.
# Elevation codes -8 and -9 are the missing codes used in this file (the same codes are used for unused callback dates). They are set to missing.
# a06b is intervention or control.

## ISSUES
# Country is not set. The title does not name a country, geographic coverage is empty, and there is no country column.
# No latitude or longitude. The file asks whether GPS was taken at the census and stores elevation, not coordinates.
# District names were not checked against GADM.
# missing variables: country, latitude, longitude
# unknown variables: survey_group_

## NEW VARIABLES
# survey_group_: intervention or control household, from a06b

## NOT PROCESSED
# Phone numbers and the other interview-time fields.
# Division, location, and sublocation names.
# The other modules: adult and child demography, diet, food security, nutrition, attitudes, crop production, sweetpotato sales and practice, seed system, project exposure, assets, housing, and anthropometry.
# haconver.dta, sp_conv.dta, the questionnaire workbook, and the Stata dictionary.
# MAMA SASHA _end_questionnaire.xlsx: design, layout, or protocol sheet, not plot or survey records.
# MAMA SASHA end_data_dictionary.do: documentation or a codebook, not an observation table.
# MAMA_SASHA-end_pg17_seedsystem_02.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg10_parity_woman.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg11_consumption_VitA.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg12_crop_production.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg13_sp_sales_01.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg13_sp_sales_02.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg15_project_exposure.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg17_seedsystem_01.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg18_SP_practise_01.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg2_adult_demog.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg20_sp_practice_02.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg21_assets.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg22_housing.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg23_anthropometry.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg3A_child_demog.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg3B_refchild.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg4_dietdiversity.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg5_food_security.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg6_nutrition.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg8_attitudes.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA_SASHA_end_pg9_health_nutrition.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Dataset for: MAMA SASHA Cross-sectional Endline Household Survey

A step toward improving the supply of nutrient-rich foods in sub-Saharan Africa (SSA) was the Sweetpotato Action for Security and Health in Africa (SASHA), a five-year action research project led by the International Potato Centre (known by its Spanish acronym, CIP) designed to improve the food security and livelihoods of poor families by addressing the bottlenecks preventing the full exploitation of sweetpotato’s potential.  The agriculture-health linkages proof-of-concept project in Western Province of Kenya, known as Mama SASHA, was one of the SASHA’s proof-of-concept projects. It was implemented in partnership with PATH, a leading non-governmental international organization in health, the Kenyan Agricultural Research Institute (KARI), local agriculture and health government stakeholders in Western Province, and two local implementing NGO agriculture partners, i.e. the Community Research in Environment and Development Initiatives (CREADIS) and the Appropriate Rural Development Agriculture Programme (ARDAP). 

The Mama SASHA project was set in Bungoma and Busia counties in Kenya’s Western Province. The overall project goal was to improve the health status of pregnant women and the nutritional status of children up to two years through an integrated OFSP and health service-delivery strategy. The specific objectives included:

1. To understand the costs and benefits of linking an integrated OFSP-focused agricultural-nutritional intervention to a health service-delivery system serving pregnant women, and to delineate the intensity of nutrition intervention required to achieve impact on newborns as well as their mothers.

2. To strengthen aspects of existing information, education, and communication materials and methods for supporting sustainable OFSP production and consumption at both the health facility and community levels; i.e. to improve the knowledge and practices of health workers, agricultural extension agents, and community members on OFSP and vitamin A rich foods. 

3. To improve the evidence base on impacts (on sweetpotato production, nutrition, women and child health status, and use of health services) and on sustainability of a delivery system for high-yielding OFSP through community- and facility-based health services, in conjunction with agricultural partners.

A quasi-experimental evaluation design measured the effectiveness of the Mama SASHA project between households and individuals in the intervention and control communities. Project activities were implemented in four intervention and four control health facilities / dispensaries in Bungoma and Busia counties in Western Kenya.   Each facility served approximately 10-15 villages. Communities were approximately 30-50 km apart. The four intervention sites receive the full range of nutrition education and communication, outreach, and health services, along with the vouchers. Mama SASHA was integrated into USAID/Kenya AIDS, Population and Health Integrated Assistance Program (APHIA II; then APHIA Plus), which was responsible for improving health services for pregnant women and mother-child pairs across the province. Thus, the four control group facilities offered the standard APHIAplus training and sensitization on Infant and Young Child Nutrition services, but without the pregnant women’s groups, vouchers, or support for the production of OFSP. The two Kenyan agricultural NGOs, ARDAP and CREADIS, each supported the communities affiliated with two health facilities.

Two evaluation methods measured the program benefits over the course of the project. The first was through a two-round cross-sectional household survey of pregnant women and mother-child (under 2 years of age) pairs conducted in four intervention and four control areas. The second was a complementary nested longitudinal cohort study on an initial sub-set of 505 pregnant women and their subsequent offspring in intervention and comparison communities. This dataset focuses on the cross-sectional endline survey’s mother-child pairs at endline.  The endline household survey was designed and implemented from March to May 2014 by CIP, PATH, Emory University, the University of Washington, and the University of Toronto, following the 31 months of implementation (from April 2011 through December 2013). Additional evaluation components summarized in this report include operations research (OR) to assess the acceptability and feasibility of the Mama SASHA project during the pilot phase in 2010 and again during implementation of the project and a cost study was conducted alongside project monitoring and evaluation in order to assess the affordability and sustainability of an integrated agriculture and health project.  The databases for these additional components will be provided in a separate metadata file.

Methods: Study Site: The cross-sectional endline survey was conducted in from March 2014 through May 2014 in Bungoma and Busia counties of western Kenya. The study sites consist of sub-counties in the Busia and Bungoma counties that were not already participating in the CIP DONATA project, a related agricultural project that promoted OFSP agriculture production in Western Kenya.  

Target Populations: The study targeted three populations, i.e. pregnant women, children aged 6-23 months and their mothers/caregivers. 

Inclusion criteria: Eligible participants included females 17-45 years of age, residing in the study villages and who are either the biological mother or primary caretaker of a child 6-23 months or who have a confirmed pregnancy (i.e. by health worker).

Exclusion criteria: women who are younger than 17 or older than 45 years of age and who are not in their first or second trimester of pregnancy or the primary caretaker of a child 6-23 months of age are not eligible. 

Sample size:  The sample size calculation was made to allow for comparison of proportions of VAD among children 6-23 months within intervention and control (i.e. endline versus baseline) and comparisons across intervention and control at the endline survey.  The sample size of 1,090 mother-child pairs per provided 90% power to detect a statistically significant 31% decrease in VAD prevalence in the intervention group children compared to the control group given a baseline VAD prevalence of 18.2%.  For pregnant women interviews, we estimated that we would require 85 pregnant women per intervention and control to observe significant increases in consumption of vitamin A rich food frequency score.


The survey included interview modules for household characteristics, household food security and dietary diversity, nutrition and health knowledge, attitudes and practices, agriculture and project exposure and uptake.  In addition, weight and height were collected for both children and their mothers.  Dried blood spot samples were obtained from the children to determine levels of retinol binding protein (an indicator of vitamin A status) and degree of inflammation (C-Reactive protein).
"

	uri <- "doi:10.21223/P3/M3ZMKR"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "quasi-experimental",
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 8,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "MAMA_SASHA_end_pg1_hh_id.dta"]
	r <- haven::read_dta(f)
	elev <- as.numeric(haven::zap_labels(r$a18))
	elev[elev == -8 | elev == -9] <- NA
	day <- as.integer(haven::zap_labels(r$a19a))
	mon <- as.integer(haven::zap_labels(r$a19b))
	yr <- as.integer(haven::zap_labels(r$a19c))
	when <- as.Date(sprintf("%04d-%02d-%02d", yr, mon, day))

	d <- data.frame(
		trial_id = "MamaSASHA_endline_2014",
		hhid = as.character(haven::zap_labels(r$hh_id)),
		adm2 = carobiner::fix_name(as.character(haven::zap_labels(r$a01a)), "title"),
		location = carobiner::fix_name(as.character(haven::zap_labels(r$a05a)), "title"),
		elevation = elev,
		survey_group_ = as.character(haven::as_factor(r$a06b)),
		date = as.character(when),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
