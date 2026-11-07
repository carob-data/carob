# R script for "carob"
# license: GPL (>=3)

## NOTES
# QDBH baseline household module, SNNPR, Ethiopia. Module 1 has 1451 interview numbers. Sampling is described as stratified random sampling by kebele.
# Woreda codes in the CSPro dictionary are 1 Chuko, 2 Dilla, and 3 Wonega. Sex is the head's status: 1 man, and 2, 3, or 4 a woman head. Education codes are the school grades in that dictionary.
# The longitude, latitude, and elevation columns in Module 1 are empty in both the dta and the sav. The interview year column is 2010 for every row, while the deposit dates the survey to November-December 2017, so the date parts are not used.

## ISSUES
# No coordinates in the released module. Kebele (admin3) is a code with no value list in the dictionary. Wonega is the dictionary spelling.
# missing variables: latitude, longitude

## NOT PROCESSED
# Modules 2 to 12 (composition, sweetpotato farming, diet, training, and assets).
# Names, phone, reference-woman age and education, and the interview clock times in Module 1.
# QDBH BASELINE SURVEY DATA DICTIONARY.txt and the Amharic-English questionnaire pdf.
# Module 1 - Household Information.sav: same table as the file of this name that is read.
# Module 10 - Training in Nutrition, OFSP Farming and Processing.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 10 - Training in Nutrition, OFSP Farming and Processing.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 11 - Nutrition and Vitamin A Knowledge.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 11 - Nutrition and Vitamin A Knowledge.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 12 - Other Household Assets and WASH.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 12 - Other Household Assets and WASH.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 2 - Household Composition and Assets.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 2 - Household Composition and Assets.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 3 - Changes in SP Farmer Past 3 Years.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 3 - Changes in SP Farmer Past 3 Years.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 4A - SP Farming and Harvesting.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 4A - SP Farming and Harvesting.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 4B - SP Sales and Seed Systems.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 4B - SP Sales and Seed Systems.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 5 - Food Security.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 5 - Food Security.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 6 - Food Consumption Frequency Past 24 Hours.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 6 - Food Consumption Frequency Past 24 Hours.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 7 - Minimal Acceptable Diet.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 7 - Minimal Acceptable Diet.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 8 - Consumption of Vitamin A Rich Foods.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 8 - Consumption of Vitamin A Rich Foods.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Module 9 - Consumption of SP and OFSP.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module 9 - Consumption of SP and OFSP.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Dataset for: Baseline Survey: The Sustained Diet Quality Improvement by Fortification with Climate-smart, Nutrition-smart Orange-fleshed Sweetpotato in Southern Nations, Nationalities and Peoples’ Region (SNNPR), Ethiopia

The QDBH project is being implemented by the International Potato Center (CIP) and partners in the Southern Nations, Nationalities and Peoples’ Region (SNNPR) in Southern Ethiopia. The project tests new OFSP varieties and nutrition training approaches using three treatments: full (Healthy Baby Toolkit, training, and OFSP planting stock), partial (training and OFSP planting stock), and control. Treatments are organized by kebeles.
"

	uri <- "doi:10.21223/P3/HBCVUN"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "stratified random sample",
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 15,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Module 1 - Household Information.dta"]
	r <- haven::read_dta(f)
	wd <- as.character(as.integer(r$admin2))
	loc <- c("1" = "Chuko", "2" = "Dilla", "3" = "Wonega")[wd]
	st <- as.character(as.integer(r$m01_02))
	sex <- c("1" = "male", "2" = "female", "3" = "female", "4" = "female")[st]
	edu <- as.character(as.integer(r$m01_05))
	edu <- c(
		"0" = "none",
		"1" = "primary 1",
		"2" = "primary 2",
		"3" = "primary 3",
		"4" = "primary 4",
		"5" = "primary 5",
		"6" = "primary 6",
		"7" = "primary 7",
		"8" = "primary 8",
		"9" = "secondary 1",
		"10" = "secondary 2",
		"11" = "secondary 3",
		"12" = "secondary 4",
		"13" = "technical",
		"14" = "university",
		"15" = "post-graduate"
	)[edu]
	trt <- trimws(as.character(r$treatment_name))

	d <- data.frame(
		trial_id = "QDBH_baseline_Ethiopia_2017",
		hhid = trimws(as.character(as.integer(r$hhid))),
		country = "Ethiopia",
		location = unname(loc),
		treatment = trt,
		sex = unname(sex),
		age = as.numeric(r$m01_03),
		education = unname(edu),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
