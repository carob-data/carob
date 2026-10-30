# R script for "carob"
# license: GPL (>=3)

## NOTES
# Potato farmers in the Bangladesh COVID value-chain survey, July to November 2021. Country is the Country column. The crop is potato.
# C5 and C6 are kilograms of potato produced in 2019 and 2021, summed across seasons. They are not kilograms per hectare. Farm land is recorded with a separate unit, so it is not converted.

## ISSUES
# Baseline ID Number is empty. Household id is Data ID.
# No latitude or longitude.
# missing variables: carob_contributor
# missing variables: latitude, longitude
# unknown variables: potato_produced_kg_2019_, potato_produced_kg_2021_

## NOT PROCESSED
# Land area and its unit, prices in taka, labour, credit, and the rest of the farmer questionnaire.
# Cold storage, processor, seed, and trader workbooks. They are other actors, not this farmer table.
# Codebook, data dictionary, and questionnaire.


carob_script <- function(path) {

"
Dataset for: COVID-19 and agricultural value chains: Impacts and adaptation of value chain actors in Bangladesh, 2021

Potatoes play an important role in rural livelihoods in Bangladesh. The study is a quantitative assessment to better understand the effects of the COVID-19 pandemic on potato production, post-harvest and marketing, using data from 5 potato value chain actors. 700 potato farmers, 120 potato traders, 52 cold storage owners, 7 seed producers, and 2 potato processors were interviewed between July and November 2021. The information collected was divided into eight sections: A) Introduction, socioeconomic information of respondent, and business basic information (13 questions), (B) Reason for stopping business operations (15 questions), (C) Crop mix and scale of operations (Farmers) / Operations (Other Value Chain actors) (15 questions), (D) Labour use (13 questions for farmers, 8 questions for the rest of the actors), (E) Purchase of inputs and credit for farmers (20 questions) / Procurement for the rest of the actors (24 questions), (F) Sales and marketing (14 questions for farmers and Cold storage owners / 9 questions for the rest of the value chain actors), (G) Overall welfare (15 questions), (H) Adaptation strategies (2 questions).
"

	uri <- "doi:10.21223/OBJFRB"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.35
	)

	f <- ff[grepl("^01_Farmer", basename(ff))]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	nm <- names(r)
	kg19 <- nm[grepl("^C5\\. How much potato \\(Kg\\)", nm)][1]
	kg21 <- nm[grepl("^C6\\. How much potato \\(Kg\\)", nm)][1]
	sex <- trimws(as.character(r[["A10. Respondent's gender"]]))
	sex <- c(Male = "male", Female = "female")[sex]

	d <- data.frame(
		trial_id = "BD_potato_COVID_2021",
		hhid = trimws(as.character(r[["Data ID"]])),
		country = trimws(as.character(r$Country)),
		adm2 = trimws(as.character(r[["A6. What district do you live in?"]])),
		adm3 = trimws(as.character(r[["A7. What upazilla do you live in?"]])),
		location = trimws(as.character(r[["A8. What village, city or town do you live in?"]])),
		crop = "potato",
		sex = sex,
		age = as.numeric(r[["A9. Age of respondent (Years)"]]),
		potato_produced_kg_2019_ = as.numeric(r[[kg19]]),
		potato_produced_kg_2021_ = as.numeric(r[[kg21]]),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
