# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household rows from VISTA_2018_survey_dataset_cleaned.dta.
# Country Mozambique and provinces Nampula and Zambezia are in the dataset title. ad1 to ad4 are stored as recorded.
# sizefarmupl is kept as recorded. The file does not give the unit in the column name.

## ISSUES
# No coordinates in the columns used here.
# No crop yield in this household table. Some household ids are missing, and some mapped rows are identical.
# write_files: missing variables: carob_contributor; geo_from_source, latitude, longitude.
# write_files: unknown variables: farm_size_upland_.
# write_files: duplicates: duplicate records detected. NA detected: hhid.

## NOT PROCESSED
# The other columns of the 753-column endline file (modules on women, crops, and assets beyond household size and the upland farm-size field).

## NEW VARIABLES
# farm_size_upland_: sizefarmupl as recorded

carob_script <- function(path) {

"
Dataset for: The Viable Sweetpotato Technologies in Africa (VISTA)-Mozambique: Endline survey for Nampula and Zambezia Provinces

The endline survey for Viable Sweetpotato Technologies in Africa (VISTA) project in Mozambique was conducted in November-December, 2018 in 15 districts: 11 in  Nampula province (11 districts in Nampula Province (Nampula City, Rapale, Murrupula, Malema, Mecuburi, Meconta, Monapo, Mogovolas, Moma, Larde and Angoche) and 4 in Zambezia Province (Alto Molocue, Gurue, Mocuba and Gile). The survey covered 1, 538 households from these 15 districts. A multi-stage sampling procedure was used in selecting households for the study. The first stage involved stratifying the 15 districts into two strata: the pilot districts that were first to be exposed to the intervention from 2014 and the expansion districts that were exposed from 2016. The second stage involved the random selection of the intervention and control villages from the list that was identified by the project staff and local agriculture and health extension officers at the beginning of the project. Once the villages were selected, the households from these villages who were among the 2,955 households interviewed during a baseline survey in 2015 and follow-up survey in 2017 were listed to create as the sampling frame from which the 1,538 households for the 2018/19 survey were randomly selected. This way, the households interviewed in previous surveys were revisited in the 2018/19 survey providing the opportunity to understand farmer experiences with OFSP and adoption, retention and dis-adoption patterns.  The number of villages per district and households per village was proportional to size. A structured household survey was used for data collection. The questionnaire had several modules including household socio-demographic characteristics; crop production and sales; knowledge, attitudes and practices on sweetpotato; preferred attributes for sweetpotato varieties; growing of OFSP in previous and current seasons; nutrition and vitamin A knowledge; intake of vitamin A rich foods including OFSP, dietary diversity and food security status.
"

	uri <- "doi:10.21223/CN3ZNG"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "farm_size_upland_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "VISTA_2018_survey_dataset_cleaned.dta"]
	r <- carobiner::read.dta(f)
	sex <- trimws(as.character(r$sex))
	sex <- tolower(sex)

	d <- data.frame(
		hhid = trimws(as.character(r$hhid)),
		country = "Mozambique",
		adm1 = trimws(as.character(r$ad1)),
		adm2 = trimws(as.character(r$ad2)),
		adm3 = trimws(as.character(r$ad3)),
		location = trimws(as.character(r$ad4)),
		sex = sex,
		age = as.numeric(r$age),
		hh_size = as.numeric(r$hhsize),
		farm_size_upland_ = as.numeric(r$sizefarmupl),
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA,
		trial_id = "VISTA_2018"
	)

	carobiner::write_files(path, meta, d)
}
