# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey after typhoon Ompong, Philippines. Country is Philippines.
# Section 0 is location and consent. The Codes sheet lists province, municipality, and barangay. 0.3, 0.4, and 0.5 match those code lists (1 = Apayo, 2 = Kalinga; municipalities Conner through Pinukpuk). The Codes sheet spells the first province Apayo.

## ISSUES
# No latitude or longitude.
# The crop is roots and tubers as a group. No single crop is named for every household.
# Sections B to G are not in this table.
# missing variables: carob_contributor
# missing variables: latitude, longitude

## NOT PROCESSED
# Section 0 columns 0.2, 0.7_1, 0.7_2, and 1.1. The Codes sheet does not define them.
# Section B through Section G.
# DataDictionary_Typhoon Philippines.xlsx and the questionnaire.


carob_script <- function(path) {

"
Dataset for: Agricultural Resilience through roots and tubers in typhoon prone regions in Philippines

Root and tuber crops (RTC) play an important role in diverse Asian agri-food systems and contribute to the household resilience which is the capacity to recover from shocks and stressors, including the ones related to climate change and increasingly frequent extreme whether events like typhoons. The dataset was collected with the objective to perform a quantitative assessment of the contribution of RTCs for enhanced resilience and disaster recovery after typhoons. Data collection was implemented after the super typhoon Ompong hit the Philippines in September 2018. The information collected was divided in seven sections: (0) Location information and consent, (B) Household roster: socio-demograpghic characteristics, (C) Household characteristics and economic activity, (D) Farm characteristics, (E) Typhoon characteristics, (F) Typhoon response: inmediate resilience strategies, and (G) Typhoon response: General strategies.
"

	uri <- "doi:10.21223/FP47JO"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "DataSet_Typhoon Philippines.xlsx"]
	s0 <- carobiner::read.excel(f, sheet = "Section 0", na=c("", "NA"))
	codes <- carobiner::read.excel(f, sheet = "Codes", na=c("", "NA"))
	lookup <- function(code, name) {
		code <- as.integer(trimws(as.character(code)))
		name <- trimws(as.character(name))
		keep <- !is.na(code) & !is.na(name) & nzchar(name)
		stats::setNames(name[keep], code[keep])
	}
	prov <- lookup(codes[[2]], codes[[1]])
	muni <- lookup(codes[[5]], codes[[4]])
	brgy <- lookup(codes[[8]], codes[[7]])

	d <- data.frame(
		trial_id = "RTC_typhoon_Philippines",
		hhid = trimws(as.character(s0$HH_uniqueID)),
		country = "Philippines",
		adm1 = unname(prov[as.character(as.integer(s0[["0.3"]]))]),
		adm2 = unname(muni[as.character(as.integer(s0[["0.4"]]))]),
		location = unname(brgy[as.character(as.integer(s0[["0.5"]]))]),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
