# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline of potato-seed agripreneurs in Nakuru County, Kenya, October 2025. The title and geographic coverage place the survey in Kenya, Nakuru County.
# The sub-county code is not defined in the dictionary. Each record fills exactly one ward block, and the block name is the sub-county: Njoroward, Molowards, KuresoiSouth, or KuresoiNorth_Ward.
# Village is text. The crop is potato.

## ISSUES
# Agripreneur id 54 is on two rows.
# Gender is coded 1 and 2. The dictionary does not define the codes.
# The dictionary lists gps_latitude and gps_longitude. Those columns are not in the anonymized workbook.
# No latitude or longitude.
# missing variables: latitude, longitude

## NOT PROCESSED
# Education, marital status, and the coded seed-source, storage, labour, and variety blocks. Units for quantity and price are codes.
# Plot sizes. The unit code is not defined in the dictionary.
# Phone numbers and the Word questionnaire.
# 02_Data_dictionary_seed.xlsx: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Agripreneurs in Potato Seed Multiplication: Baseline Evidence from Youth and Non-Youth in Nakuru County, Kenya

This baseline survey assesses the conditions, capacities, and constraints influencing the production, distribution, and adoption of potato innovations in Kenya. The study focuses on identifying opportunities to support youth- and non-youth-led agribusinesses—both female and male—as certified seed entrepreneurs within a decentralized and inclusive seed multiplication model.
"

	uri <- "doi:10.21223/P3/MCRQPJ"
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
		carob_completion = 15,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Data_POTATO_SEED_AGRIPRENEUR_BUSINESS_SURVEY_WIDE_Anonymized.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	sub <- rep(NA_character_, nrow(r))
	sub[!is.na(r$Njoroward)] <- "Njoro"
	sub[!is.na(r$Molowards)] <- "Molo"
	sub[!is.na(r$KuresoiSouth)] <- "Kuresoi South"
	sub[!is.na(r$KuresoiNorth_Ward)] <- "Kuresoi North"
	when <- as.Date(r$Date)

	d <- data.frame(
		trial_id = "Nakuru_potato_seed_2025",
		hhid = as.character(as.integer(r$Agripreneiur_ID)),
		country = "Kenya",
		adm1 = "Nakuru",
		adm2 = sub,
		location = carobiner::fix_name(trimws(as.character(r$village)), "title"),
		crop = "potato",
		hh_size = as.numeric(r$Household_size),
		age = as.numeric(r$Age),
		date = as.character(when),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
