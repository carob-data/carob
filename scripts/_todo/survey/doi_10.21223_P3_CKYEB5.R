# R script for "carob"
# license: GPL (>=3)

## NOTES
# Kisima Farm certified-seed household survey, Kenya, seasons 2013-2014. 408 households. Country is Kenya from the title and geographic coverage.
# The household tables are inside kisima_data.tar-1.gz, which get_data downloads but does not return. Part 0 has the district and the interview timestamp. Part 1 has the village. They are joined on qnno.
# The dictionary codes Treatment 0 = Non-treated and 1 = Treated. dateintr is seconds since 1582-10-14, and the converted dates fall in 2013 and 2014.

## ISSUES
# No latitude or longitude.
# missing variables: latitude, longitude

## NOT PROCESSED
# Parts 2 to 20 of the household tar (composition, land, inputs, production, credit, seed quality) and the gender subsample.
# The village tar Part A. General_anonymized.tar and both data dictionaries.
# Data_Dictionary_Village_v2.xlsx: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Impact of Certified Seed Potato on Farm Household Welfare — Kisima Farm Study, Kenya 2013/2014

This dataset contains primary survey data collected as part of a study on the impact of certified potato seed produced and distributed by Kisima Farm on farm household welfare in Kenya. Household-level survey: 408 farm households were interviewed. Data collection covers the agricultural seasons 2013-2014.
"

	uri <- "doi:10.21223/P3/CKYEB5"
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
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Data_Dictionary_Kisima_Household-v1.xlsx"]
	tarf <- file.path(dirname(f), "kisima_data.tar-1.gz")
	ex <- file.path(tempdir(), "doi_10.21223_P3_CKYEB5")
	dir.create(ex, FALSE, TRUE)
	utils::untar(tarf, exdir = ex)
	got <- list.files(ex, recursive = TRUE, full.names = TRUE)
	f1 <- got[basename(got) == "Part 1.0 Farmer and site identification_anonymized.xlsx"]
	f0 <- got[basename(got) == "Part 0 Interview and Farmer Identification.xlsx"]
	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f0)
	r2 <- r2[match(r1$qnno, r2$qnno), ]
	trt <- as.character(as.integer(r1$Treatment))
	trt <- c("0" = "Non-treated", "1" = "Treated")[trt]
	# dateintr is an SPSS datetime: seconds since 1582-10-14
	when <- as.POSIXct(as.numeric(r2$dateintr), origin = "1582-10-14", tz = "UTC")
	village <- as.character(r1$village)
	village[is.na(village)] <- ""
	district <- as.character(r2$district)
	district[is.na(district)] <- ""

	d <- data.frame(
		trial_id = "Kisima_potato_Kenya_2014",
		hhid = trimws(as.character(as.integer(r1$qnno))),
		country = "Kenya",
		adm2 = carobiner::fix_name(district, "title"),
		location = carobiner::fix_name(village, "title"),
		crop = "potato",
		treatment = unname(trt),
		date = as.character(as.Date(when)),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
