# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household maize survey in Nepal. Code sheet 1 names the three palikas: 1 Gandaki in Gorkha, 2 Panchpokhari in Sindhupalchok, 3 Ichhyakamana in Chitwan.
# Country is Nepal in the title and in geographicCoverage.
# PlanArea_Maize2077 is hectares. MaizeProd_ThisYr is grain from that land. Yield is computed only where the unit is Kg, as kilograms divided by hectares.
# The GPS columns are empty. geo_from_source is FALSE.
# 2077 is a Bikram Sambat year in the questionnaire. It is not converted to a Gregorian date.
# Rows with no maize, or with production in Pathi, Muri, Bhari, Doko, Bora, or Quintal, are left out. The file does not define those units in kilograms.

## ISSUES
# missing variables: latitude, longitude
# unknown variable: season_bs_ (season?)

## NEW VARIABLES
# season_bs_: Bikram Sambat year of the maize season in the questionnaire (2077).

## NOT PROCESSED
# Fertilizer, manure, and pesticide amounts. Urea and DAP are kilograms for the whole field, and the file does not state nutrient contents.
# Migration, gender, food, and remittance modules.
# Production reported in local units.
# Earlier years 2076, 2075, and 2072.


carob_script <- function(path) {

"
Maize, Migration and Gender Dynamics in Nepal

Household maize area and grain production in three palikas of Nepal.
"

	uri <- "hdl:11529/10549020"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[grepl("CRP_Maize_NEPAL", basename(ff))]
	r <- carobiner::read.excel(f, sheet="CRPmaize_Data_Nepal", col_types="text")
	r <- r[!is.na(r$MaizeProd_ThisYr_unit) & r$MaizeProd_ThisYr_unit == "Kg", ]

	blank <- function(x) {
		x <- trimws(x)
		x[is.na(x) | !nzchar(x) | x %in% c("NA", "-")] <- NA_character_
		x
	}
	num <- function(x) as.numeric(blank(x))
	area <- num(r$PlanArea_Maize2077)
	prod <- num(r$MaizeProd_ThisYr)
	ok <- !is.na(area) & area > 0 & !is.na(prod)
	r <- r[ok, ]
	area <- area[ok]
	prod <- prod[ok]

	pal <- c("1"="Gandaki", "2"="Panchpokhari", "3"="Ichhyakamana")
	dst <- c("1"="Gorkha", "2"="Sindhupalchok", "3"="Chitwan")
	code <- trimws(r$Palika)
	variety <- blank(r$NameVar_Maize1)
	variety[variety %in% c("No maize cultivation", "No maize cultivation / No idea", "No idea")] <- NA_character_

	d <- data.frame(
		country = "Nepal",
		adm2 = dst[code],
		location = pal[code],
		hhid = r$HH_ID,
		trial_id = r$HH_ID,
		crop = "maize",
		variety = variety,
		yield_part = "grain",
		yield = prod / area,
		field_size = area,
		season_bs_ = "2077",
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
