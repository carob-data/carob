# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are relationships_csv in Seed_system_CONPAPA_for_Dataverse_20160120.xlsx.
# DateOfInterview, DatePlanting, and DateHarvest are Excel serials (origin 1899-12-30). 42325 is 2015-11-17, the November 2015 interview named in the abstract.
# The token NR, and the typo NRff, are missing. Volume is kept as recorded. The sheet does not say whether volume is kilograms.
# Country Ecuador and province Tungurahua are in the dataset title. No coordinates.

## ISSUES
# Volume is not converted to yield.
# No coordinates and no fertilizer.
# Two records have a harvest date within 45 days of planting. Those dates are kept.
# Some rows are identical on the columns kept here.
# write_files: missing variables: carob_contributor; geo_from_source, latitude, longitude.
# write_files: unknown variables: seed_or_product_, volume_, price_min_, price_max_.
# write_files: datespan: 2 records with harvest_date within 45 days of planting_date.
# write_files: duplicates: duplicate records detected.

## NOT PROCESSED
# stakeholder_csv, IPM_Advice_Stakeholders_csv, and IPM_Advice_relationships_csv: network and advice tables, not plot yields.
# PriceMin and PriceMax have no currency in the sheet.

## NEW VARIABLES
# seed_or_product_: Seed or Product, as recorded
# volume_: amount moved, unit not stated
# price_min_: minimum price as recorded
# price_max_: maximum price as recorded

carob_script <- function(path) {

"
Dataset for: A potato seed system from CONPAPA, a consortium of small potato producers in Tungurahua Ecuador

This contains potato seed system data collected for a network analysis. It is from CONPAPA, a consortium of small potato producers in Tungurahua Ecuador. It documents the movement of potato seed, and potato product for several potato varieties. Information is from 48 farmers, and documents stakeholders involved in the seed system, and in the buying and selling of potatoes for food. Data are anonymized to protect the identities of the participants. In addition, the data documents the farmers' main sources of information for integrated pest management. Data was collected in November 2015, but documents information from several planting seasons.
"

	uri <- "doi:10.21223/P3/XKHUTL"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=6,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "volume_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Seed_system_CONPAPA_for_Dataverse_20160120.xlsx"]
	r <- carobiner::read.excel(f, sheet="relationships_csv")
	num <- function(x) {
		y <- trimws(as.character(x))
		y[y %in% c("", "NR", "NRff", "NA")] <- NA_character_
		as.numeric(y)
	}
	excel_date <- function(x) {
		if (inherits(x, "Date") || inherits(x, "POSIXt")) {
			return(as.character(as.Date(x)))
		}
		y <- trimws(as.character(x))
		y[y %in% c("", "NR", "NRff", "NA")] <- NA_character_
		ok <- !is.na(y) & grepl("^[0-9]+$", y)
		out <- rep(NA_character_, length(y))
		out[ok] <- as.character(as.Date(as.numeric(y[ok]), origin="1899-12-30"))
		out
	}
	sex <- tolower(trimws(as.character(r$GenderFrom)))
	sex[!nzchar(sex)] <- NA_character_

	d <- data.frame(
		hhid = trimws(as.character(r$InformationSource)),
		date = excel_date(r$DateOfInterview),
		planting_date = excel_date(r$DatePlanting),
		harvest_date = excel_date(r$DateHarvest),
		seed_or_product_ = trimws(as.character(r$SeedOrProduct)),
		variety = trimws(as.character(r$Variety)),
		volume_ = num(r$Volume),
		price_min_ = num(r$PriceMin),
		price_max_ = num(r$PriceMax),
		sex = sex,
		country = "Ecuador",
		adm1 = "Tungurahua",
		crop = "potato",
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA,
		trial_id = "CONPAPA_2015",
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
