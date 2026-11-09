# R script for "carob"
# license: GPL (>=3)

## NOTES
# Market survey of sweetpotato in Malawi, June 2019. Districts are the ones named on the form. The village column is empty, so the market name is the location.
# Coordinates and altitude are the GPS columns. Kampini in Dedza is stored as 14.3188 N. Dedza is in Malawi, and that latitude is the same number with the wrong sign, so it is stored as -14.3188.
# Interview dates are day-month-year.

## ISSUES
# Mzimba South and Lilongwe West are not the GADM district names.

## NOT PROCESSED
# Root counts, sale units, weights, and prices. One product block is labeled KENYA on a Malawi form that prices in Malawi kwacha, so the product names are not used.
# Substitute crops, seller counts, cassava form, and the closing questions about iron-rich beans, vitamin A maize, and purple-fleshed sweetpotato.
# Enumerator remarks and interview duration.
# DataDictionary_Market OFSP Malawi.xlsx: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Adoption and Effects of Orange-Fleshed Sweetpotato in Malawi (market)
"

	uri <- "doi:10.21223/7HIQPG"
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
		carob_completion = 35,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Market Survey.xlsx"]
	r <- carobiner::read.excel(f, sheet="market")
	lat <- as.numeric(r$"GPS Latitude")
	i <- which(r$"9. Name of Market" == "Kampini" & r$"2. District" == "Dedza" & lat > 0)
	lat[i] <- -lat[i]
	when <- as.Date(trimws(as.character(r$"5a. Interview date (DD-MM-YYYY)")), "%d-%m-%Y")

	d <- data.frame(
		trial_id = "OFSP_Malawi_market_2019",
		hhid = trimws(as.character(r$"Questionnaire ID")),
		country = "Malawi",
		adm2 = trimws(as.character(r$"2. District")),
		location = trimws(as.character(r$"9. Name of Market")),
		crop = "sweetpotato",
		latitude = lat,
		longitude = as.numeric(r$"GPS Longitude"),
		elevation = as.numeric(r$"GPS Altitude"),
		date = as.character(when),
		geo_from_source = TRUE,
		on_farm = FALSE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
