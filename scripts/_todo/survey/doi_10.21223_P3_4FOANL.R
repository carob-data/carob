# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rapid market assessment in the VISTA Tanzania districts, July 2016. This script uses the trader and distributor interviews.
# Trader number repeats across markets. Village code plus trader number is unique. The trading centre is the location.
# Country is Tanzania in the geographic coverage (stored as the terminag name Tanzania). Interview dates are already year-month-day.

## ISSUES
# Admin names are the recorded spellings, title case, and were not matched to GADM.
# Waging'ombe is stored with a Windows-1252 apostrophe. It is converted to UTF-8.
# No latitude or longitude.
# missing variables: latitude, longitude

## NOT PROCESSED
# Customer interviews. Customer number plus village is not unique.
# Product, price, credit, and practice questions on the trader file.
# Data dictionaries and the pdf tools.
# Customers Data Dictionary: documentation or a codebook, not an observation table.
# Market Survey - Customers.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Traders & Distributors Data Dictionary: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Replication Data for: Rapid market assessment: Viable Sweetpotato Technologies in Africa – Tanzania
"

	uri <- "doi:10.21223/P3/4FOANL"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Market Survey - Traders & Distributors.dta"]
	r <- haven::read_dta(f)
	txt <- function(x) {
		if (inherits(x, "haven_labelled") || is.factor(x)) {
			x <- haven::as_factor(x)
		}
		x <- as.character(x)
		bad <- !is.na(x) & is.na(iconv(x, from = "UTF-8", to = "UTF-8"))
		if (any(bad)) {
			x[bad] <- iconv(x[bad], from = "CP1252", to = "UTF-8")
		}
		trimws(x)
	}

	d <- data.frame(
		trial_id = "VISTA_Tanzania_market_2016",
		hhid = paste(txt(r$village_code), txt(r$trader_id), sep = "_"),
		country = "Tanzania",
		adm1 = carobiner::fix_name(txt(r$region_name), "title"),
		adm2 = carobiner::fix_name(txt(r$district_name), "title"),
		adm3 = carobiner::fix_name(txt(r$ward_name), "title"),
		location = carobiner::fix_name(txt(r$centre_name), "title"),
		crop = "sweetpotato",
		sex = tolower(txt(r$sex)),
		age = as.numeric(r$age),
		date = as.character(as.Date(r$interview_date)),
		on_farm = FALSE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
