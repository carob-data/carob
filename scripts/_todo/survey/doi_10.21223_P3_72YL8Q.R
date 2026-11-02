# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rwanda Super Foods baseline, November-December 2011, five districts. This script uses the cover page. Household number is unique in the file.
# A07 is the status of the household head. Man is stored as male. The three woman-headed statuses are stored as female. Missing is left blank.
# A08B1 is the number of resident people. A15G is elevation in meters. Country is Rwanda in the geographic coverage. There is no latitude or longitude column.

## ISSUES
# Province is stored as North or South, not the GADM names Northern Province and Southern Province.
# Four elevation values are 0 or below, which is not plausible for these highland districts. They are kept.
# missing variables: latitude, longitude

## NOT PROCESSED
# Adult and child rosters, housing, assets, crops, sweetpotato production, diet, nutrition knowledge, and food security.
# The questionnaire workbook and the dictionary.
# A13A is a GPS correction factor, not a coordinate.


carob_script <- function(path) {

"
Replication Data for: Rwanda Super Foods Project Baseline Survey
"

	uri <- "doi:10.21223/P3/72YL8Q"
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
		carob_completion = 25,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "RW_sf_b_coverpage.dta"]
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
	st <- txt(r$A07)
	sex <- rep(NA_character_, length(st))
	sex[st == "Man"] <- "male"
	sex[st %in% c(
		"Woman HH with the support of a non-resident man",
		"Woman without the support of a man",
		"Woman HH with resident man"
	)] <- "female"
	when <- as.Date(sprintf("%04d-%02d-%02d", as.integer(r$A16C), as.integer(r$A16B), as.integer(r$A16A)))

	d <- data.frame(
		trial_id = "Rwanda_SuperFoods_2011",
		hhid = txt(r$hhid),
		country = "Rwanda",
		adm1 = carobiner::fix_name(txt(r$prov), "title"),
		adm2 = carobiner::fix_name(txt(r$dist), "title"),
		adm3 = carobiner::fix_name(txt(r$sec), "title"),
		adm4 = carobiner::fix_name(txt(r$cell), "title"),
		location = carobiner::fix_name(txt(r$A04A), "title"),
		crop = "sweetpotato",
		sex = sex,
		hh_size = as.numeric(r$A08B1),
		elevation = as.numeric(r$A15G),
		date = as.character(when),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
