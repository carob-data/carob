# R script for "carob"
# license: GPL (>=3)

## NOTES
# Endline household survey of the Nutritious Sweetpotato for Niassa project, Mozambique, 2016. This script uses the household demographic file. hh_id_new is unique.
# The dictionary codes household-head sex as 1 = Masculino and 2 = Feminino. Province, district, administrative post, and village are text in the file.
# Country is Mozambique in the geographic coverage. Children under 5 years are the sum of the under-5-month, 6-23-month, and 24-59-month counts. Elders are the count aged 65 or older.

## ISSUES
# The latitude and longitude columns are 7-digit integers, not decimal degrees, and the ranges do not fit one encoding, so they were not converted.
# Admin names were not matched to GADM.
# missing variables: latitude, longitude
# unknown variables: survey_group_

## NEW VARIABLES
# survey_group_: intervention area or control area, as recorded.

## NOT PROCESSED
# Community, the place between the administrative post and the village.
# Education of the household head. The dictionary does not list the codes.
# Altitude. Values include 0 and 9995.
# Crop area, production, sales, diet modules, the questionnaire, and the pdf report.


carob_script <- function(path) {

"
Dataset for: Nutritious Orange-Fleshed Sweetpotato (OFSP) For Niassa - Endline Household Survey
"

	uri <- "doi:10.21223/P3/7TRXLT"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "A01 to A23 HH Demographic Data.sav"]
	r <- haven::read_sav(f)
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
	sex <- c("1" = "male", "2" = "female")[txt(r$gender_hh_head)]
	under5 <- as.numeric(r$nr_children_less_5_month) +
		as.numeric(r$nr_children_between_6_23_month) +
		as.numeric(r$nr_children_between_24_59_month)

	d <- data.frame(
		trial_id = "OFSP_Niassa_endline_2016",
		hhid = txt(r$hh_id_new),
		country = "Mozambique",
		adm1 = carobiner::fix_name(txt(r$Province), "title"),
		adm2 = carobiner::fix_name(txt(r$Districts), "title"),
		adm3 = carobiner::fix_name(txt(r$Post), "title"),
		location = carobiner::fix_name(txt(r$Village), "title"),
		crop = "sweetpotato",
		sex = unname(sex),
		age = as.numeric(r$age_hh_head),
		hh_size = as.numeric(r$nr_hh_member),
		hh_child_5 = under5,
		hh_elders = as.numeric(r$nr_hh_member_more_than_65_years),
		survey_group_ = txt(r$hh_type_st),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
