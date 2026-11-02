# R script for "carob"
# license: GPL (>=3)

## NOTES
# Endline household identification for Jumpstarting OFSP in West Africa, 2017. Ghana and Nigeria are separate files with the same columns. num_iid is unique inside each file, so the household id is the country plus that number.
# Admin codes are stored with the place name (for example "1006 - Northern"). The name after the code is used. Where the community is Other and the specify field is a name, that name is used.
# Country is Ghana or Nigeria, matching the file and the geographic coverage. Walking minutes to the nearest market are minutes.

## ISSUES
# Burkina Faso is in the geographic coverage and there is no Burkina Faso file.
# Some Nigeria communities stay Other because the specify field is not a place name.
# Admin names were not matched to GADM.
# missing variables: latitude, longitude
# unknown variables: survey_group_

## NEW VARIABLES
# survey_group_: household category as recorded (intervention or control).

## NOT PROCESSED
# Parts 2 to 8 (demographics, crops, practices, marketing, food security) for both countries.
# Distances to market, road, and office. The unit column mixes kilometres, miles, and metres.
# The Excel dictionary. Person names.


carob_script <- function(path) {

"
Dataset for: Endline Survey: Jumpstarting Orange-fleshed Sweetpotato in West Africa through Diversified Markets
"

	uri <- "doi:10.21223/P3/77B32X"
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
		carob_completion = 15,
		carob_effort = 0.45
	)

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
	place <- function(x) {
		x <- txt(x)
		trimws(sub("^[0-9]+[[:space:]]*-[[:space:]]*", "", x))
	}
	one <- function(r, country) {
		loc <- place(r$admin3cd)
		oth <- txt(r$admin3cd_other)
		i <- which(loc == "Other" & !oth %in% c("None", "1", ""))
		loc[i] <- oth[i]
		when <- as.Date(sprintf(
			"%04d-%02d-%02d",
			as.integer(r$m02_08c),
			as.integer(r$m02_08b),
			as.integer(r$m02_08a)
		))
		data.frame(
			trial_id = "OFSP_WestAfrica_endline_2017",
			hhid = paste(country, txt(r$num_iid), sep = "_"),
			country = country,
			adm1 = carobiner::fix_name(place(r$admin1cd), "title"),
			adm2 = carobiner::fix_name(place(r$admin2cd), "title"),
			location = carobiner::fix_name(loc, "title"),
			crop = "sweetpotato",
			sex = tolower(txt(r$sex)),
			date = as.character(when),
			market_timeto = as.numeric(r$near_mkt_walking_min),
			survey_group_ = txt(r$m02_10),
			on_farm = TRUE,
			is_survey = TRUE,
			geo_from_source = FALSE
		)
	}

	f1 <- ff[basename(ff) == "Part 1 - Household Identication (Ghana).sav"]
	f2 <- ff[basename(ff) == "Part 1 - Household Identication (Nigeria).sav"]
	d1 <- one(haven::read_sav(f1), "Ghana")
	d2 <- one(haven::read_sav(f2), "Nigeria")
	d <- rbind(d1, d2)

	carobiner::write_files(path, meta, d)
}
