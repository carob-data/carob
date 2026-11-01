# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline household survey for the 2016 OFDA sweetpotato project in southern Mozambique. This script uses the household location module. hh_id is unique.
# The codebook codes household-head sex as 1 = Masculino and 2 = Feminino. Province, district, administrative post, and village are columns.
# Country is Mozambique in the geographic coverage. Altitude is meters.

## ISSUES
# Latitude and longitude are not decimal degrees. Digit length and minutes are inconsistent, so they were not converted.
# Admin and village spellings are as recorded and were not matched to GADM.
# Interview dates are stored as integers of mixed length.
# missing variables: latitude, longitude

## NOT PROCESSED
# Person names.
# Locality, which sits between the administrative post and the village.
# The other modules (crops, nutrition, climate, livestock, labour, food security) and the version 4 copies.
# Codebook and the Stata log.


carob_script <- function(path) {

"
Dataset for: Baseline Survey for the OFDA 2016 Mitigating Drought Impacts on Livelihoods in Mozambique through Resilient, Nutritious Sweetpotato Project in the South of Mozambique
"

	uri <- "doi:10.21223/P3/68IIS3"
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
		carob_completion = 20,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "HH_location_&_Quality_control_v5.dta"]
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
	sex <- c("Masculino" = "male", "Feminino" = "female")[txt(r$gender_hh_head)]

	d <- data.frame(
		trial_id = "OFDA_Mozambique_baseline_2016",
		hhid = txt(r$hh_id),
		country = "Mozambique",
		adm1 = carobiner::fix_name(txt(r$prov), "title"),
		adm2 = carobiner::fix_name(txt(r$dist), "title"),
		adm3 = carobiner::fix_name(txt(r$pa), "title"),
		location = carobiner::fix_name(txt(r$ald), "title"),
		crop = "sweetpotato",
		sex = unname(sex),
		elevation = as.numeric(r$alt),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
