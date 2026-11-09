# R script for "carob"
# license: GPL (>=3)

## NOTES
# VISTA Tanzania monitoring, 2016. This script uses the decentralized vine multiplier (DVM) register. Respondent names are blank. Village code plus sheet number identifies each record.
# Region, district, ward, and village are columns. Country is Tanzania in the geographic coverage (stored as the terminag name Tanzania).
# Land areas are acres and are stored in hectares. Leased land employed by the multiplier is stored as farmland_rentedin. GPS columns on this file are empty.

## ISSUES
# Admin names are the recorded spellings, title case, and were not matched to GADM.
# No latitude or longitude.
# missing variables: latitude, longitude

## NOT PROCESSED
# VST1 vine distribution, VST2 farmer groups, VST3 agricultural monitoring, VST4 training, and VST8 community health worker files.
# DVM names, phone, age band, other crops, irrigation equipment, and net-tunnel measurements.
# Acreage in active production. The column is not clearly cropland rather than all land in use.
# Codebooks.
# VST1 - SP Distribution.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# VST2 - Farmer Group Assessment and Registration.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# VST3 - Monitoring of Agricultural Activities.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# VST4 - Training Event Register.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# VST8A - CHW Group Profile.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# VST8B - CHW Club Attendance.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Datasets for: Monitoring Data for Viable Sweetpotato Technologies in Africa (VISTA)Tanzania
"

	uri <- "doi:10.21223/P3/3NGJLV"
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
		carob_completion = 25,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "VST5 - DVM Mapping.dta"]
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
	sex <- tolower(txt(r$sex))
	acre <- 0.4046863

	d <- data.frame(
		trial_id = "VISTA_Tanzania_DVM_2016",
		hhid = paste(txt(r$village_code), txt(r$sheet), sep = "_"),
		country = "Tanzania",
		adm1 = carobiner::fix_name(txt(r$region_name), "title"),
		adm2 = carobiner::fix_name(txt(r$district_name), "title"),
		adm3 = carobiner::fix_name(txt(r$ward_name), "title"),
		location = carobiner::fix_name(txt(r$village_name), "title"),
		crop = "sweetpotato",
		sex = sex,
		date = as.character(as.Date(r$date_int)),
		farmland_owned = as.numeric(r$acre_owned) * acre,
		farmland_rentedin = as.numeric(r$acre_leased) * acre,
		farmland_rentedout = as.numeric(r$acre_rented_out) * acre,
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
