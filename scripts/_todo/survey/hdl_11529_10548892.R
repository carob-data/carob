# R script for "carob"
# license: GPL (>=3)

## NOTES
# HarvestPlus household survey in Zambia, June 2011, storage section. Coordinates, province, village, and elevation are in the sav file. geo_from_source is TRUE.
# The SPSS syntax computes Pr_lo_cob and Pr_lo_gr as the recorded loss percentage divided by 100. They are stored again as percent.
# Crop is maize. Cob and grain losses are farmer estimates for the 2010-2011 storage season.
# Temperature, rainfall, and relative humidity in the file are WorldClim or other secondary grids, not station measurements, and are not stored.

## ISSUES
# The 1-5 pest-importance codes are not labeled in the sav file or the syntax.
# District is a numeric code without a name lookup.
# write_files: unknown variable: interview_date_ (date?). unknown variables: storage_loss_cob_percent_, storage_loss_grain_percent_, lgb_cob_score_, lgb_grain_score_, weevil_cob_score_, weevil_grain_score_. duplicate coordinates: fewer coordinates than locations. duplicates: duplicate records detected.

## NEW VARIABLES
# storage_loss_cob_percent_: farmer-estimated percent loss of maize stored as cobs.
# storage_loss_grain_percent_: farmer-estimated percent loss of maize stored as grain.
# lgb_cob_score_: 1-5 importance of larger grain borer in stored cobs, as recorded.
# lgb_grain_score_: 1-5 importance of larger grain borer in stored grain, as recorded.
# weevil_cob_score_: 1-5 importance of maize weevil in stored cobs, as recorded.
# weevil_grain_score_: 1-5 importance of maize weevil in stored grain, as recorded.
# interview_date_: interview date.

## NOT PROCESSED
# WorldClim temperature, rainfall, relative humidity, agro-ecological zone flags, and market access.
# District codes, SEA numbers, and whether cobs were shelled.
# The questionnaire PDF and the SPSS syntax.


carob_script <- function(path) {

"
Harvestplus household survey, Zambia 2011, section on storage and climate

In this data base, representative georeferenced farmer survey data from Zambia from 2011 are combined with climate data to estimate storage losses, analyze their relationship with climate, and estimate the effect of climate change on storage losses. The storage loss data include importance of different pests (maize weevils and larger grain borer), and farmers’ estimates of storage loss due to both pests, in grain and cobs. The climate data include temperature (from WorldClim) and relative humidity (from CHIRTS) over the storage season in 2011.
"

	uri <- "hdl:11529/10548892"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "HarvestPlus",
		design = NA,
		data_type = "survey",
		response_vars = "storage_loss_cob_percent_;storage_loss_grain_percent_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[grepl("\\.sav$", basename(ff), ignore.case=TRUE)]
	r <- haven::read_sav(f)
	num <- function(x) as.numeric(haven::zap_labels(x))
	chr <- function(x) {
		x <- trimws(as.character(haven::as_factor(x)))
		x[!nzchar(x)] <- NA_character_
		x
	}
	when <- r$date
	if (inherits(when, "POSIXt") || inherits(when, "Date")) {
		when <- as.character(as.Date(when))
	} else {
		when <- trimws(as.character(when))
		when[!nzchar(when)] <- NA_character_
	}

	d <- data.frame(
		country = "Zambia",
		adm1 = chr(r$prov),
		location = chr(r$vill),
		hhid = as.character(num(r$hhid)),
		trial_id = as.character(num(r$hhid)),
		crop = "maize",
		latitude = num(r$Lat_DD),
		longitude = num(r$Lon_DD),
		elevation = num(r$elevation),
		geo_from_source = TRUE,
		storage_loss_cob_percent_ = num(r$Pr_lo_cob) * 100,
		storage_loss_grain_percent_ = num(r$Pr_lo_gr) * 100,
		lgb_cob_score_ = num(r$lgb_cob),
		lgb_grain_score_ = num(r$lgb_grn),
		weevil_cob_score_ = num(r$weevil_cob),
		weevil_grain_score_ = num(r$weevil_grn),
		interview_date_ = when,
		on_farm = TRUE,
		is_survey = TRUE
	)
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$location <- carobiner::fix_name(d$location, "title")

	carobiner::write_files(path, meta, d)
}
