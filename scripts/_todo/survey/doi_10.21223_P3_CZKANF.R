# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mama SASHA vine-voucher redemptions in Bungoma and Busia, Kenya. Country is Kenya from geographic coverage. This script keeps the voucher file (one row per redemption). link_ANC_id is the person id and can repeat.
# Stata value labels: sex 1 = female and 2 = male. Day 99, month 99, and year 9999 are the missing codes. Place is the village name, then the location name, then the division name, when the finer name is blank.

## ISSUES
# No latitude or longitude. Several voucher rows share a person, place, sex, and date, so they are identical once the vine counts are left unprocessed.
# missing variables: latitude, longitude
# duplicates: duplicate records detected

## NOT PROCESSED
# A2 home visits, A3 and A4 agricultural monitoring, A5 knowledge, A6 food frequency, H1 to H8 community-health and clinic files, the monitoring-forms workbook, and the docx dictionaries.
# Voucher counts for Kabode and Vita, vine multiplier, and health-facility columns on the A1 file.
# A2 - Monthly Home Visits.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# A3 - Monitoring Agricultural Activities.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# A4 - Monitoring Agricultural Activities by CIP agronomist.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# A5 - Knowledge Assessment.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# A6 - Food Frequency.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# H1 - CHW Group Profile.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# H2 - CHW Pregnant Woman Club Attendance.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# H3 - CHW Monthly Summary.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# H5 - ANC Delivery.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# H6 - PNC Delivery.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# H8 - ANC Nurses Monthly Report.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# MAMA SASHA MONITORING FORMS.xls: not read. sheet Summary actors & forms (of 26): Monitoring forms, ...2, Main activities...3, ...4, ...5, ...6, ...7, ...8, ...9, ...10, Main activities...11, ...12, .... Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Dataset for: Replication Data for: MAMA SASHA Monitoring Data

The Mama SASHA project was set in Bungoma county in Kenya’s Western Province. The overall project goal was to improve the health status of pregnant women and the nutritional status of children up to two years through an integrated OFSP and health service-delivery strategy. Nurses dispensed vouchers for OFSP vines, which women could redeem for cuttings of the Kabode and Vita varieties.
"

	uri <- "doi:10.21223/P3/CZKANF"
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
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "A1 - Voucher redemption.dta"]
	r <- haven::read_dta(f)
	sex <- as.character(as.integer(r$sex))
	sex <- c("1" = "female", "2" = "male")[sex]
	day <- as.integer(r$day)
	month <- as.integer(r$month)
	year <- as.integer(r$year)
	day[day == 99] <- NA_integer_
	month[month == 99] <- NA_integer_
	year[year == 9999] <- NA_integer_
	ok <- !is.na(day) & !is.na(month) & !is.na(year)
	ok[ok] <- month[ok] >= 1 & month[ok] <= 12 & day[ok] >= 1
	mdays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
	maxd <- rep(NA_integer_, length(month))
	maxd[ok] <- mdays[month[ok]]
	feb <- which(ok & month == 2 & (year %% 4 == 0))
	maxd[feb] <- 29
	ok[ok] <- day[ok] <= maxd[ok]
	date <- rep(NA_character_, nrow(r))
	date[ok] <- as.character(as.Date(paste(year[ok], month[ok], day[ok], sep = "-")))
	village <- as.character(r$namevill)
	village[is.na(village)] <- ""
	village <- carobiner::fix_name(village, "title")
	locn <- as.character(r$nameloc)
	locn[is.na(locn)] <- ""
	locn <- carobiner::fix_name(locn, "title")
	division <- as.character(r$namediv)
	division[is.na(division)] <- ""
	division <- carobiner::fix_name(division, "title")
	place <- village
	place[is.na(place)] <- locn[is.na(place)]
	place[is.na(place)] <- division[is.na(place)]

	d <- data.frame(
		trial_id = "Mama_SASHA_voucher_Kenya",
		hhid = trimws(as.character(as.integer(r$link_ANC_id))),
		country = "Kenya",
		location = place,
		crop = "sweetpotato",
		sex = unname(sex),
		date = date,
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
