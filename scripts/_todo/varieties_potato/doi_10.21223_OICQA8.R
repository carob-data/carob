# R script for "carob"
# license: GPL (>=3)

## NOTES
# One row per genotype and site from 01_Yield-data.xlsx.
# Site columns are t/ha. Sheet 03 of 03_2YR COMBINED MEANS.xlsx labels the same quantity "TOTAL TUBER WGT T_HA", and those site means match the MEAN column on sheet 01. Stored as kg/ha.
# Crop is potato: the measured trait is total tuber weight and the entries are CIP clone numbers.
# A cell value of "." is missing and is stored as NA, not as zero.
# Site names are stored in title case. The file prints them in uppercase.

## ISSUES
# Country is not in the dataset. It is not inferred from the site names.
# No coordinates, dates, fertilizer, or replicate rows.
# Missing yield cells in the file are the token "." and are stored as NA.
# write_files: missing variables: carob_contributor; yield_moisture, country, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: NA detected: yield.

## NOT PROCESSED
# 01_Yield-data.xlsx columns TYPE (all TABLE), NPT YRS, DUS YRS, and MEAN (the across-site mean, not a plot).
# 02_COMBINED_MEANS.xlsx, 03_2YR COMBINED MEANS.xlsx, 04_Stability.xlsx: combined means, disease scores, and stability statistics. Disease scores are not copied onto every site.

carob_script <- function(path) {

"
Dataset for: National Performance Trials and Durability, Uniformity and Stability data

National performance trials and durability data.
"

	uri <- "doi:10.21223/OICQA8"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "01_Yield-data.xlsx"]
	r <- carobiner::read.excel(f, sheet="Yield-data")
	sites <- c("EMBU", "KABETE", "KIANJAI", "MABANGA", "MARAGUA", "MOIS BRIDGE")
	parts <- lapply(sites, function(s) {
		data.frame(
			variety = trimws(as.character(r$EXP_NAME)),
			location = tools::toTitleCase(tolower(s)),
		yield = {
			y <- trimws(as.character(r[[s]]))
			y[y == "."] <- NA_character_
			as.numeric(y) * 1000
		},
			stringsAsFactors = FALSE
		)
	})
	d <- do.call(rbind, parts)
	d$crop <- "potato"
	d$treatment <- d$variety
	d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	d$trial_id <- paste(d$location, "NPT", sep="_")
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA

	carobiner::write_files(path, meta, d)
}
