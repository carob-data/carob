# R script for "carob"
# license: GPL (>=3)

## NOTES
# One row per genotype and site from 01_Yield-data.xlsx (Kenya National Performance Trials across six Kenyan KALRO/NPT sites: Embu, Kabete, Kianjai, Mabanga, Maragua, Mois Bridge).
# Site columns are t/ha ("TOTAL TUBER WGT T_HA" in 03_2YR COMBINED MEANS.xlsx). Stored as kg/ha.
# Site coordinates added from gazetteer (geo_from_source = FALSE).
# A cell value of "." is missing and is stored as NA, not as zero.

## ISSUES
# No planting/harvest dates, fertilizer rates, irrigation, yield moisture, or replicate-level rows.
# Missing yield cells in the file are "." and stored as NA.

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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 50,
		carob_effort = 0
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
		}
		)
	})
	d <- do.call(rbind, parts)
	geo <- data.frame(
		location = c("Embu", "Kabete", "Kianjai", "Mabanga", "Maragua", "Mois Bridge"),
		latitude = c(-0.538, -1.251, 0.133, 0.625, -0.793, 0.878),
		longitude = c(37.458, 36.744, 37.767, 34.617, 37.131, 35.121)
	)
	gm <- match(d$location, geo$location)
	d$country <- "Kenya"
	d$latitude <- geo$latitude[gm]
	d$longitude <- geo$longitude[gm]
	d$geo_from_source <- FALSE
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
