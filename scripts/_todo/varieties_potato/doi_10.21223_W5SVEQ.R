# R script for "carob"
# license: GPL (>=3)

## NOTES
# Kenyan national performance trial means for two seasons (April-August 2015 and October 2015-February 2016), already averaged. One row per clone.
# TTWP and MTWP are not labeled with a unit. They are stored as recorded.

## ISSUES
# TTWP is about 16-31. write_files reports low yield (tons not kg?). The column does not say t/ha, so the numbers were not multiplied by 1000.
# Three candidate rows have yield and no accession number. They are labeled "unlabeled candidate".
# No coordinates, plot area, fertilizer, or a single planting date.
# write_files: missing carob_contributor, yield_moisture, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. low yield (tons not kg?): potato.

## NOT PROCESSED
# TYPE, SOURCE, TEST STATUS, NPT YRS, DUS YRS, percent above checks, CONTRAST, NMTP, TNTP, THINN, HARV: trial administration or counts without a plot area.
# Disease columns PRLV, PVX, PVY, BLT and BWLT: the scale is not stated.


carob_script <- function(path) {

"
Dataset for: National Performance Trials and Durability, Uniformity and Stability data collected to support release of 2 LB resistant  varieties in Kenya

The results of National Performance trials in Kenya. It holds the average of 2 seasons: April-August 2015 and October 2015 to February 2016.
"

	uri <- "doi:10.21223/W5SVEQ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.3
	)

	r <- carobiner::read.excel(ff[basename(ff) == "4415_Data.xlsx"])
	ml <- carobiner::read.excel(ff[basename(ff) == "4415_Material_List.xlsx"])
	variety <- trimws(as.character(r$ACCENUMB))
	miss <- is.na(variety) | !nzchar(variety)
	variety[miss] <- paste("unlabeled candidate", seq_len(sum(miss)))
	d <- data.frame(
		variety = variety,
		treatment = variety,
		yield = as.numeric(r$TTWP),
		yield_marketable = as.numeric(r$MTWP),
		stringsAsFactors = FALSE
	)
	alias <- data.frame(
		variety = trimws(as.character(ml$Accession_Number)),
		variety_alt = trimws(as.character(ml$Accession_Name)),
		stringsAsFactors = FALSE
	)
	alias$variety_alt[!nzchar(alias$variety_alt)] <- NA_character_
	alias <- alias[!duplicated(alias$variety), ]
	d <- merge(d, alias, by="variety", all.x=TRUE)
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$country <- "Kenya"
	d$trial_id <- "Kenya_NPT_2015_2016"
	d$on_farm <- NA
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
