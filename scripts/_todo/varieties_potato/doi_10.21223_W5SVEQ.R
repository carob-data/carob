# R script for "carob"
# license: GPL (>=3)

## NOTES
# Kenyan national performance trial (NPT) means for two seasons (April-August 2015 and October 2015-February 2016), already averaged. One row per clone.
# TTWP (17.08-31.19) and MTWP (16.16-22.84) are national performance trial mean yields in t/ha, converted to kg/ha (* 1000).

## ISSUES
# Three candidate rows have yield and no accession number. They are labeled "unlabeled candidate".
# Multi-location national performance trial across Kenya with no site names or coordinates reported.
# No plot area, fertilizer, irrigation, yield moisture, or single-season planting/harvest dates (trial window 2015-04 to 2016-02).

## NOT PROCESSED
# TYPE, SOURCE, TEST STATUS, NPT YRS, DUS YRS, percent above checks, CONTRAST, NMTP, TNTP, THINN, HARV: trial administration or counts without a plot area.
# Disease columns PRLV, PVX, PVY, BLT and BWLT: the scale is not stated.
# 4415_DataDictionary_ElementDescription.xlsx: documentation or a codebook, not an observation table.
# 4415_DataDictionary_UniqueIdentifier.xlsx: documentation or a codebook, not an observation table.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 50,
		carob_effort = 0.35
	)

	f1 <- ff[basename(ff) == "4415_Data.xlsx"]
	r <- carobiner::read.excel(f1)
	f2 <- ff[basename(ff) == "4415_Material_List.xlsx"]
	r1 <- carobiner::read.excel(f2)
	variety <- trimws(as.character(r$ACCENUMB))
	miss <- is.na(variety) | !nzchar(variety)
	variety[miss] <- paste("unlabeled candidate", seq_len(sum(miss)))
	d <- data.frame(
		variety = variety,
		treatment = variety,
		yield = as.numeric(r$TTWP) * 1000,
		yield_marketable = as.numeric(r$MTWP) * 1000
	)
	alias <- data.frame(
		variety = trimws(as.character(r1$Accession_Number)),
		variety_alt = trimws(as.character(r1$Accession_Name))
	)
	alias$variety_alt[!nzchar(alias$variety_alt)] <- NA_character_
	alias <- alias[!duplicated(alias$variety), ]
	d <- merge(d, alias, by="variety", all.x=TRUE)
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	d$country <- "Kenya"
	d$planting_date <- "2015-04"
	d$harvest_date <- "2016-02"
	d$trial_id <- "Kenya_NPT_2015_2016"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
