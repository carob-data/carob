# R script for "carob"
# license: GPL (>=3)

## NOTES
# Diploid potato clones at Holetta, Ethiopia. Country, admin units, locality, elevation, and coordinates are the Minimal sheet of the yield workbook.
# TTWP is the yield column. The sheet does not state the unit, so the number was not multiplied. TTYA is empty in this file.
# FeDW, ZnDw, and DM are accession-level mineral values joined by accession number. Their units are not stated on the minerals sheet.

## ISSUES
# Begin_date is an Excel serial and End_date is 15/07/2015. They were not used as planting and harvest dates.
# write_files: missing carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: block_id, fe_dw_, zn_dw_, dm_. low yield (tons not kg?): potato.

## NOT PROCESSED
# NTP, NPH, category weights, insect counts, and the material list. The element-description dictionary is documentation.
# 4029_DataDictionary_ElementDescription.xlsx: documentation or a codebook, not an observation table.
# 4029_DataDictionary_UniqueIdentifier.xlsx: documentation or a codebook, not an observation table.

## NEW VARIABLES
# fe_dw_, zn_dw_, dm_: mineral sheet columns, unit not stated.


carob_script <- function(path) {

"
Dataset for: 100 promising diploid clones from biofortification cycle III selected and assessed in Ethiopia

Summary of selected genotypes from cycle III bio-fortified genotypes at Holetta/Ethiopia  during 2015 irrigation season. Based on Fe-Zn content 40 clones were selected, of which 5 were also comparatively high yielding
"

	uri <- "doi:10.21223/SRI1ZZ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
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
		carob_completion = 55,
		carob_effort = 0.35
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	f1 <- ff[basename(ff) == "4029_PTYield2015_Hole_Harvesting_parameters.xlsx"]
	f2 <- ff[basename(ff) == "4029_2015_Minerals.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Yield")
	r2 <- carobiner::read.excel(f2, sheet="Minerals_2015")
	minr <- data.frame(
		variety = chr(r2$ACCENUMB),
		fe_dw_ = num(r2$FeDW),
		zn_dw_ = num(r2$ZnDw),
		dm_ = num(r2$DM)
	)
	minr <- minr[!is.na(minr$variety) & !duplicated(minr$variety), ]
	d <- data.frame(
		variety = chr(r1$ACCENUMB),
		treatment = chr(r1$ACCENUMB),
		block_id = chr(r1$BLOCK),
		yield = num(r1$TTWP)
	)
	d <- merge(d, minr, by="variety", all.x=TRUE)
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$country <- "Ethiopia"
	d$adm1 <- "Oromiya"
	d$adm2 <- "Finifine zuria"
	d$adm3 <- "Wolmera"
	d$location <- "Holetta"
	d$elevation <- 2400
	d$latitude <- 8.88333
	d$longitude <- 38.38333
	d$geo_from_source <- TRUE
	d$trial_id <- paste0("Holetta_2015_", seq_len(nrow(d)))
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d <- d[!is.na(d$variety), ]

	carobiner::write_files(path, meta, d)
}
