# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mother and baby potato plots in the Bumthang workbook. The Minimal sheet has coordinates and elevation and leaves country blank. Country Bhutan is the Dataverse geographic coverage.
# Installation states a randomized complete block design, 2.8 m2 plots, 0.70 m between rows, and 0.20 m between plants. Those spacings are stored in cm.
# TTWP is stored as yield. Its unit is not stated. TTYA is about 1.8 times TTWP * 10000 / 2.8 / 1000, so it was not treated as t/ha.

## ISSUES
# The title says the 2009 campaign. The workbook begin date is 2013-03-20. Neither was used as a planting date.
# No fertilizer rates.
# write_files: missing carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: ttya_, trial_type_. low yield (tons not kg?): potato. NA detected: yield.

## NOT PROCESSED
# Flowering, storage, organoleptic, dormancy, rank, and PCA sheets in PTPV200908_BUMTHANG.xls, and the separate xlsx extracts of those sheets.
# TTYA_HM and TTYA_HB summary grids.
# 01_data_F1_Select_crit.xlsx: not read. sheet F1_Selec_Crit: ORD, PHASE, Identified criteria, SCMEN, OR_M, SCWM, OR_WM, SGLOB, PMEN, PWM, PGLOB. Observation records are missing from d until this file is standardized.
# 02_data_F2 Clones_Select_Flowering.xlsx: not read. sheet Sheet1: PLOT, REP, INSTN, Country, Admin1, Admin2, Locality, NTP, NPE, NPH, PPH, NNoMTP, .... Observation records are missing from d until this file is standardized.
# 02_data_F2_Select_crit.xlsx: not read. sheet F2 Clones_Select_Flowering: PLOT, REP, INSTN, Code, MSM, MSWM, MSGLO, BSM, BSWM, BSGLO. Observation records are missing from d until this file is standardized.
# 03_data_F3_Clones_Selec_Harvest.xlsx: not read. sheet F3_Clones_Selec_Harvest: PLOT, REP, INSTN, Code, MSM, MSWM, MSGLO, BSM, BSWM, BSGLO. Observation records are missing from d until this file is standardized.
# 04_data_yield_F4_ Harvest_Mother.xlsx: not read. sheet F4_ Harvest_Mother: PLOT, REP, INSTN, Code, NTP, NPH, PPH, NMTP, NNoMTP, NMTPL, MTWP, NoMTWP, .... Observation records are missing from d until this file is standardized.
# 05_data_yield_F5_harvest_Baby.xlsx: not read. sheet F5_harvest_Baby: PLOT, REP, INSTN, Code, NTP, NPH, PPH, NMTP, NNoMTP, NMTPL, MTWP, NoMTWP, .... Observation records are missing from d until this file is standardized.
# 06_data_F9_Clones_Select_Storage.xlsx: not read. sheet F9_Clones_Select_Storage: PLOT, REP, INSTN, Code, STYPE, SCMEN, SCWM, SGLOB, PMEN, PWM, PGLOB. Observation records are missing from d until this file is standardized.
# 07_data_Data_organoleptic_Mother.xlsx: not read. sheet Data_organoleptic_Mother: INSTN, PanelNo, Sex, appearance, taste, texture. Observation records are missing from d until this file is standardized.
# 08_data_Data_organoleptic_Baby.xlsx: not read. sheet Data_organoleptic_Baby: INSTN, PanelNo, Sex, appearance, taste, texture. Observation records are missing from d until this file is standardized.
# 09_Material_list.xlsx: not read. sheet Sheet1: Numeration, Institutional number, Clone or variety name, Female Institutional number, Female code, Male Institutional number, References to previous trials. Observation records are missing from d until this file is standardized.
# 10_Data_dictionary.xlsx: documentation or a codebook, not an observation table.

## NEW VARIABLES
# trial_type_: mother or baby.
# ttya_: total tuber yield adjusted, unit not stated.


carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones Through Farmer Selection in Buthhang

A select group of 8 potato clones will be evaluated through a Farmer Participatory Variety Selection (PVS) in Buthhang during the 2009 campaign. Yield components and post-harvest traits will be determined
"

	uri <- "doi:10.21223/M97ZQS"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.45
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
	f <- ff[basename(ff) == "PTPV200908_BUMTHANG.xls"]
	r1 <- carobiner::read.excel(f, sheet="Minimal")
	val <- function(key) {
		i <- which(trimws(r1[[1]]) == key)
		if (!length(i)) NA_character_ else chr(r1[[2]][i[1]])
	}
	one <- function(sheet, kind) {
		r <- carobiner::read.excel(f, sheet=sheet)
		data.frame(
			variety = chr(r$INSTN),
			treatment = chr(r$INSTN),
			rep = as.integer(num(r$REP)),
			yield = num(r$TTWP),
			ttya_ = num(r$TTYA),
			trial_type_ = kind
		)
	}
	d <- rbind(
		one("F4_ Harvest_Mother", "mother"),
		one("F5_harvest_Baby", "baby")
	)
	d <- d[!is.na(d$variety), ]
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$country <- "Bhutan"
	d$location <- "Bumthang"
	d$elevation <- num(val("Elevation"))
	d$latitude <- num(val("Latitude"))
	d$longitude <- num(val("Longitude"))
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$row_spacing <- 70
	d$plant_spacing <- 20
	d$trial_id <- paste("Bumthang", d$trial_type_, d$variety, d$rep, sep="_")
	d$on_farm <- NA
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
