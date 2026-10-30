# R script for "carob"
# license: GPL (>=3)

## NOTES
# Clone-level virus reactions, late-blight rAUDPC, and marketable tuber weight at 80, 100 and 120 days. Not plot replicates.
# The Dataverse geographic coverage is Peru. The late-blight columns are named Yunnan 2015 and Yunnan 2016.
# Marketable tuber weight has no unit.

## ISSUES
# Country for the Yunnan columns is not stated. Rows use the dataset country, Peru. Confirm whether those trials were in China.
# No coordinates, dates, or fertilizer.
# rAUDPC includes -0.003, below 0. Left as recorded.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: pvx_class_, pvy_class_, plrv_class_, pvs_class_, pvx_infection_, pvy_infection_, plrv_infection_, pvs_infection_, mtwp_. out of bounds rAUDPC (-0.003, 0.924).

## NOT PROCESSED
# Virus absorbance and infected-plant counts: the resistance class and the infection proportion are kept.
# SLOPE and the early-bulking label: derived from the three tuber weights.
# Material list pedigree.

## NEW VARIABLES
# pvx_class_, pvy_class_, plrv_class_, pvs_class_: resistance class as recorded.
# pvx_infection_, pvy_infection_, plrv_infection_, pvs_infection_: infection proportion as recorded.
# mtwp_: marketable tuber weight. Unit is not stated.


carob_script <- function(path) {

"
Dataverse for: Evaluation of CIP advanced clones for late blight and virus resistance and early bulking

This dataset contains the evaluation of the International Potato Center (CIP) advanced clones for late blight and virus resistance and early bulking
"

	uri <- "doi:10.21223/DSRBEV"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "rAUDPC",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "13394_Data.xlsx"]
	v <- carobiner::read.excel(f, sheet="virus resistance")
	lb <- carobiner::read.excel(f, sheet="Late blight resistance")
	bk <- carobiner::read.excel(f, sheet="MTW at 80-100-120 d")

	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+([eE][+-]?[0-9]+)?$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	d1 <- data.frame(
		record_id = as.integer(seq_len(nrow(v))),
		variety = trimws(as.character(v$ACCENUMB)),
		treatment = trimws(as.character(v$ACCENUMB)),
		pvx_class_ = trimws(as.character(v$PVX)),
		pvy_class_ = trimws(as.character(v$PVY)),
		plrv_class_ = trimws(as.character(v$PLRV)),
		pvs_class_ = trimws(as.character(v$PVS)),
		pvx_infection_ = num(v[["Percentage of PVX infection"]]),
		pvy_infection_ = num(v[["Percentage of PVY infection"]]),
		plrv_infection_ = num(v[["Percentage of PLRV infection"]]),
		pvs_infection_ = num(v[["Percentage of PVS infection"]]),
		trial_id = "virus"
	)

	d2 <- data.frame(
		record_id = as.integer(seq_len(nrow(lb)) + nrow(v)),
		variety = trimws(as.character(lb$ACCENUMB)),
		treatment = trimws(as.character(lb$ACCENUMB)),
		rAUDPC = num(lb$rAUDPC_Yunnan2015),
		location = "Yunnan",
		trial_id = "late_blight_2015"
	)
	d2b <- d2
	d2b$record_id <- as.integer(seq_len(nrow(lb)) + nrow(v) + nrow(lb))
	d2b$rAUDPC <- num(lb$rAUDPC_Yunnan2016)
	d2b$trial_id <- "late_blight_2016"

	id <- as.integer(seq_len(nrow(bk)) + nrow(v) + 2L * nrow(lb))
	d3 <- data.frame(
		record_id = id,
		variety = trimws(as.character(bk$ACCENUMB)),
		treatment = trimws(as.character(bk$ACCENUMB)),
		trial_id = "early_bulking"
	)
	wts <- c("MTWP 80 days after planting", "MTWP 100 days after planting", "MTWP 120 days after planting")
	days <- c(80L, 100L, 120L)
	pieces <- lapply(seq_along(days), function(i) {
		data.frame(
			record_id = id,
			DAP = days[i],
			mtwp_ = num(bk[[wts[i]]])
		)
	})
	dlong <- do.call(rbind, pieces)
	dlong <- dlong[!is.na(dlong$mtwp_), ]

	d <- carobiner::bindr(d1, d2, d2b, d3)
	d$crop <- "potato"
	d$country <- "Peru"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$stress <- "disease"
	d$pathogen <- NA_character_
	d$pathogen[d$trial_id %in% c("late_blight_2015", "late_blight_2016")] <- "Phytophthora infestans"

	carobiner::write_files(path, meta, d, long=dlong)
}
