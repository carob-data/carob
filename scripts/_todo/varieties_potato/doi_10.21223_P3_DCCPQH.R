# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are the YIELD sheet of Minicore_2017_v2.xlsx (DAP, BLOCK, INSTN, CODE, PLANT, FTW, DM, DTW).
# FTW and DTW are not converted to kg/ha. The sheet does not label them as t/ha.
# La Molina, Lima, Peru. Bounding box southLatitude -12.076289, westLongitude -76.948417.
# Images and the rar archive are not plot tables.

## ISSUES
# Yield in kg/ha is not computed.
# No fertilizer. DAP is days, not a calendar planting date.
# get_data warns that Images_v2.rar could not be extracted (an archive entry is marked as a directory).
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: plant_id_, ftw_, dtw_, dm_recorded_.

## NOT PROCESSED
# Sheets A & gs_max, PRI, D13, CC, and NDVI: repeated physiology, not joined here.
# MINICORE 2017 DATA SET.xls, NOTES pdf, Image_description, and the image files.

## NEW VARIABLES
# ftw_: FTW as recorded
# dtw_: DTW as recorded
# dm_recorded_: DM as recorded

carob_script <- function(path) {

"
Dataset for: Ecophysiological and remotely sensed measurements taken in a trial testing potential growth of the potato minicore (2017)

Field experiment to assess physiological performance of 24 different genotypes, using ecophysiologial traits (net photosynthesis, stomatal conductance, carbon isotope discrimination, fluorescence), yield and remotely sensed variables (leaf area, canopy cover, NDVI) taken by octocopter (drone - 14 multiespectral aerial images were adquired) along the growing season of the first trial of potato minicore carried out under an Augmented design during July-November 2017 at La Molina CIP experimental station (Lima-Peru)
"

	uri <- "doi:10.21223/P3/DCCPQH"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "augmented",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "ftw_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Minicore_2017_v2.xlsx"]
	r <- carobiner::read.excel(f, sheet="YIELD")
	num <- function(x) {
		y <- trimws(as.character(x))
		y[y %in% c("", "NA", ".")] <- NA_character_
		as.numeric(y)
	}
	d <- data.frame(
		crop = "potato",
		variety_code = trimws(as.character(r$INSTN)),
		variety = trimws(as.character(r$CODE)),
		block_id = trimws(as.character(r$BLOCK)),
		plant_id_ = trimws(as.character(r$PLANT)),
		DAP = as.integer(round(num(r$DAP))),
		ftw_ = num(r$FTW),
		dtw_ = num(r$DTW),
		dm_recorded_ = num(r$DM),
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = TRUE,
		geo_source = "Dataverse bounding box",
		trial_id = "Minicore_2017",
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = NA
	)
	d$variety[is.na(d$variety) | !nzchar(d$variety)] <- d$variety_code[is.na(d$variety) | !nzchar(d$variety)]
	d$treatment <- d$variety
	d$plot_id <- paste(d$block_id, d$variety_code, d$plant_id_, sep="_")
	carobiner::write_files(path, meta, d)
}
