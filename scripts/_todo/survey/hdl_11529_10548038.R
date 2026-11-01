# R script for "carob"
# license: GPL (>=3)

## NOTES
# TAMASA Tanzania agronomy panel, 2017, focal plot. Country in geographicCoverage is Tanzania.
# The focal-plot table has the main crop, the farmer's area estimate, and GPS. It does not have a harvest or yield column.
# The metadata does not state whether area_est is acres or hectares, so it is not stored as field_size.
# Coordinates are in the file. geo_from_source is TRUE.
# fiwi and "other" are not crop names in the vocabulary and are left out.
# region and district are codes such as N01 and N0101. The focal-plot file has no name key for them, so they are not stored as adm1 or adm2.
# Coordinates marked confidential are left empty.

## ISSUES
# unknown variables: farmer_area_
# NA detected: latitude, longitude

## NEW VARIABLES
# farmer_area_: the farmer's estimate of focal-plot area. The unit is not stated.

## NOT PROCESSED
# Fertilizer, seed, weed, and community modules.
# Household modules other than the focal plot.
# Crops recorded as fiwi or other.
# Region and district codes.


carob_script <- function(path) {

"
TAMASA Tanzania agronomy panel survey 2017, focal plot

Main crop and plot area on the focal field of each household.
"

	uri <- "hdl:11529/10548038"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "TZAPS17_fp.dta"]
	r <- carobiner::read.dta(f)
	cmap <- c(
		maize="maize", beans="common bean", tomatoes="tomato", banana="banana",
		sorghum="sorghum", groundnut="groundnut", coffee="coffee", cowpeas="cowpea",
		sunflower="sunflower", pigeon_pea="pigeon pea", green_gram="mung bean",
		irish_potatoes="potato", pyrethrum="pyrethrum", field_peas="pea",
		soyabeans="soybean", wheat="wheat"
	)
	raw <- trimws(as.character(r$mc_crop))
	raw[!nzchar(raw)] <- NA_character_
	crop <- unname(cmap[raw])
	ok <- !is.na(crop)
	r <- r[ok, ]
	crop <- crop[ok]

	gps <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | !nzchar(x) | x %in% c("NA", "-", "***CONFIDENTIAL***")] <- NA_character_
		as.numeric(x)
	}

	d <- data.frame(
		country = "Tanzania",
		latitude = gps(r$X_gpsq1_latitude),
		longitude = gps(r$X_gpsq1_longitude),
		hhid = as.character(r$hhid),
		trial_id = paste(r$hhid, r$plotid),
		crop = crop,
		farmer_area_ = as.numeric(r$area_est),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = TRUE
	)
	carobiner::write_files(path, meta, d)
}
