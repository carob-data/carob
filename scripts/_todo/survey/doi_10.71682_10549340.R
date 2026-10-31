# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household plots in the carbon-farming survey, Haryana (HR) and Madhya Pradesh (MP). Country is India in geographicCoverage.
# Plot area is in a local unit. Land_unit_convertion is how many of those units equal one acre. Hectares are local units divided by that number, times 0.404686 ha per acre.
# Kharif and rabi crops are separate rows. Basmati rice is variety Basmati. Coarse rice is rice.
# Household coordinates are on the main sheet. geo_from_source is TRUE.
# There is no harvest weight. Fallow, "Other", and the grouped vegetable, pulse, and oil classes are left out.

## ISSUES
# unknown variable: season_name_ (season?)

## NOT PROCESSED
# Fertilizer products and quantities. The nutrient contents are not stated.
# Carbon-credit awareness, machinery, and the lead-farmer questionnaire.


carob_script <- function(path) {

"
From farms to carbon markets: crop area on household plots in Haryana and Madhya Pradesh

Plot crops and area from the 2023 carbon-farming household survey.
"

	uri <- "doi:10.71682/10549340"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "field_size",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.4
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | !nzchar(x) | x %in% c("NA", "-")] <- NA_character_
		as.numeric(x)
	}
	blank <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | !nzchar(x) | x == "NA"] <- NA_character_
		x
	}

	f <- ff[grepl("Carbon Farming HH", basename(ff))]
	p <- as.data.frame(carobiner::read.excel(f, sheet="Plot_inf_details", col_types="text"))
	m <- as.data.frame(carobiner::read.excel(f, sheet="Carbon_farming_Main_sheet", col_types="text"))
	gps <- data.frame(HH_ID=blank(m$HH_ID), latitude=num(m$Latitude), longitude=num(m$Longitude))
	gps <- gps[!is.na(gps$HH_ID) & !duplicated(gps$HH_ID), ]

	cmap <- c(
		Cotton="cotton", Soybean="soybean", Maize="maize", Wheat="wheat",
		Groundnut="groundnut", Sorghum="sorghum", Basmati_Rice="rice",
		Course_Rice="rice", Sugarcane="sugarcane", Chillies="chili pepper",
		Bengal_gram="chickpea", "Mustard/Rapeseed"="mustard", Pearl_millet="pearl millet"
	)
	stn <- c(MP="Madhya Pradesh", HR="Haryana")
	acre_ha <- 0.404686

	one <- function(cropcol, areacol, season) {
		raw <- blank(p[[cropcol]])
		crop <- unname(cmap[raw])
		ha <- num(p[[areacol]]) / num(p$Land_unit_convertion) * acre_ha
		ok <- !is.na(crop) & !is.na(ha) & ha > 0
		hh <- blank(p$HH_ID)
		st <- blank(p$State)
		adm <- unname(stn[st])
		adm[is.na(adm)] <- st[is.na(adm)]
		variety <- rep(NA_character_, length(raw))
		variety[raw == "Basmati_Rice"] <- "Basmati"
		ig <- match(hh, gps$HH_ID)
		data.frame(
			country = "India",
			adm1 = adm[ok],
			adm2 = blank(p$District)[ok],
			location = blank(p$Village)[ok],
			latitude = gps$latitude[ig][ok],
			longitude = gps$longitude[ig][ok],
			hhid = hh[ok],
			trial_id = paste(hh[ok], blank(p$Index)[ok], season),
			crop = crop[ok],
			variety = variety[ok],
			field_size = ha[ok],
			season_name_ = season,
			on_farm = TRUE,
			is_survey = TRUE,
			geo_from_source = TRUE
		)
	}

	d <- rbind(
		one("Plot_crop_kh", "Plot_area_kh", "kharif"),
		one("Plot_crop_rb", "Plot_area_rb", "rabi")
	)
	d <- d[!is.na(d$hhid), ]
	carobiner::write_files(path, meta, d)
}
