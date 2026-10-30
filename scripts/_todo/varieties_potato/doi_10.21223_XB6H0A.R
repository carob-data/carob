# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIP fieldbook PTYL201007, Tashkent (Kok Saroy), 2010. Installation sheet: RCBD, 3 reps, plot 2.7 m2, 0.30 m between plants, 0.90 m between rows.
# Dictionary: TTWP and MTWP are tuber weights per plot. TTYA of 24.13 matches TTWP 6.52 kg * 10000 / 2.7 / 1000, so TTYA and MTYA are tonnes/ha. Yield is TTYA * 1000.
# Planting and harvest dates are Minimal Begin_date and End_date (2010-07-01 and 2010-11-05). The Crop_management sheet repeats 2010-07-01 and has no doses.
# Coordinates, elevation, and admin names are the Minimal sheet. Weather_data is a daily series for the same site (hour is empty).

## ISSUES
# The dictionary does not print the TTWP unit. Kilograms per plot is inferred from the TTYA check above.
# Installation says 10 plants per plot (37037/ha). Fieldbook NTP is often 30. plant_density uses plants emerged (NPE) and the 2.7 m2 plot.
# Admin names are as recorded (Tashkent Oblast). Confirm against GADM.
# No fertilizer rates and no irrigation flag in the fieldbook.
# write_files: missing variables: carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer.

## NOT PROCESSED
# 01_Fieldbook and 02_Material_list text files: the same records as the xls Fieldbook and Material List sheets.
# Fieldbook NTP, PPE, NPH, PPH, MTWCI, MTWCII, NoMTWP, TTWPL, TTWP, TTYNA, MTWPL, MTYNA. Stand counts and per-plant or unadjusted weights. Yield uses TTYA. Emerged plant count is in plant_density.
# Crop_management: template rows with empty dose and product.
# Soil_analysis: header only, no measured values.
# Hobo_data: empty.
# Weather PAR columns (µE, not kJ/m2), temperature amplitude, barometric pressure (mm). Gust speed has no unit.
# Summary by clone, Ranks by Clone, Charts, Format checks, Fieldbook_backup, Var List.

## NEW VARIABLES
# None.


carob_script <- function(path) {

"
Dataset for: Evaluation of Lowland Tropic Virus-Resistant (LTVR) Potato Varieties in Uzbekistan (2010)

In this trial, 16 potato clones were assessed using a randomized complete block design (RCBD) with three repetitions during 2010 in Tashkent, Uzbekistan. These clones belong to the Lowland Tropic Virus Resistant Population (LTVR).
"

	uri <- "doi:10.21223/XB6H0A"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "PTYL201007_TASHKENT-LTVR.xls"]
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	ml <- carobiner::read.excel(f, sheet="Material List")

	plot_m2 <- 2.7
	d <- data.frame(
		plot_id = as.character(r$PLOT),
		rep = as.integer(r$REP),
		variety = trimws(as.character(r$INSTN)),
		treatment = trimws(as.character(r$INSTN)),
		yield = as.numeric(r$TTYA) * 1000,
		yield_marketable = as.numeric(r$MTYA) * 1000,
		plant_density = as.numeric(r$NPE) * 10000 / plot_m2,
		stringsAsFactors = FALSE
	)

	name <- trimws(as.character(ml[["Clone or variety name"]]))
	name[!nzchar(name)] <- NA_character_
	fem <- trimws(as.character(ml[["Female Institutional number"]]))
	mal <- trimws(as.character(ml[["Male Institutional number"]]))
	ped <- paste(fem, mal, sep=" x ")
	ped[is.na(fem) | is.na(mal) | !nzchar(fem) | !nzchar(mal)] <- NA_character_
	alias <- data.frame(
		variety = trimws(as.character(ml[["Institutional number"]])),
		variety_alt = name,
		variety_pedigree = ped,
		stringsAsFactors = FALSE
	)
	alias <- alias[!is.na(alias$variety) & nzchar(alias$variety), ]
	alias <- alias[!duplicated(alias$variety), ]
	d <- merge(d, alias, by="variety", all.x=TRUE)

	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	d$country <- "Uzbekistan"
	d$adm1 <- "Tashkent Oblast"
	d$adm2 <- "Tashkent"
	d$location <- "Kok Saroy"
	d$elevation <- 432
	d$latitude <- 41.36577308
	d$longitude <- 69.14262235
	d$geo_from_source <- TRUE
	d$planting_date <- "2010-07-01"
	d$harvest_date <- "2010-11-05"
	d$row_spacing <- 90
	d$plant_spacing <- 30
	d$trial_id <- "PTYL201007_TASHKENT_LTVR"
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	w0 <- carobiner::read.excel(f, sheet="Weather_data")
	names(w0) <- trimws(names(w0))
	wth <- data.frame(
		station_id = "TASHKENT",
		station_name = "Kok Saroy",
		date = as.character(as.Date(w0[["Date of weather observation"]])),
		prec = as.numeric(w0[["Rainfall (mm)"]]),
		temp = as.numeric(w0[["Average temperature (°C)"]]),
		tmin = as.numeric(w0[["Minimum temperature (°C)"]]),
		tmax = as.numeric(w0[["Maximum temperature (°C)"]]),
		rhum = as.numeric(w0[["Relative humidity (%)"]]),
		dewp = as.numeric(w0[["Dew point (°C)"]]),
		wspd = as.numeric(w0[["Wind speed (m/s)"]]),
		wdir = as.numeric(w0[["Wind direction"]]),
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
