# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot rows are the Fieldbook of PTYL200507_MUG.xls. TTYNA and MTYNA are t/ha in 03_Data_dictionary.xlsx. Stored as kg/ha.
# Coordinates, elevation, dates, design, and planting density are the Minimal and Installation sheets.
# Daily weather is Weather_daily (temperature only).

## ISSUES
# The abstract says "Completely Randomized Block Design". The Installation sheet says "Randomized Complete Block Design (RCBD)". The sheet text is used.
# Crop_management dates are all 2006-09-01. Minimal begin and end are 2005-07-21 and 2005-10-21. Minimal dates are used.
# Weather_daily includes 2006 as well as 2005. Rows are kept as recorded.
# No fertilizer or irrigation. The site is not labeled on-farm or station.
# Longitude is published as 29.3 (one decimal).
# write_files: ; yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer.

## NOT PROCESSED
# 01_data_yield_Mugurero_2005.xlsx: flat export of the same Fieldbook.
# Fieldbook per-plot counts and weights (NTP, NPE, NPH, PPH, tuber counts, TTWP, MTWP, TTYA, ATW, ATMW). Plot area is not in the file.
# AVDM: not defined in the dictionary sheet.
# Soil_analysis, Hobo_data, Weather_data, Weather_Original, Weather_monthly, Summary by clone, Charts, Ranks by Clone, TTYA, MTWP, MTYA summary sheets.
# Weather_daily n_sample and TSD.

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones for Performance in Burundi, 2005

An evaluation of 24 potato clones was carried out in the cities of Mugerero , Burundi, in 2005. The study utilized a Completely Randomized Block Design to assess the clones' performance under local conditions.
"

	uri <- "doi:10.21223/57YIIP"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Randomized complete block design",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "PTYL200507_MUG.xls"]
	info <- carobiner::read.excel(f, sheet="Minimal")
	kv <- stats::setNames(as.character(info$Value), trimws(as.character(info$Factor)))
	inst <- carobiner::read.excel(f, sheet="Installation")
	ik <- stats::setNames(as.character(inst$Value), trimws(as.character(inst$Factor)))
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	ml <- carobiner::read.excel(f, sheet="Material List")
	name <- trimws(as.character(ml[["Clone or variety name"]]))
	code <- trimws(as.character(ml[["Institutional number"]]))
	name[!nzchar(name)] <- NA_character_
	m <- match(trimws(as.character(r$INSTN)), code)
	variety <- trimws(as.character(r$INSTN))
	ok <- !is.na(m) & !is.na(name[m])
	variety[ok] <- name[m[ok]]

	d <- data.frame(
		crop = "potato",
		plot_id = as.character(r$PLOT),
		rep = as.integer(r$REP),
		variety_code = trimws(as.character(r$INSTN)),
		variety = variety,
		treatment = variety,
		yield = as.numeric(r$TTYNA) * 1000,
		yield_marketable = as.numeric(r$MTYNA) * 1000,
		yield_part = "tubers",
		yield_isfresh = TRUE,
		country = kv[["Country"]],
		adm1 = kv[["Admin1"]],
		adm2 = kv[["Admin2"]],
		location = kv[["Locality"]],
		elevation = as.numeric(kv[["Elevation"]]),
		latitude = as.numeric(kv[["Latitude"]]),
		longitude = as.numeric(kv[["Longitude"]]),
		planting_date = as.character(as.Date(kv[["Begin date"]])),
		harvest_date = as.character(as.Date(kv[["End date"]])),
		plant_density = as.numeric(ik[["Planting density (plants/Ha)"]]),
		trial_id = paste(kv[["Locality"]], "2005"),
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA,
		geo_from_source = TRUE,
		geo_uncertainty = 12000,
		geo_source = "HIDAP Minimal sheet"
	)

	w <- carobiner::read.excel(f, sheet="Weather_daily")
	wth <- data.frame(
		country = as.character(w$Country),
		adm1 = as.character(w$Department),
		location = as.character(w$Locality),
		date = sprintf("%04d-%02d-%02d", as.integer(w$Year), as.integer(w$Month), as.integer(w$Day)),
		tmin = as.numeric(w$TMIN),
		tmax = as.numeric(w$TMAX),
		temp = as.numeric(w$TMEAN),
		latitude = d$latitude[1],
		longitude = d$longitude[1],
		geo_from_source = TRUE
	)
	wth <- unique(wth[!is.na(wth$date), ])

	carobiner::write_files(path, meta, d, wth=wth)
}
