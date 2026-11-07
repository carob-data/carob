# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot rows are the Fieldbook of PTYL201007_TASHKENT-BMZ.xls.
# TTYNA and MTYNA follow the CIP fieldbook scale t/ha and are stored as kg/ha.
# Site, coordinates, elevation, begin and end dates, design, plot size, and spacing are the Minimal and Installation sheets.
# Distances are published in meters and stored as cm.
# Clone names are blank. variety is the institutional number.
# Daily weather is Weather_data. Solar radiation there is PAR (µE), not irradiance in W/m2.

## ISSUES
# Crop_management dates are all 2010-07-01, including harvest. Minimal end date 2010-11-05 is used as harvest_date. Planting_date is the Minimal begin date, which matches the Crop_management planting row.
# No fertilizer or irrigation recorded.
# The site is not labeled on-farm or station.
# write_files: ; yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer.

## NOT PROCESSED
# 01_data_yield_Tashkent_2010.xlsx and the fieldbook txt: same fieldbook export.
# Fieldbook NTP, NPE, PPE, NPH, PPH, MTWCI, MTWCII, NoMTWP, TTWP, TTWPL, TTYA, MTWP, MTWPL: per-plot counts or the stand-adjusted yield.
# Weather_data solar radiation (PAR µE), temperature amplitude, barometric pressure, dew point, gust speed, and wind direction. Wind speed was empty in the rows read with the other weather columns; it is kept only when present.
# Soil_analysis, Hobo_data, Summary by clone, Charts, Ranks by Clone, Fieldbook_backup.
# 01_Fieldbook_PTYL201007_TASHKENT_BMZ.txt: not read. columns: PLOT, REP, INSTN, NTP, NPE, PPE, NPH, PPH, MTWCI, MTWCII, NoMTWP, TTWP, .... Observation records are missing from d until this file is standardized.
# 02_Material_list_PTYL201007_TASHKENT_BMZ..txt: not read. columns: Numeration, Control, Institutional number, Clone or variety name, Code of clone, Family Institutional number, Female Institutional number, Female code, Male Institutional number, Male code. Observation records are missing from d until this file is standardized.
# 03_Data_dictionary.txt: documentation or a codebook, not an observation table.

carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in Uzbekistan (2010)

Trials featuring heat-tolerant potato varieties were conducted in South West Central Asia (SWCA), specifically in Tashkent, Uzbekistan, during the year 2010. In this study, 16 potato clones were evaluated using a randomized complete block design (RCBD) with three repetitions
"

	uri <- "doi:10.21223/BF3BJW"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = "Enhanced food and income security in SWCA through potato varieties with improved tolerance to abiotic stress. Code: 07.7860.5-001.00",
		design = "Randomized complete block design",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "PTYL201007_TASHKENT-BMZ.xls"]
	r1 <- carobiner::read.excel(f, sheet="Minimal")
	kv <- stats::setNames(as.character(r1$Value), trimws(as.character(r1$Factor)))
	r2 <- carobiner::read.excel(f, sheet="Installation")
	ik <- stats::setNames(as.character(r2$Value), trimws(as.character(r2$Factor)))
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	r3 <- carobiner::read.excel(f, sheet="Material List")
	name <- trimws(as.character(r3[["Clone or variety name"]]))
	code <- trimws(as.character(r3[["Institutional number"]]))
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
		adm3 = kv[["Admin3"]],
		location = kv[["Locality"]],
		elevation = as.numeric(kv[["Elevation"]]),
		latitude = as.numeric(kv[["Latitude"]]),
		longitude = as.numeric(kv[["Longitude"]]),
		planting_date = as.character(as.Date(kv[["Begin date"]])),
		harvest_date = as.character(as.Date(kv[["End date"]])),
		plant_density = as.numeric(ik[["Planting density (plants/Ha)"]]),
		plot_area = as.numeric(ik[["Plot size (m2)"]]),
		plant_spacing = as.numeric(ik[["Distance between plants (m)"]]) * 100,
		row_spacing = as.numeric(ik[["Distance between rows (m)"]]) * 100,
		trial_id = "Tashkent_2010",
		stress = "heat",
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA,
		geo_from_source = TRUE,
		geo_source = "HIDAP Minimal sheet"
	)

	r4 <- carobiner::read.excel(f, sheet="Weather_data")
	wth <- data.frame(
		country = d$country[1],
		location = d$location[1],
		latitude = d$latitude[1],
		longitude = d$longitude[1],
		geo_from_source = TRUE,
		date = as.character(as.Date(r4[["Date of weather observation"]])),
		prec = as.numeric(r4[["Rainfall (mm)"]]),
		temp = as.numeric(r4[["Average temperature (°C)"]]),
		tmin = as.numeric(r4[["Minimum temperature (°C)"]]),
		tmax = as.numeric(r4[["Maximum temperature (°C)"]]),
		rhum = as.numeric(r4[["Relative humidity (%)"]]),
		wspd = as.numeric(r4[["Wind speed (m/s)"]])
	)
	wth <- wth[!is.na(wth$date), ]
	if (all(is.na(wth$wspd))) wth$wspd <- NULL

	carobiner::write_files(path, meta, d, wth=wth)
}
