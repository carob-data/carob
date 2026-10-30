# R script for "carob"
# license: GPL (>=3)

## NOTES
# Heat-tolerance yield trial at Ladol, Gujarat, India. The Minimal sheet gives the site, coordinates, elevation, and RCBD.
# Crop management: planted 2010-10-15, vines killed 2011-01-12, harvested 2011-01-27.
# Installation: plot 2.4 m2, 0.20 m between plants, 0.60 m between rows.
# TTWP is kg/plot. 10.11 kg on 2.4 m2 is 42.1 t/ha, which matches TTYNA. TTYA is the adjusted total tuber yield in t/ha, so yield is TTYA * 1000.
# MTYA is marketable tuber yield on the same scale.

## ISSUES
# No fertilizer rate and no irrigation statement in the crop-management sheet.
# No yield moisture.
# Weather, soil, and score sheets are not in the plot table.
# write_files: missing variables: carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer.

## NOT PROCESSED
# Soil_analysis, Hobo_data, Weather_data: site weather and soil, not plot yields.
# Summary by clone, Charts, Ranks by Clone, Fieldbook_backup, 2010-11, serial wise clone name: summaries or working sheets.
# Var List and 03_Data_dictionary.txt: column definitions. The dictionary does not state units.
# Crop_management rows that only name the measurements already in the fieldbook.
# NTP, NPH, PPH, NoMTWP, TTWP, TTWPL, TTYNA, MTWP, MTYNA: used to check units or left as plot counts. NPH is used for plant_density.


carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in India (2010)

Trials featuring heat-tolerant potato varieties were conducted in South West Central Asia (SWCA),  in Ladol, India, during the year 2010. In this study, 16 potato clones were evaluated using a randomized complete block design (RCBD) with three repetitions
"

	uri <- "doi:10.21223/0CSSI8"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "PTYL201010_LADOL.xls"]
	r <- carobiner::read.excel(f, sheet="Fieldbook", na="NA")
	names(r) <- trimws(names(r))
	ml <- carobiner::read.excel(f, sheet="Material List", na="NA")
	names(ml) <- trimws(names(ml))

	instn <- trimws(as.character(r$INSTN))
	vname <- trimws(as.character(ml[["Clone or variety name"]]))
	vname[!nzchar(vname)] <- NA_character_
	key <- trimws(as.character(ml[["Institutional number"]]))
	nm <- vname[match(instn, key)]
	variety <- ifelse(is.na(nm), instn, nm)
	female <- trimws(as.character(ml[["Female code"]]))
	female[!nzchar(female)] <- NA_character_
	male <- trimws(as.character(ml[["Male code"]]))
	male[!nzchar(male)] <- NA_character_

	d <- data.frame(
		trial_id = "Ladol_2010",
		crop = "potato",
		variety = variety,
		variety_code = instn,
		treatment = variety,
		female_parent = female[match(instn, key)],
		male_parent = male[match(instn, key)],
		rep = as.integer(r$REP),
		plot_id = as.character(r$PLOT),
		yield = as.numeric(r$TTYA) * 1000,
		yield_marketable = as.numeric(r$MTYA) * 1000,
		yield_part = "tubers",
		plot_area = 2.4,
		plant_spacing = 20,
		row_spacing = 60,
		plant_density = as.numeric(r$NPH) / 2.4 * 10000,
		country = "India",
		adm1 = "Gujarat",
		adm2 = "Mahesana",
		location = "Ladol",
		elevation = 118,
		latitude = 23.6167,
		longitude = 72.733,
		planting_date = "2010-10-15",
		harvest_date = "2011-01-27",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
