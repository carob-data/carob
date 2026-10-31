# R script for "carob"
# license: GPL (>=3)

## NOTES
# Heat-tolerance yield trial at Kok Saroy, Tashkent, 2011. Minimal sheet: RCBD, elevation 440 m, latitude 41.36273, longitude 69.14141.
# Crop management: planted 2011-07-12, harvested 2011-11-12. Plant height was measured on 2011-10-20. The unit is not printed. Values are about 50-80 and are stored as plant_height.
# Installation: plot 5.25 m2, 0.25 m between plants, 0.70 m between rows.
# TTWP is kg/plot. On 5.25 m2 that matches TTYNA in t/ha. Yield is TTYA * 1000. MTYA is marketable yield on the same scale.
# Project name is on the Minimal sheet.

## ISSUES
# No fertilizer rate and no irrigation statement.
# Plant height unit is not printed.
# No yield moisture.
# Flowering degree, plant vigor, canopy, and senescence are scores without a stated scale.
# write_files: , yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer.

## NOT PROCESSED
# Soil_analysis, Weather_data: not plot yields.
# Summary by clone, Charts, Ranks by Clone, Fieldbook_backup: summaries.
# PV, PC, Flower, SE, NTP, NPE, tuber-count and per-plot weight columns other than those used for the yield check. NPH is used for plant_density.
# 03_Data_dictionary.txt does not give units.


carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in Uzbekistan (2011)

Trials featuring heat-tolerant potato varieties were conducted in South West Central Asia (SWCA), specifically in Tashkent, Uzbekistan, during the year 2011. In this study, 32 potato clones were evaluated using a randomized complete block design (RCBD) with three repetitions
"

	uri <- "doi:10.21223/JHBLVQ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = "Enhanced food and income security in SWCA through potato varieties with improved tolerance to abiotic stress",
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable;plant_height",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "PTYL201107_TASHKENT_HEAT.xls"]
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
		trial_id = "KokSaroy_2011",
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
		plant_height = as.numeric(r$PH),
		yield_part = "tubers",
		plot_area = 5.25,
		plant_spacing = 25,
		row_spacing = 70,
		plant_density = as.numeric(r$NPH) / 5.25 * 10000,
		country = "Uzbekistan",
		adm1 = "Tashkent",
		location = "Kok Saroy",
		elevation = 440,
		latitude = 41.36273,
		longitude = 69.14141,
		planting_date = "2011-07-12",
		harvest_date = "2011-11-12",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
