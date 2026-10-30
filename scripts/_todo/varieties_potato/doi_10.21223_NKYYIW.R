# R script for "carob"
# license: GPL (>=3)

## NOTES
# Heat-tolerance yield trial at Kok Saroy, Tashkent, 2009. Minimal sheet: RCBD, elevation 440 m, latitude 41.36412621, longitude 69.14061606.
# Crop management: planted 2009-07-11, harvested 2009-10-20. Plant height was measured on 2009-10-10. The unit is not printed. Values are about 80 and are stored as plant_height.
# Installation: plot 5.25 m2, 0.25 m between plants, 0.70 m between rows.
# TTWP is kg/plot and matches TTYNA in t/ha. Yield is TTYA * 1000.

## ISSUES
# No fertilizer rate and no irrigation statement.
# Plant height unit is not printed.
# No yield moisture.
# Number of stems and plant canopy have no stated unit or scale.
# write_files: missing variables: carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer.

## NOT PROCESSED
# Soil_analysis, Weather_data.
# Summary by clone, Charts, Ranks by Clone.
# NS, PC, and the per-plot tuber counts and weights other than the yield check. NPH is used for plant_density.
# 03_Data_dictionary.txt does not give units.


carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in Uzbekistan (2009)

Trials featuring heat-tolerant potato varieties were conducted in South West Central Asia (SWCA), specifically in Tashkent, Uzbekistan, during the year 2009. In this study, 16 potato clones were evaluated using a randomized complete block design (RCBD) with three repetitions
"

	uri <- "doi:10.21223/NKYYIW"
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
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "PTYL200907_TASHKENT_HEAT.xls"]
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
		trial_id = "KokSaroy_2009",
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
		latitude = 41.36412621,
		longitude = 69.14061606,
		planting_date = "2009-07-11",
		harvest_date = "2009-10-20",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
