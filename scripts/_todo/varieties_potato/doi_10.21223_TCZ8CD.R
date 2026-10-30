# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two heat-tolerance yield trials in Tajikistan, 2011, both RCBD. Coordinates, elevation, and admin names are on each Minimal sheet.
# Fayzabad: planted 2011-06-03, harvested 2011-10-20. Muminabad: planted 2011-06-05, harvested 2011-10-08.
# Both installations: plot 7 m2, 0.25 m between plants, 0.70 m between rows.
# TTWP is kg/plot and matches TTYNA in t/ha. Yield is TTYA * 1000.
# The Fayzabad workbook lists Admin1 as Fayzabad. That is the locality name, stored here as location, with adm1 left as the workbook text.

## ISSUES
# No fertilizer rate and no irrigation statement.
# No yield moisture.
# Fayzabad as adm1 should be checked against the official region.
# write_files: missing variables: carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer.

## NOT PROCESSED
# Soil_analysis, Hobo_data, Weather_data.
# Summary by clone, Charts, Ranks by Clone, Fieldbook_backup.
# Per-plot tuber counts and weights other than the yield check. NPH is used for plant_density.
# 03_Data_dictionary.txt does not give units.


carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in Tajikistan (2011)

The evaluation of 10 potato clones was conducted in the Tajikistan region, specifically in the localities of Muminabad and Fayzabad, in 2011. The experiment followed a randomized complete block design (RCBD) with three repetitions, aiming to assess heat tolerance in these areas.
"

	uri <- "doi:10.21223/TCZ8CD"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = "Enhanced food and income security in SWCA through potato varieties with improved tolerance to abiotic stress",
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

	one <- function(f, trial, adm1, location, elevation, latitude, longitude, planting, harvest) {
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
		data.frame(
			trial_id = trial,
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
			plot_area = 7,
			plant_spacing = 25,
			row_spacing = 70,
			plant_density = as.numeric(r$NPH) / 7 * 10000,
			country = "Tajikistan",
			adm1 = adm1,
			location = location,
			elevation = elevation,
			latitude = latitude,
			longitude = longitude,
			planting_date = planting,
			harvest_date = harvest,
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = TRUE
		)
	}

	fy <- ff[basename(ff) == "PTYL201106_FAYZABAD_HEAT.xls"]
	mu <- ff[basename(ff) == "PTYL201106_MUMINABAD_HEAT.xls"]
	d1 <- one(fy, "Fayzabad_2011", "Fayzabad", "Fayzabad", 1294, 38.55706, 69.368261, "2011-06-03", "2011-10-20")
	d2 <- one(mu, "Muminabad_2011", "Khatlon", "Muminabad", 1211, 38.14893, 69.993258, "2011-06-05", "2011-10-08")
	d <- rbind(d1, d2)
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
