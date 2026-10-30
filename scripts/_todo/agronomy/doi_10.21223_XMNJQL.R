# R script for "carob"
# license: GPL (>=3)

## NOTES
# Country code RW is Rwanda. Coordinates are in the file.
# tuberY matches tubersFW / (plotLength * plotWidth) * 10 exactly (correlation 1). plotLength and plotWidth are meters and tubersFW is kilograms, so tuberY is t/ha. Yield is tuberY * 1000.
# The AGROVOC keyword on the dataset is Potatoes. The columns are tuber weights.
# Treatment codes are NPK11, NPK4_MOP2, NPK4_DAP2, NPK6, and NPK4_UREA2. The dictionary does not give kilograms of product or nutrient.
# KEY1 identifies the field (109 fields, several treatment plots each). The description says 260 validation trials and season 2021B onwards. The file has no date.

## ISSUES
# Fertilizer codes are not expanded to N, P, or K rates.
# Site1 and Site2 are labels, not place names. The coordinates are the plot GPS points.
# No planting or harvest date, irrigation, or variety name.
# Some tuberY values are above 150 t/ha. They match the fresh-weight and plot-area identity and are left as recorded.
# write_files: missing carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. out of bounds: yield (3038.194, 249131.944). high crop yield: potato 249131.9444.

## NOT PROCESSED
# 01_Scaling_AKILIMO_yield_data.csv: score_vigor, score_weeds, pest and disease counts, tuber size counts, and the subsample fresh-weight columns (tubersFWss and the PSID columns). tubersFW is the plot fresh weight already used for tuberY.
# ODK device and submission columns (uuid, deviceid, start, end, instanceID).
# 02_Dictionary_Scaling_AKILIMO_yield_data.csv: column definitions.


carob_script <- function(path) {

"
Dataset for: Validation trials to validate AKILIMO recommendations in Rwanda

A set of 260 validation trials was conducted from the season 2021B onwards to compare AKILIMO recommendations with the blanket fertilizer recommendations in Rwanda
"

	uri <- "doi:10.21223/XMNJQL"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "01_Scaling_AKILIMO_yield_data.csv"]
	r <- read.csv(f, check.names=FALSE, stringsAsFactors=FALSE)
	country <- trimws(r$country)
	if (any(country != "RW")) stop("unexpected country code")

	area_m2 <- as.numeric(r$plotLength) * as.numeric(r$plotWidth)
	d <- data.frame(
		crop = "potato",
		country = "Rwanda",
		location = trimws(r$site),
		latitude = as.numeric(r$Latitude),
		longitude = as.numeric(r$Longitude),
		geo_from_source = TRUE,
		treatment = trimws(r$treat),
		trial_id = trimws(as.character(r$KEY1)),
		plot_id = as.character(r$numeration),
		plot_area = area_m2,
		plot_length = as.numeric(r$plotLength),
		plot_width = as.numeric(r$plotWidth),
		plant_density = as.numeric(r$nrPlants) / area_m2 * 10000,
		tuber_density = as.numeric(r$tubersNr) / area_m2 * 10000,
		yield = as.numeric(r$tuberY) * 1000,
		yield_part = "tubers",
		on_farm = NA,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
