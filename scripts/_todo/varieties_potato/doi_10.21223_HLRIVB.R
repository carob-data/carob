# R script for "carob"
# license: GPL (>=3)

## NOTES
# School monitoring of native potato varieties in farmers' fields at Patacancha, Cusco, Peru. Coordinates are in the file. The country value Perú is stored as Peru.
# Observation dates are calendar dates. There is no yield.

## ISSUES
# No yield or management. The group still expects a yield column.
# Some rows are identical in the source file.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. duplicate records detected.

## NOT PROCESSED
# numeration, variety_id, user_id, location_id: internal ids. The second latitude and longitude copy the first.
# Nota: contest label, not a measurement.


carob_script <- function(path) {

"
Dataset for: Monitoring of Native potato varieties in Patacancha - Cusco

This dataset contains information from the monitoring of native potatoes registered by students from the Patacancha school, in Cusco -Peru. Nine students participated, recording around 338 observations with 100 unique varieties.
"

	uri <- "doi:10.21223/HLRIVB"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "01_data_monitoreo_Patacancha_Cusco.xlsx"]
	r <- carobiner::read.excel(f)
	latn <- grep("^latitude", names(r))[1]
	lonn <- grep("^longitude", names(r))[1]
	loc <- trimws(as.character(r$Locality))
	loc[!nzchar(loc)] <- NA_character_
	loc[is.na(loc)] <- "Patacancha"
	when <- as.character(as.Date(substr(as.character(r$observation_date), 1, 10)))
	alt <- trimws(as.character(r$cu_variety_name))
	alt[!nzchar(alt)] <- NA_character_

	d <- data.frame(
		variety = trimws(as.character(r$variety_name)),
		variety_alt = alt,
		hhid = as.character(r$farmer_id),
		country = "Peru",
		adm1 = trimws(as.character(r$adm1_region)),
		adm2 = trimws(as.character(r$adm2_province)),
		adm3 = trimws(as.character(r$adm3_district)),
		location = loc,
		latitude = as.numeric(r[[latn]]),
		longitude = as.numeric(r[[lonn]]),
		date = when,
		trial_id = paste("Patacancha", r$farmer_id),
		crop = "potato",
		geo_from_source = TRUE,
		on_farm = TRUE,
		is_survey = TRUE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
