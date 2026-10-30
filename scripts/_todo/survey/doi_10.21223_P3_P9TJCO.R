# R script for "carob"
# license: GPL (>=3)

## NOTES
# Crop rows are C02bs_m015_1_Crops_Details_Table_nopii.dta.
# Country Ghana is in the dataset description.
# Plot size is crop_plot_size_r with crop_unit_r. Hectares, acres (times 0.404685642), and square meters (divided by 10000) are stored as field_size. Other unit text is not converted.
# Variety is this_variety_name. The file does not name the crop species in these columns, so crop is not filled from the variety alone.

## ISSUES
# No coordinates and no yield in this crop-detail table.
# get_data warns that GROWING Baseline Data - nopii.rar could not be extracted (an archive entry is marked as a directory). The dta tables were already on disk.
# write_files: missing variables: carob_contributor; geo_from_source, latitude, longitude.
# write_files: unknown variables: plot_size_recorded_, plot_unit_.

## NOT PROCESSED
# The other baseline modules (income, assets, revenue, and the pro-WEAI file).

## NEW VARIABLES
# plot_size_recorded_: crop_plot_size_r as recorded
# plot_unit_: crop_unit_r as recorded

carob_script <- function(path) {

"
Dataset for: GROWING Baseline Survey (2022/2023)

This dataset presents baseline data collected by GROWING, which is being implemented in Northern Ghana by the International Potato Centre (CIP) and CARE International. The baseline data was collected in November 2022 and February 2023. The primary purpose of the dataset is to establish pre-intervention reference conditions for the project's key intervention areas. The project was designed to implement integrated interventions, including agriculture, nutrition, women's empowerment, marketing, financial inclusion, agro-processing, and small livestock rearing. Therefore, the baseline questionnaire was developed with a focus on the project's key performance indicators (KPIs) drawn from this implementation sector. The main purpose of the baseline data collection was to establish pre-intervention conditions for project participants, serve as a benchmark for annual performance monitoring and a point of comparison for longitudinal impact assessment.
"

	uri <- "doi:10.21223/P3/P9TJCO"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "field_size",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.4
	)

	f <- ff[grepl("Crops_Details_Table", basename(ff))]
	r <- carobiner::read.dta(f)
	qty <- as.numeric(r$crop_plot_size_r)
	unit <- tolower(trimws(as.character(r$crop_unit_r)))
	ha <- rep(NA_real_, length(qty))
	ha[unit %in% c("hectare", "hectares", "ha")] <- qty[unit %in% c("hectare", "hectares", "ha")]
	ha[unit %in% c("acre", "acres")] <- qty[unit %in% c("acre", "acres")] * 0.404685642
	ha[unit %in% c("square meters", "square metres", "m2")] <- qty[unit %in% c("square meters", "square metres", "m2")] / 10000

	d <- data.frame(
		hhid = trimws(as.character(r$hhid)),
		country = "Ghana",
		adm1 = trimws(as.character(r$region)),
		adm2 = trimws(as.character(r$district)),
		adm3 = trimws(as.character(r$sub_district)),
		location = trimws(as.character(r$community_name)),
		variety = trimws(as.character(r$this_variety_name)),
		treatment = trimws(as.character(r$treatment_status)),
		plot_size_recorded_ = qty,
		plot_unit_ = unit,
		field_size = ha,
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA,
		trial_id = "GROWING_baseline",
		stringsAsFactors = FALSE
	)
	carobiner::write_files(path, meta, d)
}
