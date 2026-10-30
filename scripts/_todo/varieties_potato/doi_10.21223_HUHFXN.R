# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rapid appraisal of potato practices in Rwanda. The dictionary names part_1/q004 District, q005 Sector, q007 Cell, q008 Village, and q003 Date. Country is Rwanda. The crop is potato.
# Latitude, longitude, and altitude are the GPS fields on the same sheet. Altitude is meters.
# The file has 289 records. The abstract says 288 households.

## ISSUES
# No yield in kg/ha. Fertilizer columns are product names, not a rate in kg/ha of N, P, or K.
# No planting or harvest date. q003 is the interview date.
# missing variables: carob_contributor
# missing variables: yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variables: cell_

## NOT PROCESSED
# Variety yes/no columns and the rest of the appraisal form.
# 003 to 006 fertilizer repeats. The rate unit is not used here.
# 007 data dictionary and the xlsform. The labels used above come from the dictionary.
# GPS precision.


carob_script <- function(path) {

"
Dataset for: Rapid appraisal of potato production practices in Rwanda

Assessment of the current status of agronomy practices in potato cultivation in Rwanda through a farm survey with 288 households. Five districts in Rwanda were selected for the study based on the total area under potato cultivation: Nyabihu, Rubavu, Musanze, Burera and Gicumbi. Most of the sectors in those districts are located in two major agroecological zones: the Cone and High Volcanic Plains and the Buberuka Highlands.
"

	uri <- "doi:10.21223/HUHFXN"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "adm2",
		response_vars = "elevation",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "001_Rwanda_Potato_Production_Rapid.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	fecha <- r[["part_1/q003"]]
	if (inherits(fecha, "Date") || inherits(fecha, "POSIXt")) {
		fecha <- format(fecha, "%Y-%m-%d")
	} else {
		fecha <- trimws(as.character(fecha))
	}

	d <- data.frame(
		trial_id = "Rwanda_potato_appraisal",
		hhid = trimws(as.character(r[["_uuid"]])),
		country = "Rwanda",
		adm2 = trimws(as.character(r[["part_1/q004"]])),
		adm3 = trimws(as.character(r[["part_1/q005"]])),
		cell_ = trimws(as.character(r[["part_1/q007"]])),
		location = trimws(as.character(r[["part_1/q008"]])),
		date = fecha,
		# GPS columns on 001_Rwanda_Potato_Production_Rapid.xlsx
		latitude = as.numeric(r[["q010_group/q011_group/part_3/_gps_points_latitude"]]),
		longitude = as.numeric(r[["q010_group/q011_group/part_3/_gps_points_longitude"]]),
		elevation = as.numeric(r[["q010_group/q011_group/part_3/_gps_points_altitude"]]),
		geo_from_source = TRUE,
		crop = "potato",
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
