# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rapid appraisal of potato practices in Rwanda. The dictionary names part_1/q004 District, q005 Sector, q007 Cell, q008 Village, and q003 Date. Country is Rwanda. The crop is potato.
# Latitude, longitude, and altitude are the GPS fields on the same sheet. Altitude is meters.
# The file has 289 records. The abstract says 288 households.

## ISSUES
# No yield in kg/ha. Fertilizer columns are product names, not a rate in kg/ha of N, P, or K.
# No planting or harvest date. q003 is the interview date.
# missing variables: yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variables: cell_

## NOT PROCESSED
# Variety yes/no columns and the rest of the appraisal form.
# 003 to 006 fertilizer repeats. The rate unit is not used here.
# 007 data dictionary and the xlsform. The labels used above come from the dictionary.
# GPS precision.
# 003_fertilizers_of_Rwanda_q010_group_q011_group_part_3_s4.xlsx: not read. sheet q010_group_q011_group_part_3_s4: q010_group/q011_group/part_3/select_fertilizers/cur_selection2, q010_group/q011_group/part_3/select_fertilizers/cur_selection_lbl2, q010_group/q011_group/part_3/select_fertilizers/q103, q010_group/q011_group/part_3/select_fertilizers/q105, _index, _parent_table_name, _parent_index. Observation records are missing from d until this file is standardized.
# 004_fertilizer_of_Rwanda_q010_group_q011_group_part_3_s5.xlsx: not read. sheet q010_group_q011_group_part_3_s5: q010_group/q011_group/part_3/select_fertilizers6/cur_selection6, q010_group/q011_group/part_3/select_fertilizers6/cur_selection_lbl6, q010_group/q011_group/part_3/select_fertilizers6/q103d, q010_group/q011_group/part_3/select_fertilizers6/q105d, q010_group/q011_group/part_3/select_fertilizers6/q105d_oth, _index, _parent_table_name, _parent_index. Observation records are missing from d until this file is standardized.
# 005_fertilizer_of_Rwanda_q010_group_q011_group_part_3_s6.xlsx: not read. sheet q010_group_q011_group_part_3_s6: q010_group/q011_group/part_3/select_fertilizers7/cur_selection7, q010_group/q011_group/part_3/select_fertilizers7/cur_selection_lbl7, q010_group/q011_group/part_3/select_fertilizers7/q103b, q010_group/q011_group/part_3/select_fertilizers7/q105b, _index, _parent_table_name, _parent_index. Observation records are missing from d until this file is standardized.
# 006_fertilizer_of_Rwanda_q010_group_q011_group_part_3_s8.xlsx: not read. sheet q010_group_q011_group_part_3_s8: q010_group/q011_group/part_3/select_pesticides/cur_selection9, q010_group/q011_group/part_3/select_pesticides/cur_selection_lbl9, q010_group/q011_group/part_3/select_pesticides/q113, q010_group/q011_group/part_3/select_pesticides/q114, _index, _parent_table_name, _parent_index. Observation records are missing from d until this file is standardized.
# 007_Data_dictionary_of_Rwanda.xlsx: documentation or a codebook, not an observation table.
# Rwanda_Potato_Production_Rapid_Appraisal_V7.xlsx: not read. sheet survey (of 3): type, name, label::English, choice_filter, hint, constraint, constraint_message, calculation, required, appearance, default, relevant, .... Observation records are missing from d until this file is standardized.


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
