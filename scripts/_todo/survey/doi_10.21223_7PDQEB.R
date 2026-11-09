# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are sheet 5-Productivity. Country Peru and department La Libertad are in the title.
# previous_year_yield is kept as recorded. The sheet does not give the unit, so it is not converted to kg/ha.
# crop_name papa or potato is stored as potato. Other names are kept.

## ISSUES
# No coordinates.
# The yield unit is not stated.
# write_files: ; geo_from_source, latitude, longitude.
# write_files: unknown variable: previous_yield_.

## NOT PROCESSED
# Farmer, sowing, visit, harvest, manual-activity, and soil workbooks in this dataset. The productivity sheet is the yield record.
# factor_difference and the environmental and management factor columns: coded reasons, not a rate.
# 01_Data-Agronomic-La Libertad-2021-2022-Farmers-Crop-Site.xlsx: not read. sheet 1-Farmers-crops: ID, Site, level_education, province, district, locality, experience, organization, plot_area, latitude, longitude, elevation, .... Observation records are missing from d until this file is standardized.
# 02_Data-Agronomic-La Libertad-2021-2022-Sowing.xlsx: not read. sheet 2-Planting: ID, Site, activity, crop_before_planting, planting_area, how_register_cost, determine_planting, how_planting, planting_date, planting_in, arrangement, slope_was_considered, .... Observation records are missing from d until this file is standardized.
# 03_Data-Agronomic-La Libertad-2021-2022-Visit.xlsx: not read. sheet 3-Visit: ID, Site, visit_date, crop_name, crop_stage, current_problem_crop, establishment_problems, percentage_establishment_problems, caused_by, part_affect_plagues, severity, ipm_type, .... Observation records are missing from d until this file is standardized.
# 04.1_Data-Agronomic-La Libertad-2021-2022-Crop.xlsx: not read. sheet 4.1-cultivo: crop_name1, communal_work, obtained_product_harvest, harvest_start_date, harvest_end_date, how_determine_harvest, parent_key1. Observation records are missing from d until this file is standardized.
# 04.2_Data-Agronomic-La Libertad-2021-2022-Manual activity.xlsx: not read. sheet 4.2-Manual activity: crop_name2, perform_manual_harvest, date_manual_activity, manual_activity_performed, uses_implements_manual, number_implements_used_manual, cosecha_manual-actividad1-mas_de_un_implemento-implemento_utilizado, number_people_harvest, labor_cost_day, jornal_used, duration_jornal_day, cost_type, .... Observation records are missing from d until this file is standardized.
# 04_Data-Agronomic-La Libertad-2021-2022-Harvest.xlsx: not read. sheet 4-Harvest: ID, Site, meta-instance, plot_name, surfice, crop1, harvest_method, percentage_stubble_plot, description_harvest, number_manual_harvest, manual1. Observation records are missing from d until this file is standardized.
# 05.1_Data-Agronomic-La Libertad-2021-2022-Products.xlsx: not read. sheet 5.1-producto: product_obtained, estimated_yield, real_yield, percentage_for_sale, percentage_to_self-consumption, percentage_to_seed, sold_product, sale_price, parent_key3. Observation records are missing from d until this file is standardized.
# 06_Data-Agronomic-La Libertad-2021-2022-Soil analysis.xlsx: not read. sheet 6-Soil anlysis: N, Site, pH, EC, CaCo3, OM, P, K, Sand, Silt, Clay, TC, .... Observation records are missing from d until this file is standardized.
# 07_Data_dictionary.xlsx: documentation or a codebook, not an observation table.

## NEW VARIABLES
# previous_yield_: previous year yield as recorded

carob_script <- function(path) {

"
Dataset for: Agronomic log of small potato farmers during the 2021-2022 campaign in the districts of Chugay, Pataz, Pías y Tayabamba in the department of La Libertad - Peru.

This dataset corresponds to the records of small potato farmers carried out during the 2021-2022 campaign in the districts of Chugay, Pataz, Pías, and Tayabamba, in the department of La Libertad - Peru. Corresponding to the different activities in their agricultural plots, such as field preparation, planting, agronomic management, pest and disease management, and harvest. This dataset has been developed considering the e-Agrology platform and using the modules: Farmers-crops-site (n=759), sowing (n=684), visit (n=794), harvest (n=592), productivity (n=592), and soil analysis (n=204).
"

	uri <- "doi:10.21223/7PDQEB"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "previous_yield_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "05_Data-Agronomic-La Libertad-2021-2022-Productivity.xlsx"]
	r <- carobiner::read.excel(f, sheet="5-Productivity")
	crop <- tolower(trimws(as.character(r$crop_name)))
	crop[crop %in% c("papa", "potato", "patata")] <- "potato"

	d <- data.frame(
		hhid = trimws(as.character(r$ID)),
		location = tools::toTitleCase(tolower(trimws(as.character(r$Site)))),
		plot_id = trimws(as.character(r$plot_name)),
		crop = crop,
		previous_yield_ = {
			y <- trimws(as.character(r$previous_year_yield))
			y[!nzchar(y) | y %in% c("NA", ".")] <- NA_character_
			as.numeric(y)
		},
		country = "Peru",
		adm1 = "La Libertad",
		trial_id = "LaLibertad_2021_2022",
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA
	)

	carobiner::write_files(path, meta, d)
}
