# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are seed-multiplier records in 16940_Decentralised_seed_multiplier_gross_margin_data.xlsx, not a designed variety trial.
# total_yield_t_ha is t/ha. Stored as kg/ha. up_ha_units_of_production is hectares.
# The column named country contains county names (Nandi, Uasin Gishu, and others). Country Kenya is the dataset geographic coverage. The column is stored as adm1.
# sex M/F is stored as male/female.
# The season column mixes codes (LR2016, SR2017) and date ranges. Those strings are not vocabulary season names, so they are kept as season_code_.
# Crop is potato from the dataset description.

## ISSUES
# No coordinates. Counties need georeferencing.
# No variety, fertilizer, or a calendar planting or harvest date. Season codes are not expanded into a day.
# Some yield cells are empty.
# write_files: ; yield_moisture, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variable: season_code_ (season?); NA detected: yield.

## NEW VARIABLES
# season_code_: season text as recorded (LR/SR codes or a date range)

## NOT PROCESSED
# Price, sales, input-cost, and gross-margin columns (ware and seed, KES and USD). They are not a single crop price.
# Seed quantity columns and percent_production_that_is_seed. ward is mapped to location.

carob_script <- function(path) {

"
Dataset for: Decentralised seed multiplier gross margin data

This study was conducted during the years of 2016, 2017 and 2018 in the countries of  Nandi, Uasin Gishu, Elgeyo Marakwet and Meru.   Here economic aspects of potato production were evaluated as the value of production by yield.
"

	uri <- "doi:10.21223/ZTO9TF"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "16940_Decentralised_seed_multiplier_gross_margin_data.xlsx"]
	r <- carobiner::read.excel(f)
	sex <- trimws(as.character(r$gender))
	sex[sex == "M"] <- "male"
	sex[sex == "F"] <- "female"

	d <- data.frame(
		hhid = as.character(r$ID_name),
		season_code_ = trimws(as.character(r$season)),
		sex = sex,
		country = "Kenya",
		adm1 = trimws(as.character(r$country)),
		location = trimws(as.character(r$ward)),
		field_size = as.numeric(r$up_ha_units_of_production),
		yield = as.numeric(r$total_yield_t_ha) * 1000,
		crop = "potato",
		yield_part = "tubers",
		yield_isfresh = TRUE,
		trial_id = trimws(as.character(r$season)),
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA
	)

	carobiner::write_files(path, meta, d)
}
