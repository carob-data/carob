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
# County centroids (Elgeyo Marakwet, Meru, Nandi, Uasin Gishu) added from gazetteer (geo_from_source = FALSE).
# No variety, fertilizer, or calendar planting/harvest date. Season strings are stored in season.
# Some yield cells are empty.

## NEW VARIABLES
# none

## NOT PROCESSED
# Price, sales, input-cost, and gross-margin columns (ware and seed, KES and USD). They are not a single crop price.
# Seed quantity columns and percent_production_that_is_seed. ward is mapped to location.
# 16940_data_dictionary.xlsx: documentation or a codebook, not an observation table.

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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 45,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "16940_Decentralised_seed_multiplier_gross_margin_data.xlsx"]
	r <- carobiner::read.excel(f)
	sex <- trimws(as.character(r$gender))
	sex[sex == "M"] <- "male"
	sex[sex == "F"] <- "female"
	adm1 <- trimws(as.character(r$country))
	geo <- data.frame(
		adm1 = c("Elgeyo Marakwet", "Meru", "Nandi", "Uasin Gishu"),
		latitude = c(0.670, 0.047, 0.184, 0.514),
		longitude = c(35.508, 37.656, 35.127, 35.270)
	)
	gm <- match(adm1, geo$adm1)

	sraw <- trimws(as.character(r$season))
	season <- ifelse(grepl("^LR", sraw), "long rains",
		ifelse(grepl("^SR", sraw), "short rains", NA_character_))

	d <- data.frame(
		hhid = as.character(r$ID_name),
		season = season,
		season_code_ = sraw,
		sex = sex,
		country = "Kenya",
		adm1 = adm1,
		location = trimws(as.character(r$ward)),
		latitude = geo$latitude[gm],
		longitude = geo$longitude[gm],
		field_size = as.numeric(r$up_ha_units_of_production),
		yield = as.numeric(r$total_yield_t_ha) * 1000,
		crop = "potato",
		yield_part = "tubers",
		yield_isfresh = TRUE,
		trial_id = trimws(as.character(r$season)),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE,
		irrigated = NA
	)

	carobiner::write_files(path, meta, d)
}
