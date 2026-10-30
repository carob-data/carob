# R script for "carob"
# license: GPL (>=3)

## NOTES
# Advanced late-blight and heat-tolerance yield trial at San Ramon, Junin, Peru.
# The dictionary states MTbWg, NoMTWg, and TubWg in kg per plot, and MTYNA, TbYldNAj, MTYA, and TTYA in t/ha. Yield is the adjusted total tuber yield TTYA times 1000. Yield_marketable is MTYA times 1000.
# DM % is dry matter percentage.
# A dash in plants harvested is missing.
# The dataset geographic coverage gives one west longitude and one south latitude. Those are the coordinates used here.

## ISSUES
# The dictionary does not give a scale for tuber appearance, uniformity, or size.
# No fertilizer rate, irrigation statement, or planting and harvest dates. The dataset time span is 2021-06-01 to 2022-06-01 and is not labeled as planting or harvest.
# The design is described as 16 rows by 12 columns with three repetitions. It is not named as a standard design.
# write_files: missing variables: carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: clone_type_, field_row_, field_col_, n_plants_harvested_, dry_matter_percent_.

## NOT PROCESSED
# TubApp, TubUni, TubSiz: scores with no stated scale.
# NTP, NMTb, NNMTb, MTbWg, NoMTWg, TubWg, MTYNA, TbYldNAj: plot counts and the unadjusted yields. TTYA and MTYA are the per-area yields.
# 02_DataDictionary: column definitions.


carob_script <- function(path) {

"
Dataset for: yield and stability advanced trial for late blight and heat tolerant (LBHT) potato population conducted in San Ramón, Peru.

Dataset for yield and stability advanced trial for late blight and heat tolerant (LBHT) potato population conducted in San Ramón. 150 advanced clones of the LBHT  and heat-tolerant population, with four control varieties Yungay, Kory, Amarilis and Desiree, and 23 parents were planted in San Ramón Peru between 2021 and 2022.  (16 Rows x 12 Columns)
"

	uri <- "doi:10.21223/BMWFX2"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable;dry_matter_percent_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_data_advanced_trial_LBHTC2_SanRamon2021.xlsx"]
	r <- carobiner::read.excel(f, sheet="SRA21-02", na=c("", "NA"))
	names(r) <- trimws(names(r))
	variety <- trimws(as.character(r$CIPN))
	plants <- trimws(as.character(r$PltHrv))
	plants[plants %in% c("-", "")] <- NA_character_

	d <- data.frame(
		trial_id = "SanRamon_LBHT_2021",
		country = "Peru",
		adm1 = "Junin",
		adm2 = "Chanchamayo",
		location = "San Ramon",
		# dataset geographic coverage: one west longitude and one south latitude
		latitude = -11.1267144,
		longitude = -75.3576875,
		geo_from_source = TRUE,
		crop = "potato",
		variety = variety,
		variety_code = variety,
		clone_type_ = trimws(as.character(r$Type)),
		treatment = variety,
		rep = as.integer(r$Rep),
		plot_id = as.character(r$Plot),
		field_row_ = as.integer(r$Row),
		field_col_ = as.integer(r$Column),
		n_plants_harvested_ = as.numeric(plants),
		yield = as.numeric(r$TTYA) * 1000,
		yield_marketable = as.numeric(r$MTYA) * 1000,
		yield_part = "tubers",
		dry_matter_percent_ = as.numeric(r[["DM %"]]),
		on_farm = FALSE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
