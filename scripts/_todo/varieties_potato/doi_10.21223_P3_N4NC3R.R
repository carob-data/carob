# R script for "carob"
# license: GPL (>=3)

## NOTES
# Interview rows in 01_Data_ economic profitability.xlsx.
# Measured harvest columns are kilograms (Cantidad_cosecha_medida_TOTAL_KG_*). No area column was found, so they are not converted to kg/ha.
# PMEJ and PNAT are potato (improved and native). Mashua kilograms are kept separately. Mashua is not a vocabulary crop name.
# Country Peru is the geographic coverage. Pazos and Quilcas are not assigned as coordinates.

## ISSUES
# No coordinates.
# No fertilizer rate that is clearly elemental N, P, or K. Guano and blended fertilizer amounts are kilograms without a nutrient analysis.
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: variety_improved_, variety_native_, harvest_kg_improved_, harvest_kg_native_, mashua_harvest_kg_.

## NOT PROCESSED
# Prices, expected and historical harvests, and seed quantities. The mapped harvest is the measured total kilograms.
# The rest of the 163 columns.

## NEW VARIABLES
# harvest_kg_improved_: measured improved-potato harvest, kg
# harvest_kg_native_: measured native-potato harvest, kg
# mashua_harvest_kg_: measured mashua harvest, kg
# variety_improved_: Variedad_PMEJ
# variety_native_: Variedad_PNAT

carob_script <- function(path) {

"
Dataset for: Exploratory field study of economic profitability among native potato and mashua producers: plot-level data.

This dataset comes from an exploratory field study with 21 farmers in the districts of Pazos (Huancavelica) and Quilcas (Junín), Peru. Semi-structured interviews were conducted with 11 farmers in Pazos and 10 farmers in Quilcas to capture and compare information on production costs and revenues for two groups of target plots with specific cropping combinations: (a) plots with improved potato varieties and (b) plots with native potato varieties grown with mashua. The data are provided at plot level.
"

	uri <- "doi:10.21223/P3/N4NC3R"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "harvest_kg_improved_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.35
	)

	f <- ff[grepl("profitability", basename(ff))]
	r <- carobiner::read.excel(f)
	num <- function(x) as.numeric(as.character(x))
	d <- data.frame(
		crop = "potato",
		variety_improved_ = trimws(as.character(r$Variedad_PMEJ)),
		variety_native_ = trimws(as.character(r$Variedad_PNAT)),
		harvest_kg_improved_ = num(r$Cantidad_cosecha_medida_TOTAL_KG_PMEJ),
		harvest_kg_native_ = num(r$Cantidad_cosecha_medida_TOTAL_KG_PNAT),
		mashua_harvest_kg_ = num(r$Cantidad_cosecha_medida_TOTAL_KG_MASH),
		country = "Peru",
		trial_id = "Pazos_Quilcas",
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA,
		stringsAsFactors = FALSE
	)
	d$plot_id <- as.character(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
