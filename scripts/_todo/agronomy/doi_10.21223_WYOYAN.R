# R script for "carob"
# license: GPL (>=3)

## NOTES
# Conservation-agriculture potato trial at Licame, Chugay, Sánchez Carrión, La Libertad, Peru. Variety INIA-325 Poderosa.
# The dictionary states FTY and MTY in t ha-1. Yield and yield_marketable are those columns times 1000. DMCt is tuber dry matter in percent.
# The abstract gives plots of 3 x 4 m and spacing of 0.3 m between plants and 1 m between furrows. Spacing is stored in cm.
# The dataset geographic coverage is one west longitude and one south latitude.

## ISSUES
# The abstract gives the season as 23 October 2023 to 15 April 2024. Those dates are not labeled planting and harvest.
# No fertilizer rate and no irrigation statement.
# Benefit-cost ratio and carbon footprint are in the yield file. The columns were not mapped because the dictionary was not used to separate CF, CF_ha, and CF_ton.
# Soil moisture, soil temperature, decomposition, and soil analysis are repeated or laboratory tables and are not in the plot table.
# The data include minimum tillage as well as conventional and zero tillage, and a mulch level of 0. The abstract names conventional and zero tillage and mulch of 10, 20, and 30 cm.
# mulch in the vocabulary is kg/ha. The factor here is thickness in cm, so it is not stored as mulch.
# write_files: missing variables: carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: tillage_ (age?). unknown variable: mulch_thickness_ (mulch?). unknown variables: plastic_, dry_matter_percent_.

## NOT PROCESSED
# 1_Harvest: tuber weights in kg per inner plot. FTY is the per-area yield.
# 4_Descomposition rate, 5_Gravimetric soil moisture, 6_Soil temperature, 7_Soil analysis.
# Design Schematic_AC.xlsx: a layout sheet.
# BCR, CF, CF_ha, CF_ton.


carob_script <- function(path) {

"
Dataset for: Conservation agriculture trial in the locality of Licame, Chugay district, La Libertad - Peru (Second season 2023-2024)

An experimental potato trial based on conservation agriculture in the second season (October 23rd, 2023 to April 15th, 2024) was conducted in a total area of 987 m2 with the support of the 'Asociación de Productores Agropecuarios Mi Licame Unido' of the locality of Licame, district of Chugay, province of Sánchez Carrión, La Libertad. For this trial, the improved potato variety INIA-325 Poderosa was used with a split-plit plot design considering three factors: tillage (conventional and zero tillage), plastic barriers (with and without barrier) and amount of oat/barley of mulching (10, 20 and 30 cm thickness). The combination of these factors generated a total of 12 treatments; and an additional control treatment (conventional practices in the area). Four replicates were established, with a total of 52 plots of 3x4 m2 each (distance between plant and furrow of 0.3 m and 1 m, respectively). The variables evaluated were: fresh tuber yield (t ha-1), cost-benefit ratio, carbon footprint (kg CO2e Total), descompositio rate, soil moisture (%), soil temperature (°C) and soil analysis at different times during the vegetative period of the crop.
"

	uri <- "doi:10.21223/WYOYAN"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "split-split plot",
		data_type = "experiment",
		treatment_vars = "tillage_;plastic_;mulch_thickness_",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.45
	)

	fy <- ff[grepl("^2_Yield", basename(ff))]
	fd <- ff[grepl("^3_Dry", basename(ff))]
	y <- carobiner::read.excel(fy, na=c("", "NA"))
	dm <- carobiner::read.excel(fd, na=c("", "NA"))
	names(y) <- trimws(names(y))
	names(dm) <- trimws(names(dm))
	dmc <- dm$DMCt[match(y$ID, dm$ID)]

	d <- data.frame(
		trial_id = "Licame_2023",
		country = "Peru",
		adm1 = "La Libertad",
		adm2 = "Sánchez Carrión",
		adm3 = "Chugay",
		location = "Licame",
		# dataset geographic coverage: one west longitude and one south latitude
		latitude = -7.76,
		longitude = -77.85,
		geo_from_source = TRUE,
		crop = "potato",
		variety = "INIA-325 Poderosa",
		treatment = trimws(as.character(y$Code)),
		tillage_ = trimws(as.character(y$TT_f1)),
		plastic_ = trimws(as.character(y$PB_f2)),
		mulch_thickness_ = trimws(as.character(y$M_f3)),
		rep = as.integer(y$Block),
		plot_id = as.character(y$ID),
		plot_area = 3 * 4,
		plant_spacing = 0.3 * 100,
		row_spacing = 1 * 100,
		yield = as.numeric(y$FTY) * 1000,
		yield_marketable = as.numeric(y$MTY) * 1000,
		yield_part = "tubers",
		dry_matter_percent_ = as.numeric(dmc),
		on_farm = TRUE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
