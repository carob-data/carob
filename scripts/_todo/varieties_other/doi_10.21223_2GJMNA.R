# R script for "carob"
# license: GPL (>=3)

## NOTES
# Drought trial at Umbeluzi Research Station, Mozambique. The image file and the dataset coverage give the same point, longitude 32.36186 and latitude -26.0518.
# This workbook is the flight on the sheet 031019. The image table says that flight is 190 days after planting. Canopy temperature is degrees C. Coberture is percent plant cover.
# Plot size is 25 m2. Plant spacing is 0.3 m and row spacing is 0.9 m. Spacing is stored in cm.
# Treatments are optimum irrigation and late-season drought. Drought plots were irrigated until 110 days after planting.

## ISSUES
# Urea 46N:0P:0K was applied three weeks after planting. The rate in kg/ha is not stated.
# No yield in this workbook.
# Standard deviations of the image pixels are not mapped.

## NEW VARIABLES
# irrigation_regime_: irrigation treatment (OI = optimum irrigation, LSD = late-season drought).
# field_row_, field_col_: plot row and column coordinates in the field layout.

## NOT PROCESSED
# Image_description columns for format, flight altitude, pixel size, and image width and height. Longitude and latitude are used.
# NDVI, NDVIgreen, and chlorophyll-index standard deviations.
# Data_Dictionary_Moz2.xlsx: the definitions used above.


carob_script <- function(path) {

"
Dataset for: Remote sensing and agronomic data for drought trials evaluated at Umbeluzi Research Station in 2019 (2nd experiment).

The trial had 120 clones,  the majority begin parents used in crossing blocks at Umbeluzi and Gurue Research Stations. The check clones such as Resisto and 199062.1 are parents in crossing blocks as well and some checks were landraces such as Xitsekele, Xiadla xakau. Trial life span started on 28 March 2019 and ended on 18 October 2019. The experiment was laid in two treatments defined as follows;   (i) Optimum Irrigation (OI, as control): No water restriction was imposed.  (ii) Late-season drought (LSD): Irrigation was withheld at 110 DAP and no irrigation was applied thereafter. Plot size was 25m2. The plots were arranged as 8 rows × 15 columns per treatment and replicated twice in each treatment. The plots were fertilized with urea, 46N:0P:0K at three weeks after planting and irrigated twice a week. All the field plots were kept weed-free by hoe weeding. A Cypermethrin (Avansagro, Shangai, China) insecticide was applied at 20 and 35 days after planting (DAP) in all trials with doses of 10mL 100mL-1 water.
"

	uri <- "doi:10.21223/2GJMNA"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;irrigation_regime_",
		response_vars = "ground_cover;NDVI;NDVI_green;canopy_temperature;chlorophyll_index",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Umbeluzi_2019_Moz2.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	r <- r[-1, ]
	f1 <- ff[basename(ff) == "Image_description_Moz2.xlsx"]
	r1 <- carobiner::read.excel(f1, na=c("", "NA"))
	variety <- trimws(as.character(r[[5]]))
	regime <- trimws(as.character(r[[6]]))

	d <- data.frame(
		trial_id = "Umbeluzi_2019",
		country = "Mozambique",
		adm1 = "Maputo",
		adm2 = "Boane",
		location = "Umbeluzi",
		# Image_description_Moz2.xlsx, and the same point in the dataset geographic coverage
		latitude = as.numeric(r1$Lat[1]),
		longitude = as.numeric(r1$Lon[1]),
		geo_from_source = TRUE,
		# Same CIP sweetpotato check clones (Resisto, 199062.1, Xitsekele, Xiadla xakau) as 1st experiment doi:10.21223/WJWMZB
		crop = "sweetpotato",
		variety = variety,
		variety_code = variety,
		treatment = regime,
		irrigation_regime_ = regime,
		planting_date = "2019-03-28",
		harvest_date = "2019-10-18",
		rep = as.integer(r[[4]]),
		plot_id = as.character(r[[1]]),
		field_row_ = as.integer(r[[2]]),
		field_col_ = as.integer(r[[3]]),
		plot_area = 25,
		plant_spacing = 0.3 * 100,
		row_spacing = 0.9 * 100,
		DAP = as.integer(r1$DAP[1]),
		canopy_temperature = as.numeric(r[["Canopy Temperature (°C)"]]),
		NDVI = as.numeric(r$NDVI),
		NDVI_green = as.numeric(r$NDVIgreen),
		chlorophyll_index = as.numeric(r$CI),
		ground_cover = as.numeric(r$Coberture),
		irrigated = TRUE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
