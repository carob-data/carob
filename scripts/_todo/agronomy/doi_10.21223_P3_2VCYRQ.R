# R script for "carob"
# license: GPL (>=3)

## NOTES
# UNICA potato, drip irrigation, La Molina, June to September 2018. T1 is full irrigation, T2 is CWSI below 0.4, T3 is CWSI below 0.7. Five blocks.
# Dictionary: biomass is the mean of 4 plants per plot. It does not state grams or kilograms. Canopy cover is described as a percentage; the values are between 0 and 1.
# Coordinates are the Dataverse point (south -12.076289, west -76.948417).

## ISSUES
# Biomass cannot be converted to kg/ha: the mass unit is not stated and the plot area is not given.
# Irrigation volume has no unit.
# CWSI hour values are not usable clock times.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: harvest_, irrigation_volume_, fresh_leaf_, fresh_stem_, fresh_root_, fresh_tuber_, tuber_number_, dry_leaf_, dry_stem_, dry_root_, dry_tuber_, canopy_cover_, cwsi_.

## NOT PROCESSED
# CWSI sheet sensor columns other than CWSI (foliage and air temperature, humidity, vapour pressure, PAR, solar radiation, wind). Units are not in the dictionary.
# Irrigation daTI and nIrri: timing indexes. The volume is kept.

## NEW VARIABLES
# fresh_leaf_, fresh_stem_, fresh_root_, fresh_tuber_, dry_leaf_, dry_stem_, dry_root_, dry_tuber_: biomass per plant. Unit is not stated.
# tuber_number_: tubers per plant.
# canopy_cover_: canopy cover as recorded.
# irrigation_volume_: irrigation volume per plot. Unit is not stated.
# cwsi_: crop water stress index.


carob_script <- function(path) {

"
Dataset for: Analyze the use of thresholds based on crop water stress index calculus in potato  'Unica' under drip irrigation

The objective of this study was to evaluate the UNICA potato crop variety's yield relate with the threshold of Crop Water Stress Index subjected to different water stress levels and under drip of irrigation. The trial was carried out in the experimental field of National Agrarian University La Molina during the months of June-2018 to September-2018. Three treatments of irrigation moments based on the Crop Water Stress Index (CWSI) were applied for this trial: Full irrigated (T1), CWSI less than 0.4 (T2), CWSI less than 0.7 (T3).
"

	uri <- "doi:10.21223/P3_2VCYRQ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "fresh_tuber_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Data.xls"]
	lab <- c(T1="T1 full irrigation", T2="T2 CWSI < 0.4", T3="T3 CWSI < 0.7")
	r1 <- carobiner::read.excel(f, sheet="Biomass")
	r2 <- carobiner::read.excel(f, sheet="Canopy cover")
	r3 <- carobiner::read.excel(f, sheet="Soil humidity")
	r4 <- carobiner::read.excel(f, sheet="Irrigation")
	r5 <- carobiner::read.excel(f, sheet="CWSI")

	d <- unique(data.frame(
		plot_id = as.character(r1$plot),
		block_id = as.character(r1$block),
		treatment = unname(lab[as.character(r1$treatment)])
	))
	d$record_id <- as.integer(d$plot_id)
	d$crop <- "potato"
	d$variety <- "Unica"
	d$country <- "Peru"
	d$adm1 <- "Lima"
	d$location <- "La Molina"
	d$latitude <- -12.076289
	d$longitude <- -76.948417
	d$geo_from_source <- TRUE
	d$planting_date <- "2018-06"
	d$harvest_date <- "2018-09"
	d$irrigated <- TRUE
	d$trial_id <- "LaMolina_2018"
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c("NA", "NULL")] <- NA_character_
		as.numeric(x)
	}
	b <- data.frame(
		record_id = as.integer(r1$plot),
		DAP = as.integer(r1$dap),
		harvest_ = as.integer(r1$harvest),
		fresh_leaf_ = num(r1$FL),
		fresh_stem_ = num(r1$FS),
		fresh_root_ = num(r1$FR),
		fresh_tuber_ = num(r1$FT),
		tuber_number_ = num(r1$nT),
		dry_leaf_ = num(r1$DL),
		dry_stem_ = num(r1$DS),
		dry_root_ = num(r1$DR),
		dry_tuber_ = num(r1$DT)
	)
	c <- data.frame(
		record_id = as.integer(r2$plot),
		DAP = as.integer(r2$dap),
		canopy_cover_ = num(r2$CC)
	)
	h <- data.frame(
		record_id = as.integer(r3$plot),
		DAP = as.integer(r3$dap),
		soil_GWC = num(r3$Hv)
	)
	v <- data.frame(
		record_id = as.integer(r4$plot),
		DAP = as.integer(r4$dap),
		irrigation_volume_ = num(r4$V)
	)
	w <- data.frame(
		record_id = as.integer(r5$plot),
		DAP = as.integer(r5$dap),
		cwsi_ = num(r5$CWSI)
	)
	dlong <- carobiner::bindr(b, c, h, v, w)

	carobiner::write_files(path, meta, d, long=dlong)
}
