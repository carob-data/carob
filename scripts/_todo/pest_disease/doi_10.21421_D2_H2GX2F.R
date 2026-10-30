# R script for "carob"
# license: GPL (>=3)

## NOTES
# Medium-duration groundnut at Gumel, rainfed. Dataverse year 2015. The file name has two dots before xlsx.
# DPWkgha is dry pod yield (kg/ha). DFWkgha is dry fodder. The first NPLE and NPH pair is plant population and plants harvested.
# Shelling ratio is a percentage. HSW (g/100 seeds) times 10 is seed_weight.
# DEM, DFF and DM are days.
# One cell of plot 312 (FDRF7-57) is the text NA for yield and several other traits. It is read as missing and the row is kept.
# Coordinates are the bounding-box midpoint. In the metadata the south latitude is slightly north of the north latitude; the midpoint of the two values is used.
# The title mentions foliar diseases. This workbook has no disease scores.

## ISSUES
# Kept in pest_disease because the raw files are in that folder. This is a yield variety trial without disease columns.
# DFWkgha for one plot is 19982.2 kg/ha. Left as recorded.
# The second NPLE/NPH pair and the gram columns are not the same numbers as the first pair and the kg/ha yields. Plot area is not given, so they were not converted.
# No fertilizer. Irrigated is FALSE from the rainfed statement.
# Checks among the 18 names are not flagged.
# write_files: missing carob_contributor, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown plant_population_, plants_harvested_, sound_mature_kernel_; NA yield on plot 312 (FDRF7-57), left as recorded.

## NOT PROCESSED
# S.no, Entry number.
# NPLE...17 and NPH...18: second plant-count pair. The description lists NPLE and NPH once.
# DFW, DPWg, DPWKg, DPWKg_h: fodder and pod weights in g or kg. DPWkgha and DFWkgha were used.

## NEW VARIABLES
# plant_population_: first NPLE column, plants per plot.
# plants_harvested_: first NPH column, plants per plot.
# sound_mature_kernel_: SMK as recorded. Unit not stated.

carob_script <- function(path) {

"
Evaluation of medium duration lines for agronomic traits and Foliar diseases under rainfed condition in Gumel

Medium-Duration Groundnut for Multi-location evaluation comprises 15 breeding lines of groundnut and 3 released Varieties. The trial was conducted under rainfed condition and it was aimed to test their adaptability, yield superiority, resistance and tolerance to biotic and abiotic stresses location which includes; Gumel to confirm their        Experimental location on Google Map
"

	uri <- "doi:10.21421/D2/H2GX2F"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = "3 replications in blocks",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_residue",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Data file of Medium duration lines for agronomic traits in the location Gumel..xlsx"]
	r <- carobiner::read.excel(f, na="NA")

	d <- data.frame(
		variety = trimws(r$Variety),
		treatment = trimws(r$Variety),
		rep = as.integer(r[["Replication number"]]),
		block_id = as.character(r$Block),
		plot_id = as.character(r[["Plot no"]]),
		emergence_days = as.numeric(r$DEM),
		flowering_days = as.numeric(r$DFF),
		maturity_days = as.numeric(r$DM),
		plant_population_ = as.numeric(r[["NPLE...9"]]),
		plants_harvested_ = as.numeric(r[["NPH...10"]]),
		yield = as.numeric(r$DPWkgha),
		dmy_residue = as.numeric(r$DFWkgha),
		seed_weight = as.numeric(r$HSW) * 10,
		shelling_percentage = as.numeric(r[["Shelling ratio"]]),
		sound_mature_kernel_ = as.numeric(r$SMK),
		stringsAsFactors = FALSE
	)
	d$crop <- "groundnut"
	d$yield_part <- "pod"
	d$yield_moisture <- 0
	d$yield_isfresh <- FALSE
	d$irrigated <- FALSE
	d$country <- "Nigeria"
	d$location <- "Gumel"
	d$planting_date <- "2015"
	d$trial_id <- "Gumel_2015"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	d$longitude <- (9.370741 + 9.393268) / 2
	d$latitude <- (12.619884 + 12.620996) / 2
	d$geo_uncertainty <- 1225
	d$geo_source <- "ICRISAT Dataverse bounding box"

	carobiner::write_files(path, meta, d)
}
