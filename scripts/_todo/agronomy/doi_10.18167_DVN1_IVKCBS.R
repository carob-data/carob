# R script for "carob"
# license: GPL (>=3)

## NOTES
# Grain rows in Rice Biomass & Grains are measured treatment means (t/ha) with a standard error, kept as kg/ha. They are not STICS output. Vegetative biomass is the long table.
# F1 mineral fertilizer is 0. F2 is 400 kg/ha NPK 11/22/16 plus 200 kg/ha urea (Fertilization sheet). P and K use the oxide factors. Manure t DM/ha is OM_amount from that sheet (7.5 in 2016-2017 and 5.0 in 2017-2018). The practices text says 7.5 t/ha in both seasons; the fertilization sheet is used.
# Cropping practices: RMC is rice in rotation with a mucuna and crotalaria mixture (mucuna stored as velvet bean). RR is rice monocropping. Variety Nerica 4 is the description. Sowing was 23 and 24 Nov 2016, and 24 and 25 Nov 2017, so planting_date is the month.
# Coordinates and elevation are the description: 19°33'18.90" S, 46°24'53.83" E, 930 m. Country Madagascar is geographicCoverage (the title spells Magadascar). Rainfed, so irrigated is FALSE. City Ivory is the description. The state field in geographicCoverage is a geonames URL, not an admin name.

## ISSUES
# Records are treatment means, not plots. Grain moisture is not stated. Soil moisture, soil N initialization, and residue tables are not mapped.
# write_files: missing variables: project. missing variables: yield_moisture. unknown variable: yield_se_ (yield?). unknown variables: vegetative_biomass_.

## NOT PROCESSED
# Moisture initialization, soil N initialization, DataResidues, and the soil water and N time series. Rice N in biomass and grain.

## NEW VARIABLES
# yield_se_: standard error of the treatment-mean yield, kg/ha.
# vegetative_biomass_: vegetative biomass time series, kg/ha, from the mean amount column.


carob_script <- function(path) {

"
Dataset for modelling agronomic and environmental performances of rainfed rice based cropping systems with or without legume in Magadascar

This dataset provide data for calibration and evaluation of the model Stics in the specific context of malagasy higlands with rice cultivar Nerica 4. Data include soil moisture and N initialization, fertilization applied (quality and quantity), cropping practices, data on rice biomass and grain, N in biomass and grain for two cropping seasons (2016/217 and 2017/2018) at Ivory station (19°33’18.90’’ lat. S, 46°24'53.83’’ long. E, 930 m a‧s‧l.).
"

	uri <- "doi:10.18167/DVN1/IVKCBS"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.agee.2021.107576",
		design = NA,
		data_type = "experiment",
		treatment_vars = "crop_rotation;N_fertilizer",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "DatasetModellingRiceLegume.xlsx"]
	g <- carobiner::read.excel(f, sheet = "Rice Biomass & Grains")
	fz <- carobiner::read.excel(f, sheet = "Fertilization")

	om_col <- grep("fumier", names(fz), ignore.case = TRUE, value = TRUE)[1]
	fz$OM_amount <- as.numeric(fz[[om_col]]) * 1000
	fz$N_fertilizer <- ifelse(fz$Fertilization == "F1", 0, 400 * 0.11 + 200 * 0.46)
	fz$P_fertilizer <- ifelse(fz$Fertilization == "F1", 0, 400 * 0.22 / 2.29)
	fz$K_fertilizer <- ifelse(fz$Fertilization == "F1", 0, 400 * 0.16 / 1.2051)
	fz$fertilizer_type <- ifelse(fz$Fertilization == "F1", "none", "NPK;urea")
	fz <- fz[, c("Year", "Fertilization", "OM_amount", "N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_type")]

	veg <- g[g[["Type of biomass"]] == "Vegetative biomass", ]
	g <- g[g[["Type of biomass"]] == "Grain", ]
	g <- merge(g, fz, by = c("Year", "Fertilization"), all.x = TRUE)

	# Description: 19°33'18.90" S, 46°24'53.83" E, 930 m a.s.l.
	lat <- -(19 + 33/60 + 18.90/3600)
	lon <- 46 + 24/60 + 53.83/3600

	d <- data.frame(
		trial_id = g$Year,
		plot_id = paste(g$Year, g$Treatment, g$Fertilization, sep = "_"),
		crop = "rice",
		variety = "Nerica 4",
		country = "Madagascar",
		location = "Ivory",
		latitude = lat,
		longitude = lon,
		elevation = 930,
		geo_from_source = TRUE,
		planting_date = ifelse(g$Year == "2016-2017", "2016-11", "2017-11"),
		harvest_date = as.character(as.Date(g[["Date of Observation"]])),
		crop_rotation = ifelse(g$Treatment == "RMC", "velvet bean;crotalaria", "rice"),
		treatment = paste(g$Treatment, g$Fertilization),
		N_fertilizer = g$N_fertilizer,
		P_fertilizer = g$P_fertilizer,
		K_fertilizer = g$K_fertilizer,
		fertilizer_type = g$fertilizer_type,
		OM_amount = g$OM_amount,
		OM_used = TRUE,
		yield = as.numeric(g[["Mean amount (t/ha)"]]) * 1000,
		yield_se_ = as.numeric(g[["Standard error"]]) * 1000,
		yield_part = "grain",
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	veg$plot_id <- paste(veg$Year, veg$Treatment, veg$Fertilization, sep = "_")
	long <- data.frame(
		plot_id = veg$plot_id,
		date = as.character(as.Date(veg[["Date of Observation"]])),
		vegetative_biomass_ = as.numeric(veg[["Mean amount (t/ha)"]]) * 1000
	)
	long <- long[long$plot_id %in% d$plot_id & !is.na(long$date), ]

	carobiner::write_files(path, meta, d, long = long)
}
