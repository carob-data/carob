# R script for "carob"
# license: GPL (>=3)

## NOTES
# Conservation agriculture systems in the dictionary are stored as treatment. CP2 and RI-M+C are not in that dictionary, so those codes are kept as recorded.
# Nitrogen in this file is soil nitrogen in kg/ha. It is not fertilizer N, and it is not soil_N (mg/kg), so it is not stored.
# Phosphorus and potassium are soil concentrations in mg/kg. Clay, sand, and organic C are percent.
# The dictionary does not give a unit for grain or biomass. Both stay below 14, so they are stored as t/ha converted to kg/ha.
# Season is the cropping-season year, stored as planting_date. It is not a day.
# The dataset covers Malawi, Mozambique, Zambia, and Zimbabwe. The file does not say which station is in which country, and it has no coordinates.

## ISSUES
# Grain and biomass units are not in the dictionary. They are stored as kg/ha on the reading that the recorded values are t/ha.
# CP2 (32 rows) and RI-M+C (73 rows) are not defined in the dictionary.
# No country column and no coordinates.
# write_files: missing variables: carob_contributor. missing variables: yield_moisture, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: biomass_kg_ha_.

## NEW VARIABLES
# biomass_kg_ha_: maize aboveground biomass in kg/ha. The dictionary does not say fresh or dry, and it does not state the unit.

## NOT PROCESSED
# Soil nitrogen (kg/ha). Rainfall has no unit in the dictionary.


carob_script <- function(path) {

"
Grain yield from conservation agriculture experiments at six stations, seasons 2005 to 2019. Stations are Henderson, Monze Farmer Training Centre, Chitedze, Sussundenga, Domboshawa, and Msekera. The dataset covers Malawi, Mozambique, Zambia, and Zimbabwe.
"

	uri <- "hdl:11529/10548832"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = "doi:10.1016/j.agee.2021.107812",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "DATA SA 2005 to 2019.xls"]
	r <- carobiner::read.excel(f, sheet = "Raw Data")

	sys <- c(
		"CP" = "conventional ploughing",
		"RI" = "ripping",
		"JP" = "jab planter",
		"DS-MSy" = "direct seeding and maize-soybean rotation",
		"DS-MSfC" = "direct seeding and maize-sunflower-cotton rotation",
		"DS-MSf" = "direct seeding and maize-sunflower rotation",
		"DS-MG" = "direct seeding and maize-groundnut rotation",
		"DS-MCtS" = "direct seeding and maize-cotton-sunn hemp rotation",
		"DS-MCt" = "direct seeding and maize-cotton rotation",
		"DS-MC" = "direct seeding and maize-cowpea rotation",
		"DS-MBio" = "direct seeding and maize and biochar",
		"DS-M+Pp" = "direct seeding and maize-pigeon pea intercrop",
		"DS-M+C" = "direct seeding and maize-cowpea intercrop",
		"DS" = "direct seeding sole maize",
		"DiS-MC" = "dibble stick and maize-cowpea rotation",
		"DiS-M+Pp" = "dibble stick and maize-pigeon pea intercrop",
		"DiS-M+Mp" = "dibble stick and maize-velvet bean intercrop",
		"DiS-M+C" = "dibble stick and maize-cowpea intercrop",
		"DiS" = "dibble stick",
		"CP-MCt" = "conventional ploughing and maize-cotton rotation",
		"BA" = "basins"
	)
	label <- sys[r$System]
	label[is.na(label)] <- r$System[is.na(label)]

	loc <- c(
		"HRS" = "Hennderson Research Station",
		"MFTC" = "Monze Farmer Training Centre",
		"CRS" = "Chitedze Research Station",
		"DTC" = "Domboshawa Training Centre",
		"SRS" = "Sussundenga Research Station",
		"MRS" = "Msekera Research Station"
	)

	prep <- rep(NA_character_, nrow(r))
	prep[r$System %in% c("CP", "CP-MCt")] <- "ploughing"
	prep[r$System == "RI"] <- "ripping"
	prep[r$System == "BA"] <- "basins"

	rotation <- rep(NA_character_, nrow(r))
	rotation[r$System == "DS-MSy"] <- "maize;soybean"
	rotation[r$System == "DS-MSfC"] <- "maize;sunflower;cotton"
	rotation[r$System == "DS-MSf"] <- "maize;sunflower"
	rotation[r$System == "DS-MG"] <- "maize;groundnut"
	rotation[r$System == "DS-MCtS"] <- "maize;cotton;sunn hemp"
	rotation[r$System %in% c("DS-MCt", "CP-MCt")] <- "maize;cotton"
	rotation[r$System %in% c("DS-MC", "DiS-MC")] <- "maize;cowpea"

	inter <- rep(NA_character_, nrow(r))
	inter[r$System %in% c("DS-M+Pp", "DiS-M+Pp")] <- "pigeon pea"
	inter[r$System %in% c("DS-M+C", "DiS-M+C")] <- "cowpea"
	inter[r$System == "DiS-M+Mp"] <- "velvet bean"

	d <- data.frame(
		trial_id = r$Location,
		location = loc[r$Location],
		planting_date = as.character(r$Season),
		plot_id = paste(r$Season, r$Plot, r$Rep),
		rep = as.integer(r$Rep),
		treatment = label,
		land_prep_method = prep,
		crop_rotation = rotation,
		intercrops = inter,
		intercropped = !is.na(inter),
		crop = "maize",
		yield_part = "grain",
		yield = r$Grain * 1000,
		biomass_kg_ha_ = r$Biomass * 1000,
		soil_P = r$Phosphorus,
		soil_K = r$Potassium,
		soil_clay = r$Clay,
		soil_sand = r$Sand,
		soil_SOC = r$OrgC,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
