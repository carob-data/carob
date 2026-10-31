# R script for "carob"
# license: GPL (>=3)

## NOTES
# Grain yield is already kg/ha at 12.5% moisture (code book).
# The year column is the harvest year. Seasons in the description are 2020/21 to 2022/23, so the planting year is the previous calendar year.
# Coordinates are in the dataset description: Domboshava Training Centre 17.62 S, 31.17 E; University of Zimbabwe farm 17.73 S, 31.020 E.
# The trial is rainfed and was run under conservation agriculture.
# Fertilizer is recorded only as Fert+ or Fert-. The product and the rate are not in the file.
# MZPP1 is the traditional intercrop. MZCP2 and MZPP2 are double-row strip cropping (code book).

## ISSUES
# Fertilizer product and N, P, and K rates are not in the file.
# Biomass is not stated as fresh or dry. It is stored as biomass_kg_ha_.
# write_files: , K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: biomass_kg_ha_.

## NEW VARIABLES
# biomass_kg_ha_: above-ground biomass in kg/ha, excluding maize cobs and legume pod shells (code book). Grain is reported separately.

## NOT PROCESSED
# system_data, radar_plot_data, and conversion_factors are calculated from the plot yields. They are not plot records.


carob_script <- function(path) {

"
Grain yield, biomass, and nutritional values of various cropping systems tested on-station with and without fertilizer on two soils in Zimbabwe, harvest years 2021 to 2023. Sites are the Domboshava Training Centre and the University of Zimbabwe farm. Systems are sole maize, maize-cowpea and maize-pigeon pea rotations, a maize-pigeon pea intercrop, and double-row strip crops of maize with cowpea or pigeon pea.
"

	uri <- "doi:10.71682/10549322"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Masamba et al_contrasting legumes and layouts_data.xlsx"]
	r <- carobiner::read.excel(f, sheet = "yield_data")

	sys <- c(
		"MZ" = "sole maize",
		"MZ-CP" = "maize-cowpea rotation",
		"MZ-PP" = "maize-pigeon pea rotation",
		"MZPP1" = "maize-pigeon pea intercrop",
		"MZCP2" = "maize-cowpea double-row strip",
		"MZPP2" = "maize-pigeon pea double-row strip"
	)
	label <- sys[r$cropping.system]

	crop <- r$crop
	crop[crop == "pigeonpea"] <- "pigeon pea"

	intercrops <- rep(NA_character_, nrow(r))
	pp <- r$cropping.system %in% c("MZPP1", "MZPP2")
	cp <- r$cropping.system == "MZCP2"
	intercrops[pp & crop == "maize"] <- "pigeon pea"
	intercrops[pp & crop == "pigeon pea"] <- "maize"
	intercrops[cp & crop == "maize"] <- "cowpea"
	intercrops[cp & crop == "cowpea"] <- "maize"

	itype <- rep(NA_character_, nrow(r))
	itype[r$cropping.system == "MZPP1"] <- "mixed"
	itype[r$cropping.system %in% c("MZCP2", "MZPP2")] <- "strip"

	rotation <- rep(NA_character_, nrow(r))
	rotation[r$cropping.system == "MZ-CP"] <- "maize;cowpea"
	rotation[r$cropping.system == "MZ-PP"] <- "maize;pigeon pea"

	site <- data.frame(
		site = c("DTC", "UZ"),
		location = c("Domboshava Training Centre", "University of Zimbabwe farm"),
		latitude = c(-17.62, -17.73),
		longitude = c(31.17, 31.020)
	)

	d <- data.frame(
		country = "Zimbabwe",
		trial_id = r$site,
		location = site$location[match(r$site, site$site)],
		latitude = site$latitude[match(r$site, site$site)],
		longitude = site$longitude[match(r$site, site$site)],
		plot_id = r$plot,
		rep = as.integer(sub(".*\\.", "", r$rep)),
		planting_date = as.character(r$year - 1),
		harvest_date = as.character(r$year),
		crop = crop,
		yield_part = ifelse(crop == "maize", "grain", "seed"),
		treatment = paste(label, r$fertilizer),
		crop_rotation = rotation,
		intercrops = intercrops,
		intercrop_type = itype,
		intercropped = !is.na(intercrops),
		yield = r$grain.kg.ha,
		yield_moisture = 12.5,
		biomass_kg_ha_ = r$biomass.kg.ha,
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = FALSE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
