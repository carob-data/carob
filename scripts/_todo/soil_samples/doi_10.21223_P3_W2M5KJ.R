# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil fertility under maca, fallow, native grassland, and cultivated pasture in the Junín department, Peru. The dictionary says every location is in that department.
# X and Y are UTM. The file does not give the zone or the datum, so they are not stored as longitude and latitude.
# TOC is percent and is soil_SOC. Bray-1 P is mg/kg. Exchangeable Ca, Mg, K, Na, and Al+H, and CEC, are cmolc/kg.
# Where the fertilization answer is No, the manure rate is 0. "Did not answer" is left missing. The rate is Mg/ha/year of the named manure, not elemental N, P, or K.
# Maca yield is the farmer's approximate yield from the previous season, Mg/ha/year, stored as kg/ha.

## ISSUES
# No longitude or latitude.
# Total N is a percentage. soil_N is extractable N in mg/kg, so total N is not stored.
# Bray-1 P above 200 mg/kg is kept.
# write_files: missing latitude, longitude. unknown variables manure_type_, manure_rate_, maca_yield_. out of bounds soil_P (0.867, 517.219).

## NOT PROCESSED
# NH4-N and NO3-N. Those columns are mineralization rates in µg/cm2/day, not a soil concentration.
# Carbonates. The dictionary calls it a presence dummy and gives no unit.
# 15N of total N, the TOC:N ratio, 15N of roots, aggregate stability, and above-ground, below-ground, and total biomass.
# Slope, cover, bulk-density method notes, and how long the land use has been in place.
# Data_dictionary.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# manure_type_: sheep manure, guano, or chicken manure.
# manure_rate_: manure applied, Mg/ha/year.
# maca_yield_: approximate maca yield of the previous season, kg/ha. Empty where the plot was not maca.


carob_script <- function(path) {

"
Replication data for: Land Use Effects on Soil Fertility and Nutrient Cycling in the Peruvian High-Andean Puna Grasslands
"

	uri <- "doi:10.21223/P3/W2M5KJ"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Database_fertility.csv"]
	r <- read.csv(f, check.names=FALSE)

	qty <- trimws(as.character(r$"Fertilization quantity"))
	qty[qty == "Did not answer"] <- NA_character_
	fert <- trimws(as.character(r$Fertilization))
	rate <- as.numeric(qty)
	rate[which(fert == "No")] <- 0
	manure <- trimws(as.character(r$"Fertilization type"))
	manure[manure %in% c("No", "NA", "")] <- NA_character_
	maca <- as.numeric(r$Maca.yield) * 1000
	land <- trimws(as.character(r$"Land use"))
	maca[which(land != "Maca cropland")] <- NA_real_

	d <- data.frame(
		trial_id = "Junin_puna_fertility",
		sample_id = as.character(r$ID),
		country = "Peru",
		adm1 = "Junín",
		location = carobiner::fix_name(r$Location, "title"),
		land_use = land,
		manure_type_ = manure,
		manure_rate_ = rate,
		maca_yield_ = maca,
		soil_pH = as.numeric(r$pH),
		soil_sand = as.numeric(r$Sand),
		soil_silt = as.numeric(r$Silt),
		soil_clay = as.numeric(r$Clay),
		soil_bd = as.numeric(r$"Bulk density"),
		soil_SOC = as.numeric(r$TOC),
		soil_P = as.numeric(r$"Bray1-P"),
		soil_CEC = as.numeric(r$CEC),
		soil_Ca_exch = as.numeric(r$Ca),
		soil_Mg_exch = as.numeric(r$Mg),
		soil_K_exch = as.numeric(r$K),
		soil_Na_exch = as.numeric(r$Na),
		soil_ex_acidity = as.numeric(r$"Al and H"),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
