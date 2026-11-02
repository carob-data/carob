# R script for "carob"
# license: GPL (>=3)

## NOTES
# Metadata sheet describes every column as a field measurement. No column is identified as a model prediction, so none was dropped for that reason.
# ferti is HF or LF and irrig is a fraction (0.25, 0.5, 0.75, 1). Rates in kg/ha and irrigation in mm are not in the file.
# Seedcotton yield is already kg/ha. Fiber moisture is fiber HVI moisture, not seedcotton yield moisture.
# Leaf, stem and reproductive biomass are g per 5 plants. Converted with the recorded 12.5 plants/m2.

## ISSUES
# N, P and K rates are not in the file (only HF/LF). No harvest date. Coordinates are a bounding box, not a point.
# yield_part "seedcotton" is not in the terminag yield_part list. One anthesis cell is the text NA.
# write_files: missing yield_moisture, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown variables fiber_percentage_, boll_harvest_index_, fertilization_level_, irrigation_level_, fiber_moisture_, fiber_micronaire_, fiber_maturity_, fiber_length_, fiber_uniformity_, fiber_short_, fiber_strength_, fiber_elongation_, fiber_reflectance_, fiber_yellowness_, boll_weight_, node_number_; invalid terms yield_part seedcotton; high crop yield cotton 8730.

## NEW VARIABLES
# fertilization_level_: HF or LF (high or low), as coded. Not a nutrient rate.
# irrigation_level_: fraction of the irrigation treatment (dimensionless).
# fiber_moisture_: fiber moisture (%)
# fiber_micronaire_: fiber micronaire
# fiber_maturity_: fiber maturity (0-1)
# fiber_length_: upper-half mean length (mm)
# fiber_uniformity_: length uniformity (0-1 in the metadata; values are about 80)
# fiber_short_: short fiber (%)
# fiber_strength_: fiber strength (g/tex)
# fiber_elongation_: fiber elongation (%)
# fiber_reflectance_: fiber reflectance (%)
# fiber_yellowness_: fiber yellowness
# fiber_percentage_: fiber content of seedcotton (%)
# boll_weight_: mean boll weight (g/boll)
# boll_harvest_index_: seedcotton weight over boll weight (%)
# node_number_: main-stem node count

## NOT PROCESSED
# boll_nb_m2: bolls per m2, redundant with yield and boll weight
# anthesis_date: used as flowering_date

carob_script <- function(path) {

"
Cotton experimental dataset for modeling cultivar * irrigation * fertilization interactions in Greece from 2018 to 2020

Cotton dataset collected on HAO-DEMETER research station in Thessaloniki, Greece from 2018 to 2020. The experiment was set up to study 3 agronomic factors: 4 cultivars, 4 levels of irrigation, 2 levels of fertilization.
The dataset includes cotton phenology, morphology, physiology, leaf area index, biomass, yield components and fiber quality.
"

	uri <- "doi:10.18167/DVN1/XQU8ZF"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;fertilization_level_;irrigation_level_",
		response_vars = "yield;plant_height;LAI",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 75,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Cotton dataset Greece 2018 to 2020"]
	r <- carobiner::read.excel(f, sheet="Dataset")
	num <- function(x) {
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "na")] <- NA
		as.numeric(x)
	}

	# g/5 plants to kg/ha at 12.5 plants/m2: g * 12.5 / 5 * 10
	per5_to_kgha <- 25

	d <- data.frame(
		country = "Greece",
		location = "Thermi",
		crop = "cotton",
		variety = r$cultivar,
		fertilization_level_ = r$ferti,
		irrigation_level_ = num(r$irrig),
		treatment = paste(r$cultivar, r$ferti, r$irrig),
		plot_id = as.character(r$plot),
		planting_date = as.character(as.Date(r$planting_date, tz="UTC")),
		flowering_date = as.character(as.Date(num(r$anthesis_date), origin="1899-12-30")),
		flowering_days = num(r$anthesis_DAP),
		anthesis_days = num(r$anthesis_DAP),
		yield = num(r$seedcotton_yield_kgha),
		yield_part = "seedcotton",
		plant_density = num(r$plant_density_m2) * 10000,
		fiber_moisture_ = num(r$moisture),
		fiber_micronaire_ = num(r$micronaire),
		fiber_maturity_ = num(r$maturity),
		fiber_length_ = num(r$length),
		fiber_uniformity_ = num(r$length_uniformity),
		fiber_short_ = num(r$short_fiber),
		fiber_strength_ = num(r$strength),
		fiber_elongation_ = num(r$elongation),
		fiber_reflectance_ = num(r$reflectance),
		fiber_yellowness_ = num(r$b),
		fiber_percentage_ = num(r$fiber_percentage),
		boll_weight_ = num(r$boll_weight_g),
		boll_harvest_index_ = num(r$boll_harvest_index),
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = TRUE
	)
	# country from geographicCoverage (Greece) and the title. location from geographicCoverage city Thermi.
	# irrigated: every irrig level is a fraction of irrigation, including 0.25.

	d$trial_id <- as.character(r$year)
	d$record_id <- as.integer(seq_len(nrow(d)))

	piece <- function(dap, lai, spad, height, nodes, leaf, stem, repro) {
		data.frame(
			record_id = d$record_id,
			DAP = as.integer(dap),
			LAI = num(lai),
			SPAD = num(spad),
			plant_height = num(height),
			node_number_ = num(nodes),
			dmy_leaves = num(leaf) * per5_to_kgha,
			dmy_stems = num(stem) * per5_to_kgha,
			dmy_storage = num(repro) * per5_to_kgha
		)
	}
	long <- rbind(
		piece(70, r$lai_70, r$spad_70, r$height_70, r$node_nb_70, r$biomass_leaf_70, r$biomass_stem_70, r$biomass_reproductive_70),
		piece(80, r$lai_80, NA, NA, NA, NA, NA, NA),
		piece(100, r$lai_100, NA, NA, NA, NA, NA, NA),
		piece(110, NA, r$spad_110, NA, NA, NA, NA, NA),
		piece(120, r$lai_120, NA, r$height_120, r$node_nb_120, r$biomass_leaf_120, r$biomass_stem_120, r$biomass_reproductive_120)
	)
	keep <- !is.na(long$LAI) | !is.na(long$SPAD) | !is.na(long$plant_height) | !is.na(long$dmy_leaves)
	long <- long[keep, ]

	carobiner::write_files(path, meta, d, long=long)
}
