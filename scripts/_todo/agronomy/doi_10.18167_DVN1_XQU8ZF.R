# R script for "carob"
# license: GPL (>=3)

## NOTES
# Associated publication (found by search): Tsaliki et al. (2024), "Cotton Cultivation in Greece under Sustainable Utilization of Inputs", Sustainability 16(1):347, doi:10.3390/su16010347 (ARIMNet2 TomorrowS project, grant ANR-17-ARM2-0001).
# Location coordinates, split-split-plot design, plot dimensions, soil properties (0-30 cm, Table 1), fertilization products and rates (Table 2), irrigation amounts/method, herbicide/insecticide management, previous crop (wheat), and harvest dates are taken from doi:10.3390/su16010347.
# anthesis_DAP is 1-based inclusive (anthesis_date - planting_date + 1), matching the harvest DAP reported in the paper.
# Leaf, stem, and reproductive dry biomass are reported as g per 5 plants and converted to kg/ha using the recorded 12.5 plants/m2. The Metadata sheet descriptions for biomass_reproductive_70 and biomass_reproductive_120 are swapped by typo; column names match the actual biomass magnitudes.
# moisture is HVI fiber moisture (%), not seedcotton harvest moisture. length_uniformity is described as [0-1] in the Metadata sheet, but values are percentages (76.3-87.8%).
# irrigation_amount is based on the paper's 4000 m3/ha (400 mm) total for 100% irrigation scaled by irrig (0.25, 0.5, 0.75, 1); all plots received 120 m3/ha (12 mm) establishment sprinkler irrigation plus 5 drip irrigations during flowering.
# Replicate (rep = 1..3) is derived from plot numbering: each year has six 16-plot blocks (plots 1-48 HF, 49-96 LF), each containing all 16 irrig x cultivar combinations.

## ISSUES
# yield_moisture (seedcotton harvest moisture) is not reported in the data or publication; omitted rather than set to NA.
# yield_part "seedcotton" is not in the terminag yield_part vocabulary; kept rather than mislabeling as "seed" or "lint".
# seedcotton_yield_kgha reaches 8730 kg/ha (2020 plot 92, Zeta 2; 7.0 g/boll, 124.7 bolls/m2), exceeding terminag's 5000 kg/ha threshold for cotton; kept as reported.
# 2019 plot 25 (Assos, HF, 0.5) has biomass_leaf_70 = 0 despite LAI = 2.95 and height = 70.6 cm; kept as reported.
# 2020 plot 66 (Stam129A, LF, 0.25) has missing anthesis_date and anthesis_DAP.
# Potassium sulphate is listed as 0-0-30 in Table 2 of the paper without specifying S or Mg content, so S_fertilizer is not computed.
# Remaining write_files() messages: invalid date(s) in carob_date (placeholder), missing variables (yield_moisture), invalid terms (yield_part: seedcotton), high crop yield (cotton: 8730), unknown variables for _-suffixed cotton/fiber/treatment traits, and contributor: NA.

## NEW VARIABLES
# fertilization_level_: categorical fertilization treatment code (HF = normal fertilization, LF = 50% reduced fertilization)
# irrigation_level_: fractional irrigation treatment level (0.25, 0.5, 0.75, 1; dimensionless)
# boll_density_: number of bolls per hectare (bolls/ha; converted from bolls/m2)
# boll_weight_: mean boll weight (g/boll)
# boll_harvest_index_: seedcotton weight divided by total boll weight (%)
# fiber_percentage_: fiber (lint) content of seedcotton, ginning outturn (%)
# fiber_moisture_: HVI fiber moisture (%)
# fiber_micronaire_: HVI fiber micronaire
# fiber_maturity_: HVI fiber maturity ratio (0-1)
# fiber_length_: HVI upper-half mean fiber length (mm)
# fiber_uniformity_: HVI fiber length uniformity (%)
# fiber_short_: HVI short fiber (%)
# fiber_strength_: HVI fiber strength (g/tex)
# fiber_elongation_: HVI fiber elongation (%)
# fiber_reflectance_: HVI fiber reflectance Rd (%)
# fiber_yellowness_: HVI fiber yellowness +b (Hunter yellowness index)
# node_number_: main-stem node count (nodes/plant; in long table)

## NOT PROCESSED
# None (all 40 columns of the Dataset sheet are processed; the Metadata sheet is the codebook).

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
		data_organization = "DIMITRA;CIRAD",
		publication = "doi:10.3390/su16010347",
		project = "TomorrowS",
		design = "split-split-plot",
		data_type = "experiment",
		treatment_vars = "variety;N_fertilizer;P_fertilizer;K_fertilizer;irrigation_amount",
		response_vars = "yield;anthesis_days;LAI;SPAD;plant_height;dmy_leaves;dmy_stems;dmy_storage",
		carob_LLM = "Gemini 4 Neon",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_completion = 100,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Cotton dataset Greece 2018 to 2020"]
	r <- carobiner::read.excel(f, sheet="Dataset", na="NA")

	# Fertilizer product rates (kg/ha) from Tsaliki et al. (2024) Table 2 (doi:10.3390/su16010347):
	# Basal (before planting): MAP (12-52-0), DAS (21-0-0), SOP (0-0-30).
	# Top-dressing (at squaring stage): granular AN (33.5-0-0).
	# HF (normal): 2018 & 2020 = 200 MAP, 300 DAS, 350 SOP, 120 AN; 2019 = 200 MAP, 200 DAS, 0 SOP, 250 AN.
	# LF (low): 50% of HF in all years.
	ffac <- ifelse(r$ferti == "HF", 1, 0.5)
	map_rate <- 200 * ffac
	das_rate <- ifelse(r$year == 2019, 200, 300) * ffac
	sop_rate <- ifelse(r$year == 2019, 0, 350) * ffac
	an_rate <- ifelse(r$year == 2019, 250, 120) * ffac

	yr_idx <- match(r$year, c(2018, 2019, 2020))

	d <- data.frame(
		trial_id = as.character(as.integer(r$year)),
		record_id = as.integer(seq_len(nrow(r))),
		plot_id = as.character(as.integer(r$plot)),
		# Six 16-plot blocks per year: plots 1-48 HF (reps 1-3), plots 49-96 LF (reps 1-3)
		rep = as.integer(((r$plot - 1) %% 48) %/% 16 + 1),
		country = "Greece",
		adm1 = "Macedonia and Thrace",
		adm2 = "Central Macedonia",
		adm3 = "Thermi",
		location = "Thermi",
		site = "Institute of Plant Breeding and Genetic Resources",
		# Coordinates from Tsaliki et al. (2024): 40°32'08.75" N, 23°00'21.47" E
		latitude = 40.53576,
		longitude = 23.00596,
		geo_from_source = TRUE,
		crop = "cotton",
		previous_crop = "wheat",
		variety = r$cultivar,
		treatment = paste(r$cultivar, r$ferti, r$irrig),
		fertilization_level_ = r$ferti,
		irrigation_level_ = as.numeric(r$irrig),
		planting_date = as.character(as.Date(r$planting_date, tz="UTC")),
		# Harvest dates from Tsaliki et al. (2024): 3 Oct 2018, 30 Sep 2019, 27 Sep 2020
		harvest_date = c("2018-10-03", "2019-09-30", "2020-09-27")[yr_idx],
		flowering_date = as.character(as.Date(r$anthesis_date, tz="UTC")),
		anthesis_days = as.numeric(r$anthesis_DAP),
		# Plot geometry from Tsaliki et al. (2024): 6 rows x 0.80 m spacing x 10 m length
		row_spacing = 80,
		plot_length = 10,
		plot_width = 4.8,
		plot_area = 48,
		plant_density = as.numeric(r$plant_density_m2) * 10000,
		fertilizer_used = TRUE,
		fertilizer_type = ifelse(r$year == 2019, "MAP;DAS;AN", "MAP;DAS;SOP;AN"),
		N_splits = 2L,
		N_fertilizer = round(map_rate * 0.12 + das_rate * 0.21 + an_rate * 0.335, 2),
		P_fertilizer = round(map_rate * 0.52 / 2.29, 2),
		K_fertilizer = round(sop_rate * 0.30 / 1.2051, 2),
		irrigated = TRUE,
		# 12 mm establishment sprinkler irrigation + 5 drip irrigations during flowering; 4000 m3/ha (400 mm) at 100%
		irrigation_method = "drip",
		irrigation_number = 6L,
		irrigation_amount = as.numeric(r$irrig) * 400,
		# Herbicides from Tsaliki et al. (2024): pendimethalin at planting, fluazifop-P-butyl at 30 DAP; no insecticides
		herbicide_used = TRUE,
		herbicide_product = "pendimethalin;fluazifop-p-butyl",
		herbicide_times = 2L,
		herbicide_timing = "0, 30",
		insecticide_used = FALSE,
		# Soil properties (0-30 cm) from Tsaliki et al. (2024) Table 1
		depth_top = 0,
		depth_bottom = 30,
		soil_texture = "silty loam",
		soil_sand = c(47.2, 48.0, 44.0)[yr_idx],
		soil_silt = c(33.6, 36.0, 38.0)[yr_idx],
		soil_clay = c(19.2, 16.0, 18.0)[yr_idx],
		soil_pH = c(7.7, 7.8, 7.6)[yr_idx],
		soil_SOM = c(0.40, 1.60, 0.58)[yr_idx],
		soil_EC = c(0.28, 0.56, 0.56)[yr_idx],
		# Reported as NO3-N (mg/kg) in Table 1
		soil_NO3 = c(4.2, 13.5, 12.2)[yr_idx],
		soil_P = c(2.8, 13.4, 3.1)[yr_idx],
		soil_K = c(71, 607, 134)[yr_idx],
		yield = as.numeric(r$seedcotton_yield_kgha),
		yield_part = "seedcotton",
		boll_density_ = as.numeric(r$boll_nb_m2) * 10000,
		boll_weight_ = as.numeric(r$boll_weight_g),
		boll_harvest_index_ = as.numeric(r$boll_harvest_index),
		fiber_percentage_ = as.numeric(r$fiber_percentage),
		fiber_moisture_ = as.numeric(r$moisture),
		fiber_micronaire_ = as.numeric(r$micronaire),
		fiber_maturity_ = as.numeric(r$maturity),
		fiber_length_ = as.numeric(r$length),
		fiber_uniformity_ = as.numeric(r$length_uniformity),
		fiber_short_ = as.numeric(r$short_fiber),
		fiber_strength_ = as.numeric(r$strength),
		fiber_elongation_ = as.numeric(r$elongation),
		fiber_reflectance_ = as.numeric(r$reflectance),
		fiber_yellowness_ = as.numeric(r$b),
		on_farm = FALSE,
		is_survey = FALSE
	)

	# g per 5 plants to kg/ha using recorded plant density (12.5 plants/m2 -> * 25)
	per5_to_kgha <- as.numeric(r$plant_density_m2) / 5 * 10

	piece <- function(dap, lai, spad, height, nodes, leaf, stem, repro) {
		data.frame(
			record_id = d$record_id,
			DAP = as.integer(dap),
			LAI = as.numeric(lai),
			SPAD = as.numeric(spad),
			plant_height = as.numeric(height),
			node_number_ = as.numeric(nodes),
			dmy_leaves = as.numeric(leaf) * per5_to_kgha,
			dmy_stems = as.numeric(stem) * per5_to_kgha,
			# Reproductive dry biomass (squares, flowers, bolls)
			dmy_storage = as.numeric(repro) * per5_to_kgha
		)
	}

	long <- rbind(
		piece(70, r$lai_70, r$spad_70, r$height_70, r$node_nb_70, r$biomass_leaf_70, r$biomass_stem_70, r$biomass_reproductive_70),
		piece(80, r$lai_80, NA, NA, NA, NA, NA, NA),
		piece(100, r$lai_100, NA, NA, NA, NA, NA, NA),
		piece(110, NA, r$spad_110, NA, NA, NA, NA, NA),
		piece(120, r$lai_120, NA, r$height_120, r$node_nb_120, r$biomass_leaf_120, r$biomass_stem_120, r$biomass_reproductive_120)
	)
	meas_cols <- c("LAI", "SPAD", "plant_height", "node_number_", "dmy_leaves", "dmy_stems", "dmy_storage")
	long <- long[rowSums(!is.na(long[, meas_cols])) > 0, ]
	rownames(long) <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
