# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Coordinates are estimated from Dar es Salaam (data dictionary coverage), since CKAN coverage_x/y (-6.52471, 35.78784) is Tanzania's country centroid. Confirm the exact trial station (e.g., Chambezi/Bagamoyo vs Dar es Salaam) and planting/harvest dates, variety, and plant spacing from Issa et al. (2025, doi:10.1111/jen.70046).
# 2023-whitefly-virus.csv Month is coded 1-6 without a "MAP" suffix; treated as months_after_planting_ (1-6) matching 2024 ("1MAP"-"6MAP") — confirm from the paper.
# Root_weight is fresh weight in grams per plant with no plant spacing in the raw files, so it cannot yet be converted to yield (kg/ha).

## NEW
# trial_year_: trial year (2023 or 2024).
# plant_number_: individual plant number (1-12) within each block x treatment plot.
# root_number_: number of harvested fresh cassava roots per plant (count/plant).
# root_weight_: fresh cassava root weight per plant (g/plant).
# months_after_planting_: observation time in months after planting (1-6 months) in the long table.
# stage_: whitefly life stage (adult or nymph) in the long table.
# pest_species value not in terminag: Bemisia tabaci.

## MISSING
# yield, yield_moisture, N_fertilizer, P_fertilizer, K_fertilizer, irrigated, planting_date, harvest_date.

## NOTES
# Two-year cassava whitefly (Bemisia tabaci) management trials (2023 and 2024; 4 blocks, 12 plants/plot) evaluating neem oil foliar sprays (TopNeem Ultra, crude Ethanol Neem) and Mandiplus cutting dip (thiamethoxam + fludioxonil + metalaxyl-M, per Issa et al. 2025, doi:10.1111/jen.70046).
# In 2024-whitefly-virus.csv, the Block and Treatment headers are swapped relative to the other three files (Block holds the treatment name and Treatment holds block 1-4); corrected here.
# All 384 individual plants (144 in 2023, 240 in 2024) are preserved in d with per-plant root harvest data, linked via record_id to long table dd holding monthly adult/nymph counts and 1-5 CMD/CBSD severity scores.

## NOT PROCESSED
# data_dictionary.csv: column documentation only.


carob_script <- function(path) {

"
Efficacy of botanical oils and cutting dipping in insecticides for control of cassava Bemisia tabaci whitefly

The project evaluated efficacy of crude formulation of neem oil, commercial neem oil in combination with cutting dipping in insecticides solutions for control of whiteflies in cassava.
"

	uri <- "doi:10.25502/34f2-0h91/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; Keele",
		publication = "doi:10.1111/jen.70046",
		project = "Novel botanical formulations for control of virus vectors",
		design = "RCBD; 4 blocks; neem oil foliar sprays and insecticide/fungicide cutting dip (Mandiplus)",
		data_type = "experiment",
		treatment_vars = "insecticide_used;insecticide_product;fungicide_used;fungicide_product;seed_treatment",
		response_vars = "pest_incidence;disease_severity;root_weight_;root_number_",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "2023-whitefly-virus.csv"]
	f2 <- ff[basename(ff) == "2023-root-yield.csv"]
	f3 <- ff[basename(ff) == "2024-whitefly-virus.csv"]
	f4 <- ff[basename(ff) == "2024-root-yield.csv"]
	r1 <- read.csv(f1, check.names=FALSE)
	r2 <- read.csv(f2, check.names=FALSE)
	r3 <- read.csv(f3, check.names=FALSE)
	r4 <- read.csv(f4, check.names=FALSE)

	align_trt <- function(x) {
		x <- trimws(x)
		x[tolower(x) == "control"] <- "control"
		x[x == "TopNeem"] <- "TopNeem Ultra"
		x[x %in% c("EthaNeem", "EthanolNeem", "Ethanol-Neem")] <- "Ethanol Neem"
		x[x == "Mandiplus+Ethanol-Neem"] <- "Mandiplus & Ethanol Neem"
		x
	}

	make_plants <- function(y, year) {
		trt <- align_trt(y$Treatment)
		# Active ingredients per Issa et al. (2025, doi:10.1111/jen.70046):
		# TopNeem Ultra / Ethanol Neem = neem (margosa) oil foliar spray;
		# Mandiplus cutting dip = thiamethoxam + fludioxonil + metalaxyl-M (mapped to metalaxyl)
		has_neem <- trt %in% c("TopNeem Ultra", "Ethanol Neem", "Mandiplus & Ethanol Neem")
		has_mandi <- trt %in% c("Mandiplus", "Mandiplus & Ethanol Neem")
		ins_prod <- ifelse(has_mandi & has_neem, "thiamethoxam;margosa oil",
			ifelse(has_mandi, "thiamethoxam",
				ifelse(has_neem, "margosa oil", "none")))
		data.frame(
			trial_year_ = as.integer(year),
			trial_id = paste("Dar_es_Salaam", year, sep="_"),
			block_id = as.character(y$Block),
			treatment = trt,
			plant_number_ = as.integer(y$Plant),
			insecticide_used = has_neem | has_mandi,
			insecticide_product = ins_prod,
			fungicide_used = has_mandi,
			fungicide_product = ifelse(has_mandi, "fludioxonil;metalaxyl", "none"),
			seed_treatment = ifelse(has_mandi, "Mandiplus (thiamethoxam; fludioxonil; metalaxyl-M) cutting dip", "none"),
			root_number_ = as.integer(y$Root_numbers),
			root_weight_ = as.numeric(y$Root_weight)
		)
	}

	d <- rbind(make_plants(r2, 2023), make_plants(r4, 2024))
	d <- d[order(d$trial_year_, d$block_id, d$treatment, d$plant_number_), ]
	rownames(d) <- NULL
	d$record_id <- seq_len(nrow(d))
	d$crop <- "cassava"
	d$yield_part <- "roots"
	d$yield_isfresh <- TRUE
	d$country <- "Tanzania"
	d$location <- "Dar es Salaam"
	# Georeferenced to Dar es Salaam (data dictionary coverage); 20 km city-level uncertainty
	d$latitude <- -6.7924
	d$longitude <- 39.2083
	d$geo_from_source <- FALSE
	d$geo_uncertainty <- 20000
	d$geo_source <- "Dar es Salaam (data dictionary coverage)"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$stress <- "pests;disease"

	key_d <- paste(d$trial_year_, d$block_id, d$treatment, d$plant_number_, sep="|")

	make_long <- function(w, year, swapped) {
		trt <- align_trt(if (swapped) w$Block else w$Treatment)
		blk <- as.character(if (swapped) w$Treatment else w$Block)
		plt <- as.integer(w$Rep)
		map_mo <- as.integer(sub("MAP$", "", trimws(as.character(w$Month))))
		rid <- d$record_id[match(paste(year, blk, trt, plt, sep="|"), key_d)]
		stopifnot(!any(is.na(rid)))

		a <- data.frame(
			record_id = rid,
			months_after_planting_ = map_mo,
			pest_species = "Bemisia tabaci",
			stage_ = "adult",
			pest_incidence = as.integer(w$Whitefly_number),
			disease = NA_character_,
			disease_severity = NA_character_,
			severity_scale = NA_character_
		)
		n <- data.frame(
			record_id = rid,
			months_after_planting_ = map_mo,
			pest_species = "Bemisia tabaci",
			stage_ = "nymph",
			pest_incidence = as.integer(w$Nymph_number),
			disease = NA_character_,
			disease_severity = NA_character_,
			severity_scale = NA_character_
		)
		cmd <- data.frame(
			record_id = rid,
			months_after_planting_ = map_mo,
			pest_species = NA_character_,
			stage_ = NA_character_,
			pest_incidence = NA_integer_,
			disease = "cassava mosaic",
			disease_severity = as.character(w$CMD_severity),
			severity_scale = "1-5"
		)
		cbsd <- data.frame(
			record_id = rid,
			months_after_planting_ = map_mo,
			pest_species = NA_character_,
			stage_ = NA_character_,
			pest_incidence = NA_integer_,
			disease = "cassava brown streak",
			disease_severity = as.character(w$CBSD_severity),
			severity_scale = "1-5"
		)
		rbind(a, n, cmd, cbsd)
	}

	# 2024 whitefly file has Block and Treatment headers swapped
	dd <- rbind(make_long(r1, 2023, swapped=FALSE), make_long(r3, 2024, swapped=TRUE))

	carobiner::write_files(path, meta, d, long=dd)
}
