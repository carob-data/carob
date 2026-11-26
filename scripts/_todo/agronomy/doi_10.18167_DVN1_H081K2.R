# R script for "carob"
# license: GPL (>=3)

## NOTES
# Agronomic trial (B2A Project: Banane Agroécologique pour l'Afrique) on 8 plantain varieties x 2 irrigation regimes (Ir/Ni) x 3 fertilization regimes (0/M/P) at Akoupé-Zeudji, Lagunes, Côte d'Ivoire (planted 2023-05-16 and 2023-05-18; cycles 1-3 observed through Jan 2025).
# Coordinates (5.483 N, -4.150 E) are estimated for Akoupé-Zeudji village (geo_from_source = FALSE, geo_uncertainty = 5000 m); verify exact trial site if available.
# Fertilization codes (from INoVar_Dataverse_data meanings.csv, g/tree/month, KCl from month 5): 0 = 0 g NPK + 100 g KCl; M = 25 g NPK + 85 g KCl; P = 75 g NPK + 50 g KCl. Because NPK grade and planting density are not stated, N/P/K_fertilizer (kg/ha) cannot be computed; regimes and per-tree doses are stored explicitly.
# Bunch weight is reported in kg/plant without planting density, so per-ha yield cannot be computed and is stored as bunch_weight_ (kg/plant) in the long table.
# Wide table d has one row per plantain tree (1,152 trees, including 19 dead trees with all observations NA per meanings file); long table dlong stores all stage-dated measurements across Cycle 1 (first_leaf, flowering, sigatoka, harvest), Cycle 2 (follower, flowering, harvest), and Cycle 3 (follower) linked via record_id.
# flowering_days and harvest_days are computed only for Cycle 1 (days since planting); Cycle 2 ratoon dates are recorded in flowering_date and harvest_date without subtracting Cycle 1 planting_date.

## ISSUES
# Planting density and NPK formulation are not reported, so yield (kg/ha) and N/P/K_fertilizer (kg/ha) cannot be computed (triggers missing variable messages for yield, yield_part, yield_moisture, N/P/K_fertilizer).
# Plantain Cycle 1 flowering (172-617 days) and harvest (256-616 days) naturally exceed terminag's annual-crop bounds (250 and 365 days), and initial Cycle 2/3 follower heights include 0 cm (20 plants with no emerged follower yet) and 9 cm (< 10 cm bound). Left intact as measured.
# Coordinates are village-level estimates for Akoupé-Zeudji; author should verify exact field coordinates.

## NEW VARIABLES
# ridge_: ridge identification number separating deep ditches
# plant_number_: plant replicate index (1-3) within variety x treatment x ridge
# fertiliser_regime_: fertilization treatment code (0, M, or P)
# npk_g_tree_month_: NPK product applied per tree per month (g/tree/month)
# kcl_g_tree_month_: KCl applied per tree per month from the 5th month (g/tree/month)
# cycle_: crop cycle number (1, 2, or 3)
# stage_: phenological/measurement stage (first_leaf, follower, flowering, sigatoka, harvest)
# pseudostem_girth50_: pseudostem girth 50 cm above corm (cm)
# pseudostem_girth100_: pseudostem girth 100 cm above corm (cm)
# emitted_leaves_: cumulative number of emitted leaves
# living_leaves_: number of living (green) leaves at flowering or harvest
# leaf_length_: length of first leaf after planting (stage = first_leaf) or last emitted leaf at harvest (cm)
# leaf_width_: width of first leaf after planting (stage = first_leaf) or last emitted leaf at harvest (cm)
# leaf3_length_: length of third-last emitted leaf at harvest (cm)
# leaf3_width_: width of third-last emitted leaf at harvest (cm)
# bunch_weight_: fresh bunch weight per plant (kg/plant)
# hands_: number of hands per bunch
# fingers_: number of fingers per bunch
# finger_grade_: caliper grade of the middle finger of the second hand (cm)
# finger_girth_: girth of the middle finger of the second hand (cm)
# finger_length_: length of the middle finger of the second hand (cm)
# sigatoka_leaf4_: number of black sigatoka lesions on the 4th leaf
# sigatoka_leaf5_: number of black sigatoka lesions on the 5th leaf
# sigatoka_leaf6_: number of black sigatoka lesions on the 6th leaf
# sigatoka_leaf6_rank_: emitted leaf rank corresponding to the 6th leaf assessed for black sigatoka
# youngest_necrotic_leaf_: rank of the youngest leaf presenting necrotic tissue (YLS)
# villardebo_new_galleries_: Villardebo weevil damage index counting only new galleries at harvest

## NOT PROCESSED
# INoVar_Dataverse.csv: Name of the observer (administrative metadata).
# INoVar_Dataverse_data meanings.csv: codebook/data dictionary, not an observation table.


carob_script <- function(path) {

"
Essai agronomique fertilisation et irrigation de 8 variétés de bananiers plantain en Côte d'Ivoire, 2023 - 2025

Essai agronomique visant à déterminer les variétés les plus adaptées aux conditions de culture du producteur. Données de croissance, de rendement et de dégâts de charançons (Villardebo) de 8 variétés de bananiers plantain sur deux cycles de culture. Cet essai inclus 3 modalités de fertilisation azoté et 2 modalités d'irrigation.

An agronomic trial designed to identify the varieties best suited to the grower's cultivation conditions. Data on growth, yield, and weevil damage (Villardebo) for 8 plantain varieties over two growing cycles. This trial included 3 nitrogen fertilization treatments and 2 irrigation treatments.
"

	uri <- "doi:10.18167/DVN1/H081K2"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "B2A",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;irrigated;fertiliser_regime_",
		response_vars = "plant_height;bunch_weight_;SPAD;pest_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "INoVar_Dataverse.csv"]
	r <- read.csv(f, sep=";", na.strings=c("NA", ""), check.names=FALSE)
	names(r) <- gsub("\\s+", " ", trimws(names(r)))

	vname <- c(BA="Batard", BE="Big Ebanga", BC="Blanche Créole", CE="Curare Enano",
		DH="Dominico Harton", MP="MP1", MB="Mbouroukou 3", OR="Orishele")
	# From meanings file (g/tree/month; KCl from 5th month)
	npk <- c("0"=0, "M"=25, "P"=75)
	kcl <- c("0"=100, "M"=85, "P"=50)
	fert <- trimws(as.character(r$fertilisation))

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = "Akoupe_2023",
		plot_id = as.character(r[["Unique ID per banana tree"]]),
		ridge_ = as.integer(r[["Ridge number"]]),
		plant_number_ = as.integer(r[["plant number"]]),
		variety = unname(vname[trimws(as.character(r[["Plantain variety"]]))]),
		treatment = paste(unname(vname[trimws(as.character(r[["Plantain variety"]]))]), r$irrigation, fert),
		fertiliser_regime_ = fert,
		fertilizer_used = TRUE,
		fertilizer_type = ifelse(fert == "0", "KCl", "NPK;KCl"),
		npk_g_tree_month_ = unname(npk[fert]),
		kcl_g_tree_month_ = unname(kcl[fert]),
		irrigated = r$irrigation == "Ir",
		planting_date = as.character(as.Date(r[["Plantation date"]])),
		country = "Côte d'Ivoire",
		adm1 = "Lagunes",
		location = "Akoupé-Zeudji",
		# Estimated village coordinates for Akoupé-Zeudji, Côte d'Ivoire (approx. 5°29'N, 4°09'W)
		longitude = -4.150,
		latitude = 5.483,
		geo_from_source = FALSE,
		geo_uncertainty = 5000,
		crop = "plantain",
		stress = "pests;disease",
		on_farm = TRUE,
		is_survey = FALSE
	)

	num_col <- function(nm) {
		if (!nzchar(nm) || !nm %in% names(r)) return(rep(NA_real_, nrow(r)))
		x <- gsub(",", ".", trimws(as.character(r[[nm]])), fixed=TRUE)
		x[x %in% c("", "NA")] <- NA_character_
		as.numeric(x)
	}
	date_col <- function(nm) {
		if (!nzchar(nm) || !nm %in% names(r)) return(rep(NA_character_, nrow(r)))
		as.character(as.Date(trimws(as.character(r[[nm]]))))
	}
	chr_col <- function(nm) {
		if (!nzchar(nm) || !nm %in% names(r)) return(rep(NA_character_, nrow(r)))
		x <- trimws(as.character(r[[nm]]))
		x[x %in% c("", "NA")] <- NA_character_
		x
	}

	n <- nrow(r)
	mk <- function(cycle_, stage_, date = rep(NA_character_, n),
			flowering_date = rep(NA_character_, n), harvest_date = rep(NA_character_, n),
			flowering_days = rep(NA_real_, n), harvest_days = rep(NA_real_, n),
			plant_height = rep(NA_real_, n), pseudostem_girth50_ = rep(NA_real_, n),
			pseudostem_girth100_ = rep(NA_real_, n), emitted_leaves_ = rep(NA_real_, n),
			living_leaves_ = rep(NA_real_, n), leaf_length_ = rep(NA_real_, n),
			leaf_width_ = rep(NA_real_, n), leaf3_length_ = rep(NA_real_, n),
			leaf3_width_ = rep(NA_real_, n), SPAD = rep(NA_real_, n),
			bunch_weight_ = rep(NA_real_, n), hands_ = rep(NA_real_, n),
			fingers_ = rep(NA_real_, n), finger_grade_ = rep(NA_real_, n),
			finger_girth_ = rep(NA_real_, n), finger_length_ = rep(NA_real_, n),
			pest_severity = rep(NA_character_, n), villardebo_new_galleries_ = rep(NA_real_, n),
			sigatoka_leaf4_ = rep(NA_real_, n), sigatoka_leaf5_ = rep(NA_real_, n),
			sigatoka_leaf6_ = rep(NA_real_, n), sigatoka_leaf6_rank_ = rep(NA_real_, n),
			youngest_necrotic_leaf_ = rep(NA_real_, n)) {
		out <- data.frame(
			record_id = d$record_id,
			trial_id = d$trial_id,
			plot_id = d$plot_id,
			cycle_ = as.integer(cycle_),
			stage_ = as.character(stage_),
			date = date,
			flowering_date = flowering_date,
			harvest_date = harvest_date,
			flowering_days = flowering_days,
			harvest_days = harvest_days,
			plant_height = plant_height,
			pseudostem_girth50_ = pseudostem_girth50_,
			pseudostem_girth100_ = pseudostem_girth100_,
			emitted_leaves_ = emitted_leaves_,
			living_leaves_ = living_leaves_,
			leaf_length_ = leaf_length_,
			leaf_width_ = leaf_width_,
			leaf3_length_ = leaf3_length_,
			leaf3_width_ = leaf3_width_,
			SPAD = SPAD,
			bunch_weight_ = bunch_weight_,
			hands_ = hands_,
			fingers_ = fingers_,
			finger_grade_ = finger_grade_,
			finger_girth_ = finger_girth_,
			finger_length_ = finger_length_,
			pest_severity = pest_severity,
			villardebo_new_galleries_ = villardebo_new_galleries_,
			sigatoka_leaf4_ = sigatoka_leaf4_,
			sigatoka_leaf5_ = sigatoka_leaf5_,
			sigatoka_leaf6_ = sigatoka_leaf6_,
			sigatoka_leaf6_rank_ = sigatoka_leaf6_rank_,
			youngest_necrotic_leaf_ = youngest_necrotic_leaf_
		)
		val_cols <- setdiff(names(out), c("record_id", "trial_id", "plot_id", "cycle_", "stage_"))
		keep <- rowSums(!is.na(out[, val_cols])) > 0
		out[keep, ]
	}

	pd <- as.Date(d$planting_date)
	c1_fl <- date_col("Cycle 1 flowering date")
	c1_hv <- date_col("Cycle 1 date of harvest")
	c2_fl <- date_col("Cycle 2 flowering date")
	c2_hv <- date_col("Cycle 2 date of harvest")

	dlong <- rbind(
		# Cycle 1: first leaf
		mk(1, "first_leaf",
			date = date_col("Cycle 1 apparition date of the first leaf"),
			leaf_length_ = num_col("Cycle 1 length of first leaf after planting"),
			leaf_width_ = num_col("Cycle 1 width of first leaf after planting")),
		# Cycle 1: flowering
		mk(1, "flowering",
			date = c1_fl,
			flowering_date = c1_fl,
			flowering_days = as.numeric(as.Date(c1_fl) - pd),
			plant_height = num_col("Cycle 1 height of plantain tree at flowering"),
			pseudostem_girth50_ = num_col("Cycle 1 pseudostem girth 50cm above corm at flowering"),
			pseudostem_girth100_ = num_col("Cycle 1 pseudostem girth 100cm above corm at flowering"),
			emitted_leaves_ = num_col("Cycle 1 number of emitted leaves at flowering"),
			living_leaves_ = num_col("Cycle 1 number of leaving leaves at flowering"),
			SPAD = num_col("Cycle 1 chlorophyll content (SPAD) at flowering")),
		# Cycle 1: punctual Black Sigatoka observation
		mk(1, "sigatoka",
			date = date_col("Date of Black Sigatoka punctual observation"),
			sigatoka_leaf4_ = num_col("Cycle 1 number of black sigatoka lesion of the fourth leaf"),
			sigatoka_leaf5_ = num_col("Cycle 1 number of black sigatoka lesion of the fifth leaf"),
			sigatoka_leaf6_ = num_col("Cycle 1 number of black sigatoka lesion of the sixth leaf"),
			sigatoka_leaf6_rank_ = num_col("Cycle 1 rank of the sixth leaf"),
			youngest_necrotic_leaf_ = num_col("Cycle 1 Youngest leave presenting necrotic tissues")),
		# Cycle 1: harvest
		mk(1, "harvest",
			date = c1_hv,
			harvest_date = c1_hv,
			harvest_days = as.numeric(as.Date(c1_hv) - pd),
			leaf_length_ = num_col("Cycle 1 length of the last emitted leaf"),
			leaf_width_ = num_col("Cycle 1 width of the last emitted leaf"),
			leaf3_length_ = num_col("Cycle 1 length of the third-last emitted leaf"),
			leaf3_width_ = num_col("Cycle 1 width of the third-last emitted leaf"),
			bunch_weight_ = num_col("Cycle 1 weight of the bunch"),
			hands_ = num_col("Cycle 1 number of hands of the bunch"),
			fingers_ = num_col("Cycle 1 number of fingers of the bunch"),
			finger_grade_ = num_col("Cycle 1 grade of the middle finger of the second hand"),
			finger_girth_ = num_col("Cycle 1 girth of the middle finger of the second hand"),
			finger_length_ = num_col("Cycle 1 length of the middle finger of the second hand"),
			living_leaves_ = num_col("Cycle 1 number of leaving leaves at harvest"),
			pest_severity = chr_col("Cycle 1 Villardebo index at harvest"),
			villardebo_new_galleries_ = num_col("Cycle 1 Villardebo index of new galeries at harvest")),
		# Cycle 2: initial follower measurement (generally at Cycle 1 harvest)
		mk(2, "follower",
			date = date_col("Cycle 2 First date of measure for cycle 2"),
			plant_height = num_col("Cycle 2 heigth at measuring date"),
			pseudostem_girth50_ = num_col("Cycle 2 pseudostem girth 50cm above corm at measuring date"),
			pseudostem_girth100_ = num_col("Cycle 2 pseudostem girth 100cm above corm at measuring date"),
			emitted_leaves_ = num_col("Cycle 2 number of emitted leaves at measuring date")),
		# Cycle 2: flowering
		mk(2, "flowering",
			date = c2_fl,
			flowering_date = c2_fl,
			plant_height = num_col("Cycle 2 height of plantain tree at flowering"),
			pseudostem_girth50_ = num_col("Cycle 2 pseudostem girth 50cm above corm at flowering"),
			pseudostem_girth100_ = num_col("Cycle 2 pseudostem girth 100cm above corm at flowering"),
			emitted_leaves_ = num_col("Cycle 2 number of emitted leaves at flowering"),
			living_leaves_ = num_col("Cycle 2 number of living leaves at flowering")),
		# Cycle 2: harvest
		mk(2, "harvest",
			date = c2_hv,
			harvest_date = c2_hv,
			leaf_length_ = num_col("Cycle 2 length of the last emitted leaf"),
			leaf_width_ = num_col("Cycle 2 width of the last emitted leaf"),
			leaf3_length_ = num_col("Cycle 2 length of the third-last emitted leaf"),
			leaf3_width_ = num_col("Cycle 2 width of the third-last emitted leaf"),
			bunch_weight_ = num_col("Cycle 2 weight of the bunch"),
			hands_ = num_col("Cycle 2 number of hands of the bunch"),
			fingers_ = num_col("Cycle 2 number of fingers of the bunch"),
			finger_grade_ = num_col("Cycle 2 grade of the middle finger of the second hand"),
			finger_girth_ = num_col("Cycle 2 girth of the middle finger of the second hand"),
			finger_length_ = num_col("Cycle 2 length of the middle finger of the second hand"),
			living_leaves_ = num_col("Cycle 2 number of leaving leaves at harvest"),
			pest_severity = chr_col("Cycle 2 Villardebo index at harvest"),
			villardebo_new_galleries_ = num_col("Cycle 2 Villardebo index of new galeries at harvest")),
		# Cycle 3: initial follower measurement
		mk(3, "follower",
			date = date_col("Cycle 3 First date of measure for cycle 2"),
			plant_height = num_col("Cycle 3 heigth at measuring date"),
			pseudostem_girth50_ = num_col("Cycle 3 pseudostem girth 50cm above corm at measuring date"),
			pseudostem_girth100_ = num_col("Cycle 3 pseudostem girth 100cm above corm at measuring date"),
			emitted_leaves_ = num_col("Cycle 3 number of emitted leaves at measuring date"))
	)

	carobiner::write_files(path, meta, d, long=dlong)
}
