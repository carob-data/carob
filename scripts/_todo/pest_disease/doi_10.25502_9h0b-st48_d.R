# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Neem seed oil doses ("dose 1", "dose 2", "dose 3") have no concentrations or application rates in the dataset or dictionary.
# 2. No site name or plot-level GPS is given; coordinates (3.84803 N, 11.50208 E, Yaoundé) come from CKAN coverage metadata.
# 3. Eight aphid columns (outer/inner apterous/alate turnip and cabbage aphids) only take integer values 1-5 (never 0) and IncTurnAphids/InCabAph = 0 whenever their sum equals 4, showing they are 1-5 abundance scores (1 = absent) rather than raw counts; stored in pest_severity with severity_scale = "1-5". Confirm scale definition.
# 4. Outnbre_de_chenilles_processionnaires is mapped to "Spodoptera" following the dictionary ("Number of spodopterans"), though the French column name means processionary caterpillars.
# 5. Entomopathogen counts include large spikes (100, 120, 200, 1000, 1001, 1100); left as recorded.
# 6. Two duplicate plant-number entries exist (SN 56 & 59, week 1 chou dose 3 bloc 2 plant 1; SN 1561 & 1571, week 10 chou dose 1 bloc 4 plant 1); 29 rows in kale control block 4 have NA for all counts.

## NEW
# insecticide_dose_: neem seed oil dose level ("dose 1", "dose 2", "dose 3", "none"; concentration not reported)
# plant_number_: sampled plant index within plot (1-5)
# leaf_number_: number of leaves per sampled plant (nbre_de_feuilles)
# leaf_position_: leaf position sampled ("outer" or "inner")
# insect_stage_: life stage or observation type ("apterous", "alate", "larva", "pupa", "damage")
# organism_role_: ecological role ("pest", "predator", "pathogen")
# pest_species values not in terminag: Hellula, Plutella, turnip aphid, cabbage aphid, Spodoptera, snail, looper, entomopathogen

## MISSING
# yield, yield_moisture, irrigated, planting_date, harvest_date, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Weekly plant-level insect pest, predator, and damage observations on cabbage (chou) and kale
#   in Cameroon (22 Nov 2016 to 18 Feb 2017; 4 blocks x 4 treatments x 2 crops x 5 plants x 13 weeks = 2080 rows).
# Each row of d is one sampled plant on one observation date; dd (long) holds all outer/inner leaf pest
#   counts, aphid abundance scores, damage scores, and predator counts linked by record_id.
# engrais is "fiente" (poultry manure) on every row without a rate. Growth stage was recorded for cabbage only.

## NOT PROCESSED
# TotalHelula, TotPlutel, TotMchBlch, TotArrreign, TotSyrph, TotChenille, TotChP, TotEsc, TotEntomoP,
#   IncTotTurnAphids, InTotCabAph, IncTotHelul, IncTotPlut: sums of the outer and inner columns already stored in dd.
# IncTurnAphids, InCabAph, IncHelul, IncPlut: 0/1 presence flags derived from the counts/scores in dd.


carob_script <- function(path) {

"
Effect of neem seed oil at different doses on insect pests of cabbage

Prelinimary testing of plant extracts as insecticides in organic cultivation
"

	uri <- "doi:10.25502/9h0b-st48/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Enhancing horticultural productivity, incomes and livelihoods through integrated management of aphid pests on vegetables in sub-Saharan Africa",
		design = "RCBD; 4 blocks; 3 neem seed oil doses and untreated control on cabbage and kale; 5 plants per plot sampled weekly",
		data_type = "experiment",
		treatment_vars = "insecticide_used;insecticide_dose_",
		response_vars = "pest_incidence;pest_severity;insect_abundance",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "effect-of-neem-seed-oil-at-different-doses-on-insect-pests-of-cabbage.csv"]
	r <- read.csv(f, check.names = FALSE, fileEncoding = "latin1")
	stopifnot(names(r)[4] == "culture", names(r)[8] == "bloc", ncol(r) == 56)

	crop <- trimws(r$culture)
	crop[crop == "chou"] <- "cabbage"

	stage <- tolower(trimws(r$stade_dvlpt))
	growth <- rep(NA_character_, length(stage))
	growth[grepl("^v", stage)] <- "vegetative"
	growth[grepl("^m", stage)] <- "mature"
	growth[grepl("format", stage)] <- "head formation"

	# Normalize accented "témoin" to "control"
	pest <- tolower(trimws(r$pesticides))
	pest[grepl("^t.*moin$", pest)] <- "control"

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = "1",
		plot_id = paste(crop, gsub(" ", "_", pest), r$bloc, sep = "_"),
		country = "Cameroon",
		adm1 = "Centre",
		latitude = 3.84803,
		longitude = 11.50208,
		geo_from_source = TRUE,
		date = as.character(as.Date(r[["date_d'obs"]], format = "%d/%m/%Y")),
		order = as.integer(r$semaine),
		crop = crop,
		growth_stage = growth,
		treatment = ifelse(pest == "control", "control", paste("neem seed oil", pest)),
		insecticide_used = pest != "control",
		# azadirachtin is the active ingredient of neem seed oil
		insecticide_product = ifelse(pest != "control", "azadirachtin", "none"),
		insecticide_dose_ = ifelse(pest == "control", "none", pest),
		block_id = as.character(r$bloc),
		rep = as.integer(r$bloc),
		plant_number_ = as.integer(r$no_de_plante),
		leaf_number_ = as.integer(r$nbre_de_feuilles),
		yield_part = "leaves",
		OM_used = TRUE,
		OM_type = "poultry manure",
		on_farm = NA,
		is_survey = FALSE,
		stress = "pests"
	)

	make_pest <- function(i, species, position, stage, role, what, scale = NA_character_) {
		v <- as.integer(r[[i]])
		keep <- !is.na(v)
		n <- sum(keep)
		data.frame(
			record_id = d$record_id[keep],
			pest_species = if (role == "predator") rep(NA_character_, n) else rep(species, n),
			insect_species = if (role == "predator") rep(species, n) else rep(NA_character_, n),
			leaf_position_ = rep(position, n),
			insect_stage_ = rep(stage, n),
			organism_role_ = rep(role, n),
			pest_incidence = if (what == "count") v[keep] else rep(NA_integer_, n),
			pest_severity = if (what == "severity") as.character(v[keep]) else rep(NA_character_, n),
			severity_scale = if (what == "severity") rep(scale, n) else rep(NA_character_, n),
			insect_abundance = if (what == "predator") as.character(v[keep]) else rep(NA_character_, n)
		)
	}

	# Columns 11-13 are plant-level damage scores; 14-17 & 28-31 are 1-5 aphid abundance scores;
	# remaining columns 18-27 & 32-39 are individual counts on outer/inner leaves.
	dd <- rbind(
		make_pest(11, "Hellula", NA_character_, "damage", "pest", "severity", "1-5"),
		make_pest(12, "Plutella", NA_character_, "damage", "pest", "severity", "1-5"),
		make_pest(13, "aphid", NA_character_, "damage", "pest", "severity", "1-5"),
		make_pest(14, "turnip aphid", "outer", "apterous", "pest", "severity", "1-5"),
		make_pest(15, "turnip aphid", "outer", "alate", "pest", "severity", "1-5"),
		make_pest(16, "cabbage aphid", "outer", "apterous", "pest", "severity", "1-5"),
		make_pest(17, "cabbage aphid", "outer", "alate", "pest", "severity", "1-5"),
		make_pest(18, "Plutella", "outer", "larva", "pest", "count"),
		make_pest(19, "Plutella", "outer", "pupa", "pest", "count"),
		make_pest(20, "Hellula", "outer", "larva", "pest", "count"),
		make_pest(21, "whitefly", "outer", NA_character_, "pest", "count"),
		make_pest(22, "spider", "outer", NA_character_, "predator", "predator"),
		make_pest(23, "syrphid", "outer", "larva", "predator", "predator"),
		make_pest(24, "Spodoptera", "outer", "larva", "pest", "count"),
		make_pest(25, "snail", "outer", NA_character_, "pest", "count"),
		make_pest(26, "looper", "outer", "larva", "pest", "count"),
		make_pest(27, "entomopathogen", "outer", NA_character_, "pathogen", "count"),
		make_pest(28, "turnip aphid", "inner", "apterous", "pest", "severity", "1-5"),
		make_pest(29, "turnip aphid", "inner", "alate", "pest", "severity", "1-5"),
		make_pest(30, "cabbage aphid", "inner", "apterous", "pest", "severity", "1-5"),
		make_pest(31, "cabbage aphid", "inner", "alate", "pest", "severity", "1-5"),
		make_pest(32, "Plutella", "inner", "larva", "pest", "count"),
		make_pest(33, "Plutella", "inner", "pupa", "pest", "count"),
		make_pest(34, "Hellula", "inner", "larva", "pest", "count"),
		make_pest(35, "whitefly", "inner", NA_character_, "pest", "count"),
		make_pest(36, "spider", "inner", NA_character_, "predator", "predator"),
		make_pest(37, "syrphid", "inner", "larva", "predator", "predator"),
		make_pest(38, "looper", "inner", "larva", "pest", "count"),
		make_pest(39, "entomopathogen", "inner", NA_character_, "pathogen", "count")
	)

	carobiner::write_files(path, meta, d, long = dd)
}
