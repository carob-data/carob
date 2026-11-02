# R script for "carob"
# license: GPL (>=3)

## NOTES
# HumidTropics / CIALCA household baseline, Rwanda. Plot files were read as latin1.
# Production is taken from input_use.csv (module with no season name) and input_short_rainy_season.csv (season "short rains").
# Survey window in the dataset metadata is 2014-09-17 to 2014-11-24. Household start dates in November 2014 are month/day/year (some days are 14-15, so the month is November). Those dates are not planting dates.
# Country code 3 in the plot files is Rwanda in hh_compostion_characteristic.csv and in the dataset metadata.
# Empty crop-roster rows (area, both harvests, both fertilizers, organic input, and seed all 0 or missing) were dropped.
# crop_area_ha / Crops_area of 0 is treated as not recorded: many of those rows still have a harvest in kg. field_size is NA in that case. Yield (kg/ha) is harvest kg / field_size only when area is > 0.
# Where both a dry and a fresh harvest are > 0 (common for beans and maize), yield uses the dry kg and yield_isfresh is FALSE. The fresh kg is kept in harvest_fresh_kg_. Moisture was not recorded.
# DAP is 18% N and 20.1% P, urea is 46% N, MAP is 11% N and 26.9% P (terminag values_fertilizer_type). NPK has no grade in the file, so N/P/K are NA when NPK or an unnamed fertilizer was applied. "No fertilizer" with quantity 0 is a rate of 0.
# "Animal manure" is stored as farmyard manure. Organic quantity is kg on the plot (metadata); OM_amount is kg/ha only when area is > 0.
# Place names come from household_identifies.csv (the plot files store numeric codes). The most frequent cleaned name for each village code is used. adm1 is the province of that district (Eastern Province: Bugesera, Gatsibo, Kayonza, Kirehe, Nyagatare; Western Province: Karongi, Nyabihu, Rubavu).
# Household-head category in HH_type is stored as farmer_gender (male / female). "Sother" is left missing.
# No Burundi CIALCA _todo script was present to copy; this draft uses the same two plot-input modules that file would have.

## ISSUES
# No household coordinates. Do not use the dataset JSON point (coverage_x -1.94028, coverage_y 29.87388); it is one coverage coordinate. Georeference districts Bugesera, Gatsibo, Karongi, Kayonza, Kirehe, Nyabihu, Nyagatare, Rubavu, and the sector and village names. Some village codes have more than one sector name, or only a numeric name; the modal cleaned name was kept and the rest are missing.
# input_use.csv is not labelled long rains or season A/B. It is not coded as "long rains" here. Confirm which Rwanda season it is (A is about September-February, B about March-June).
# Area is 0 or missing on most plot rows, so yield is missing even when harvest kg is present (see harvest_dry_kg_ and harvest_fresh_kg_). A few areas are very large (beans 35 ha, soybean 75 ha). Recorded yields include 0 and values up to 1500000 kg/ha (maize 1500000 kg on 1 ha; fodder 2000000 kg fresh on 2 ha). Left as recorded.
# The low-yield warning for millet, pineapple, and timber is not a tons-to-kg conversion: the metadata says harvest is kg (millet about 12 kg/ha; timber 10-80 kg/ha).
# Compost quantity reaches 900000 kg and animal manure 30000 kg on a plot. OM_amount per ha is then extreme where area is small. Left as recorded.
# A few "No fertilizer" rows have a quantity above 0. Nutrients are NA on those rows.
# Percent_area was not used to split intercrop area (monocrop values include 0, 100, and 1000).
# Crop "others" has no species name (crop left NA). Millet was not recoded to finger millet. Groundnut yield_part is seed; the file does not say whether the kg are shelled.
# Pesticide and herbicide quantities are "kg or litres" with no product name, so they are not converted to kg/ha. herbicide_used is only the yes/no from a positive quantity.
# Fertilizer "Others" specify-fields are numbers, not product names.
# Citation in the metadata, without a DOI: Dontsop-Nguezet et al., Food and Energy Security, 2020, pages 1-19. publication left NA.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude.
# write_files: unknown variables season_module_, cropping_system_, intercrop_percent_, organic_kg_, seed_kg_, pesticide_used_, residue_use_, harvest_dry_kg_, harvest_fresh_kg_.
# write_files: invalid OM_type "Crop residues", "Household refuse", "Natural fallow" (kept as recorded). Animal manure was mapped to farmyard manure; Compost to compost; Others to unknown.
# write_files: out of bounds yield (0 and up to 1500000); low yield for millet, pineapple, timber; duplicate records (repeated household-plot-crop rows in the source, not removed); NA yield where area is missing; NA yield_part where the crop name is missing or is cotton.
# Unmapped modules (not plot production): agricultural_activities, asset_roaster, asset_roaster2, comments, communal_property_resources, credit_source, crop_harvestd_utilizatn (household crop disposal and sales; currency not stated), fruit_beverages, household_diseases, improved_crop_management, improved_crop_varieties (adoption, not the plot variety name), ipm, land_use_management, land_use_mgt_short_rainy, livestock_ownership, livestock_products, market_access, meat_animalproducts, month_food_scarcity, nonfood_items1, nonfood_items2, other_income, other_livestock, others, platform_type, staple_food, utilzationshortrainyseason, vegetable_fat_others, warrantage_use, and the *_metadata.csv files.

## NOT PROCESSED
# input_use.csv / input_short_rainy_season.csv: decision-maker, person working the plot, seed cost, hired labour cost (currency not stated), fertilizer specify-fields (numeric), pesticide quantity (unit is kg or litres).
# hh_compostion_characteristic.csv: housing, food security, credit, extension, livestock costs, labour counts, crops-grown codes for season A/B. Farmsize is total farm size, not the plot area in the input files.
# household_identifies.csv: rows whose district text is a code, "RWANDA", or a person's name were not used as adm2.
# crop_harvestd_utilizatn.csv: sales, consumption, and storage of the harvest. Not joined, because it is household-by-crop rather than plot-level and would double count input_use harvests. Prices have no currency.

## NEW VARIABLES
# harvest_dry_kg_: total dry harvest on the plot (kg), from T_harvst_dry / T_harvt_dry.
# harvest_fresh_kg_: total fresh harvest on the plot (kg).
# organic_kg_: organic input on the plot (kg), before the per-ha conversion.
# seed_kg_: own saved seed plus purchased seed (kg on the plot).
# cropping_system_: monocrop or intercrop, as recorded.
# intercrop_percent_: Percent_area for intercrop rows only.
# residue_use_: what was done with crop residues after harvest.
# pesticide_used_: TRUE when the pesticide quantity is > 0. Product and unit are not known.
# season_module_: "input_use" or "short rains", because the first file has no season name.


carob_script <- function(path) {

"
HumidTropic Baseline and Impact assessment of CIALCA project in Rwanda

IITA is implementing a baseline survey of Humid Tropic program in Rwanda, Congo and Burundi. The survey will serve not only as the baseline against which the HumidTropic program could be evaluated but will also serve as avenue to evaluate the impact of CIALCA project implemented since 2006 in the same area. The idea is to go back to some of the farmers that were interviewed in 2006 and see changes that have happened in their agricultural livelihoods and wellbeing after about 8 years of CIALCA implementation. 

The objectives of this activity were: (1) to insure that the households surveyed in 2006 are still living in their respective villages and are available for the survey in preparation. (2) To identify a group of about 12 farmers that are supposed to know CIALCA technologies so that they participate in Focus Group Discussions to be organised at the village level.
"

	uri <- "doi:10.25502/avx4-zj82/d"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "HumidTropics; CIALCA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 2
	)

	na_str <- c("", "NA", "N/A")
	fu <- ff[basename(ff) == "input_use.csv"]
	fs <- ff[basename(ff) == "input_short_rainy_season.csv"]
	fi <- ff[basename(ff) == "household_identifies.csv"]
	fh <- ff[basename(ff) == "hh_compostion_characteristic.csv"]
	ru <- read.csv(fu, na.strings = na_str, fileEncoding = "latin1")
	rs <- read.csv(fs, na.strings = na_str, fileEncoding = "latin1")
	ri <- read.csv(fi, na.strings = na_str, fileEncoding = "latin1")
	rh <- read.csv(fh, na.strings = na_str, fileEncoding = "latin1")

	pos <- function(x) {
		x <- as.numeric(x)
		!is.na(x) & x > 0
	}
	keep_row <- function(area, dry, fresh, q1, q2, qo, s1, s2) {
		pos(area) | pos(dry) | pos(fresh) | pos(q1) | pos(q2) | pos(qo) | pos(s1) | pos(s2)
	}

	u <- data.frame(
		hhid = as.character(ru$HHID),
		vcode = as.character(ru$Village_Sun_colline),
		parcel = as.character(ru$Parcl_id),
		plt = as.character(ru$Plt_id),
		crop_raw = ru$crops_grown,
		area = as.numeric(ru$crop_area_ha),
		system = ru$Cropping_system,
		variety_raw = ru$Crop_variety,
		pct = as.numeric(ru$Percent_area),
		seed_own = as.numeric(ru$Qty_seed_own_kg),
		seed_buy = as.numeric(ru$Qty_seed_purchs_kg),
		fert1 = ru$Inorg_fert1_used,
		q1 = as.numeric(ru$Qty_fert_kg),
		fert2 = ru$Inorg_fert2_used,
		q2 = as.numeric(ru$Qty_fert2_used),
		om = ru$organic_input_type,
		omq = as.numeric(ru$Qty_organic_input_used_kg),
		pest = as.numeric(ru$Qty_pesticdes),
		herb = as.numeric(ru$Qty_herb),
		stress_raw = ru$Strs_pest_disease,
		residue = ru$Use_crop_residue,
		dry = as.numeric(ru$T_harvst_dry),
		fresh = as.numeric(ru$T_harvst_fresh),
		season = NA_character_,
		season_module_ = "input_use"
	)
	s <- data.frame(
		hhid = as.character(rs$HHID),
		vcode = as.character(rs$Village_Sun_colline),
		parcel = as.character(rs$parcell_id),
		plt = as.character(rs$plott_id),
		crop_raw = rs$crops_grwn,
		area = as.numeric(rs$Crops_area),
		system = rs$Cropn_systm,
		variety_raw = rs$Crop_varieties,
		pct = as.numeric(rs$percent_areas),
		seed_own = as.numeric(rs$Qty_seed_ownd),
		seed_buy = as.numeric(rs$Qty_seed_purchsd),
		fert1 = rs$inorganc_fert1_type,
		q1 = as.numeric(rs$Qty_fert1_use),
		fert2 = rs$inorganic_fert2_typ,
		q2 = as.numeric(rs$Qty_fert2_use),
		om = rs$organic_inpt_type,
		omq = as.numeric(rs$Qty_organic_inpt_use),
		pest = as.numeric(rs$Qty_pestcde),
		herb = as.numeric(rs$Qty_herbcdes),
		stress_raw = rs$stres_pest_disease,
		residue = rs$crop_residue,
		dry = as.numeric(rs$T_harvt_dry),
		fresh = as.numeric(rs$T_harvt_fresh),
		season = "short rains",
		season_module_ = "short rains"
	)
	r <- rbind(u, s)
	r <- r[keep_row(r$area, r$dry, r$fresh, r$q1, r$q2, r$omq, r$seed_own, r$seed_buy), ]

	canon_dist <- function(x) {
		y <- tolower(trimws(x))
		y <- chartr("\u00e4\u00e0\u00e1\u00e2\u00e3\u00e5\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef\u00f3\u00f2\u00f4\u00f5\u00f6\u00fa\u00f9\u00fb\u00fc\u00fd\u00f1\u00e7",
			"aaaaaaeeeeiiiiooooouuuuync", y)
		y <- gsub("[^a-z]", "", y)
		out <- rep(NA_character_, length(y))
		out[y %in% c("bugesera", "bugesesra", "bugrsera", "bugsera")] <- "Bugesera"
		out[y == "gatsibo"] <- "Gatsibo"
		out[y %in% c("karongi", "karong")] <- "Karongi"
		out[y == "kayonza"] <- "Kayonza"
		out[y == "kirehe"] <- "Kirehe"
		out[y == "nyabihu"] <- "Nyabihu"
		out[y == "nyagatare"] <- "Nyagatare"
		out[y == "rubavu"] <- "Rubavu"
		out
	}
	mode_name <- function(x) {
		x <- x[!is.na(x) & x != ""]
		if (!length(x)) return(NA_character_)
		tab <- sort(table(x), decreasing = TRUE)
		names(tab)[1]
	}
	place <- trimws(ri$SectorGroupmentZone)
	place[grepl("^[0-9]+$", place)] <- NA
	place[is.na(place)] <- NA
	vill <- trimws(ri$VillageSun_colline)
	vill[grepl("^[0-9]+$", vill)] <- NA
	id <- data.frame(
		vcode = as.character(ri$Village_Sun_colline),
		adm2 = canon_dist(ri$DistrictCefferieCommune),
		adm3 = carobiner::fix_name(place, "title"),
		location = carobiner::fix_name(vill, "title")
	)
	id$adm3[is.na(place)] <- NA
	id$location[is.na(vill)] <- NA
	lk <- aggregate(cbind(adm2, adm3, location) ~ vcode, id, mode_name)

	map_crop <- function(x) {
		key <- tolower(trimws(x))
		m <- c(
			"banana" = "banana",
			"sweet banana" = "banana",
			"beans" = "common bean",
			"cabbage" = "cabbage",
			"carrots" = "carrot",
			"cassava" = "cassava",
			"chilies" = "chili pepper",
			"coffee" = "coffee",
			"cotton" = "cotton",
			"fodder" = "forage crop",
			"gardamon" = "cardamom",
			"groundnuts" = "groundnut",
			"irish potatoes" = "potato",
			"kale" = "kale",
			"maize" = "maize",
			"millet" = "millet",
			"other green leafy vegetables" = "vegetable",
			"others" = NA_character_,
			"pigeon peas" = "pigeon pea",
			"pineapple" = "pineapple",
			"rice" = "rice",
			"sorghum" = "sorghum",
			"soyabean" = "soybean",
			"sweet potatoes" = "sweetpotato",
			"timber" = "timber",
			"tomatoes" = "tomato",
			"wheat" = "wheat",
			"yam" = "yam",
			"eggplants" = "eggplant"
		)
		out <- unname(m[key])
		out[is.na(key)] <- NA_character_
		out
	}
	parts <- c(
		"banana" = "fruit", "common bean" = "seed", "cabbage" = "leaves", "carrot" = "roots",
		"cassava" = "roots", "chili pepper" = "fruit", "coffee" = "seed",
		"forage crop" = "aboveground biomass", "cardamom" = "seed", "groundnut" = "seed",
		"potato" = "tubers", "kale" = "leaves", "maize" = "grain", "millet" = "grain",
		"vegetable" = "leaves", "pigeon pea" = "seed", "pineapple" = "fruit",
		"rice" = "grain", "sorghum" = "grain", "soybean" = "seed", "sweetpotato" = "roots",
		"timber" = "wood", "tomato" = "fruit", "wheat" = "seed", "yam" = "tubers",
		"eggplant" = "fruit"
	)
	stress_map <- c(
		"Diseases" = "disease", "Drought" = "drought", "Insects pest" = "pests",
		"Low soil fertility" = "nutrient deficiency", "No stress" = "none",
		"Others" = "other", "Rodents" = "rodents", "Water logging" = "excess water"
	)

	fert_kg <- function(type, qty) {
		type <- trimws(as.character(type))
		qty[is.na(qty)] <- 0
		n <- rep(0, length(type))
		p <- n
		k <- n
		is_dap <- !is.na(type) & type == "Diammonium phosphate(DAP)"
		is_urea <- !is.na(type) & type == "UREA"
		is_map <- !is.na(type) & type == "MonoAmmonium phosphate(MAP)"
		is_none <- is.na(type) | type == "No fertilizer"
		is_none[is.na(is_none)] <- FALSE
		unclear <- (!is_dap & !is_urea & !is_map & !is_none & qty > 0) |
			(!is.na(type) & type == "No fertilizer" & qty > 0)
		n[unclear] <- NA
		p[unclear] <- NA
		k[unclear] <- NA
		n[is_dap] <- qty[is_dap] * 0.18
		p[is_dap] <- qty[is_dap] * 0.201
		n[is_urea] <- qty[is_urea] * 0.46
		n[is_map] <- qty[is_map] * 0.11
		p[is_map] <- qty[is_map] * 0.269
		list(n = n, p = p, k = k)
	}
	fert_lab <- function(type, qty) {
		type <- trimws(as.character(type))
		qty_pos <- !is.na(qty) & qty > 0
		known <- !is.na(type)
		out <- rep(NA_character_, length(type))
		out[known & type == "Diammonium phosphate(DAP)" & qty_pos] <- "DAP"
		out[known & type == "UREA" & qty_pos] <- "urea"
		out[known & type == "MonoAmmonium phosphate(MAP)" & qty_pos] <- "MAP"
		out[known & type == "NPK" & qty_pos] <- "NPK"
		out[known & type == "Others" & qty_pos] <- "unknown"
		out[known & type == "No fertilizer" & qty_pos] <- "unknown"
		out
	}

	crop <- map_crop(r$crop_raw)
	variety <- rep(NA_character_, nrow(r))
	variety[tolower(trimws(r$crop_raw)) == "sweet banana"] <- "sweet banana"
	vtype <- tolower(trimws(r$variety_raw))
	vtype[vtype == ""] <- NA

	area <- r$area
	area[is.na(area) | area <= 0] <- NA

	a <- fert_kg(r$fert1, r$q1)
	b <- fert_kg(r$fert2, r$q2)
	n_kg <- a$n + b$n
	p_kg <- a$p + b$p
	k_kg <- a$k + b$k
	to_ha <- function(kg, area) {
		out <- rep(NA_real_, length(kg))
		zero <- !is.na(kg) & kg == 0
		out[zero] <- 0
		ok <- !is.na(kg) & kg != 0 & !is.na(area)
		out[ok] <- kg[ok] / area[ok]
		out
	}
	lab1 <- fert_lab(r$fert1, r$q1)
	lab2 <- fert_lab(r$fert2, r$q2)
	fertilizer_type <- paste(lab1, lab2, sep = "; ")
	fertilizer_type[is.na(lab1) & is.na(lab2)] <- "none"
	fertilizer_type[is.na(lab1) & !is.na(lab2)] <- lab2[is.na(lab1) & !is.na(lab2)]
	fertilizer_type[!is.na(lab1) & is.na(lab2)] <- lab1[!is.na(lab1) & is.na(lab2)]

	om_type <- rep("none", nrow(r))
	om_src <- trimws(as.character(r$om))
	om_src[om_src == "Animal manure"] <- "farmyard manure"
	om_src[om_src == "Compost"] <- "compost"
	om_src[om_src == "Others"] <- "unknown"
	use_om <- !is.na(r$omq) & r$omq > 0
	om_type[use_om] <- om_src[use_om]
	om_type[use_om & (is.na(om_src) | om_src == "")] <- "unknown"

	seed_kg <- rowSums(cbind(r$seed_own, r$seed_buy), na.rm = TRUE)
	seed_kg[is.na(r$seed_own) & is.na(r$seed_buy)] <- NA
	seed_rate <- rep(NA_real_, nrow(r))
	ok_seed <- !is.na(seed_kg) & seed_kg > 0 & !is.na(area)
	seed_rate[ok_seed] <- seed_kg[ok_seed] / area[ok_seed]

	sys <- trimws(r$system)
	cropping_system_ <- rep(NA_character_, nrow(r))
	cropping_system_[sys == "Monocropping"] <- "monocrop"
	cropping_system_[sys == "Intercropping"] <- "intercrop"
	intercrop_percent_ <- ifelse(cropping_system_ == "intercrop", r$pct, NA_real_)

	stress <- unname(stress_map[trimws(r$stress_raw)])
	stress[is.na(r$stress_raw)] <- NA_character_

	use_dry <- !is.na(r$dry) & r$dry > 0
	only_fresh <- !is.na(r$fresh) & r$fresh > 0 & !use_dry
	yield <- rep(NA_real_, nrow(r))
	yield[use_dry] <- r$dry[use_dry] / area[use_dry]
	yield[only_fresh] <- r$fresh[only_fresh] / area[only_fresh]
	zero_h <- !use_dry & !only_fresh & !is.na(area)
	yield[zero_h] <- 0
	yield_isfresh <- rep(NA, nrow(r))
	yield_isfresh[use_dry & !is.na(yield)] <- FALSE
	yield_isfresh[only_fresh & !is.na(yield)] <- TRUE

	herb_used <- rep(NA, nrow(r))
	herb_known <- !is.na(r$herb)
	herb_used[herb_known] <- r$herb[herb_known] > 0
	pest_used <- rep(NA, nrow(r))
	pest_known <- !is.na(r$pest)
	pest_used[pest_known] <- r$pest[pest_known] > 0

	residue <- trimws(as.character(r$residue))
	residue[residue == ""] <- NA

	d <- data.frame(
		hhid = r$hhid,
		trial_id = ifelse(is.na(r$season), paste0(r$hhid, "_M"), paste0(r$hhid, "_S")),
		plot_id = paste(r$parcel, r$plt, sep = "-"),
		country = "Rwanda",
		season = r$season,
		season_module_ = r$season_module_,
		crop = crop,
		variety = variety,
		variety_type = vtype,
		field_size = area,
		cropping_system_ = cropping_system_,
		intercrop_percent_ = intercrop_percent_,
		fertilizer_type = fertilizer_type,
		N_fertilizer = to_ha(n_kg, area),
		P_fertilizer = to_ha(p_kg, area),
		K_fertilizer = to_ha(k_kg, area),
		OM_used = use_om,
		OM_type = om_type,
		OM_amount = to_ha(ifelse(use_om, r$omq, 0), area),
		organic_kg_ = r$omq,
		seed_kg_ = seed_kg,
		seed_rate = seed_rate,
		herbicide_used = herb_used,
		pesticide_used_ = pest_used,
		stress = stress,
		residue_use_ = residue,
		harvest_dry_kg_ = r$dry,
		harvest_fresh_kg_ = r$fresh,
		yield = yield,
		yield_part = unname(parts[crop]),
		yield_isfresh = yield_isfresh,
		vcode = r$vcode
	)
	d$yield_part[is.na(d$crop)] <- NA
	d$OM_amount[!use_om] <- 0
	# named product but no area: amount per ha is unknown, including a 0 from to_ha on kg==0 only
	miss_area <- use_om & is.na(area)
	d$OM_amount[miss_area] <- NA

	d <- merge(d, lk, by = "vcode", all.x = TRUE)
	d$vcode <- NULL
	east <- c("Bugesera", "Gatsibo", "Kayonza", "Kirehe", "Nyagatare")
	west <- c("Karongi", "Nyabihu", "Rubavu")
	d$adm1 <- NA_character_
	d$adm1[d$adm2 %in% east] <- "Eastern Province"
	d$adm1[d$adm2 %in% west] <- "Western Province"

	hh_type <- tolower(trimws(rh$HH_type))
	gend <- rep(NA_character_, length(hh_type))
	gend[grepl("female", hh_type)] <- "female"
	gend[grepl("male headed", hh_type) & !grepl("female", hh_type)] <- "male"
	hg <- data.frame(hhid = as.character(rh$HHID), farmer_gender = gend)
	hg <- hg[!duplicated(hg$hhid), ]
	d <- merge(d, hg, by = "hhid", all.x = TRUE)

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- as.character(d$trial_id)

	carobiner::write_files(path, meta, d)
}
