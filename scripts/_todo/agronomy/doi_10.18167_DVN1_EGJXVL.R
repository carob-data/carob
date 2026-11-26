# R script for "carob"
# license: GPL (>=3)

## NOTES
# Measured grain yields (Trials_yield_observation.csv) and SOC stocks (Trials_SOC_observations.csv) from four long-term rainfed maize experiments in sub-Saharan Africa: Gagnoa (Côte d'Ivoire), Embu and Machanga (Kenya), and Murewa (Zimbabwe), from Couëdel et al. (2026), Global Change Biology, doi:10.1111/gcb.70720.
# Only observed trial data and calibration inputs are processed; crop model simulations and virtual climate-change scenarios are excluded.
# Yield is reported in t dry matter/ha (summary_data_definition.xlsx), so yield is multiplied by 1000, yield_moisture = 0, and yield_isfresh = FALSE.
# Treatments combine organic amendment (+C/-C) and mineral nitrogen (+N/-N), corresponding to fertilizer/OM codes C1N1, C1N0, C0N1, C0N0.
# Mineral N, P, and K (fe_n, fe_p, fe_k) are elemental kg/ha summed over up to three applications; blank cells on matched rows are 0.
# Fertilizer type "CaNH4NO2" at Machanga is a typo for "CaNH4NO3" (CAN); "ammonium nitrate" is mapped to "AN", "super-phosphate" to "SSP", and "PK fertiliser" kept as "PK".
# Organic amendment amounts (om_amount_DW, t DW/ha) are converted to kg/ha (* 1000); om_type is compost at Gagnoa and manure (farmyard manure) at Embu, Machanga, and Murewa. N_organic, P_organic, and K_organic are computed from dry-weight amount and nutrient concentrations (% DW).
# Phenology dates (emergence, anthesis -> flowering_date, physiological maturity, harvest) come from obs_phenology.csv.
# Topsoil properties come from soil_general_info.csv and soil_layers_characterisctics_corrected.xlsx (first soil layer per site).
# Daily weather data for all four sites are read from Models_inputs/weather/*.txt (solar radiation MJ/m2 -> kJ/m2).

## ISSUES
# Ten yield rows in Trials_yield_observation.csv have missing yield values and are kept.
# "PK" (from "PK fertiliser") is not in the terminag fertilizer_type vocabulary.
# K_organic reaches 650 kg K/ha on Gagnoa compost plots (10.96 t DW/ha at 5.93% K), exceeding terminag's upper bound of 300 kg/ha.
# Trials_SOC_observations.csv reports SOC stock in t C/ha for topsoil (0-20 cm at Gagnoa and Murewa; 0-15 cm at Embu and Machanga), linked to the nearest season record_id per site and treatment. Converted to g/m2 (* 100), values (1164-5691 g/m2) exceed terminag's current valid_max of 1000 for soil_C_stock.

## NEW VARIABLES
# None.

## NOT PROCESSED
# Trials_models_yield_simulations.csv, Trials_models_SOC_simulations.csv, Models_ouputs_simulation_scenarios.csv, Virtual_experiments_models_input_outputs/*: simulated/modeled outputs and virtual scenarios (not observed data).
# soil_layers_characterisctics_corrected.xlsx: deeper soil layers beyond the topsoil layer and van Genuchten hydraulic parameters (Or, alpha, n, coarse_fraction).
# soil_layers_initial.csv: initial treatment-specific Nmin, SOC, and SON stocks by layer.
# phenology_info.csv: GDD model parameters.
# residue_initial.csv: initial surface/root residue (all 0 after fallow).
# ICGA - Copie.txt: duplicate copy of ICGA.txt.
# READ.ME.txt, summary_data_definition.xlsx: documentation / codebook.


carob_script <- function(path) {

"
Data for 'Beyond climate change: the role of integrated soil fertility management for sustaining future maize yield in sub-Saharan Africa'

The dataset contains 15 soil crop model i) input and outputs on long-term experimental data on maize growth and soil organic carbon and ii) climate change and integrated soil fertility management simulation outputs at four sites in sub-Saharan Africa (2026)
"

	uri <- "doi:10.18167/DVN1/EGJXVL"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1111/gcb.70720",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;N_fertilizer;OM_amount;OM_type",
		response_vars = "yield",
		carob_contributor = NA,
		carob_LLM = "Gemini 4 Neon",
		carob_date = "202?-??-??",
		carob_completion = 85,
		carob_effort = 0
	)

	read_semi <- function(f) {
		read.csv(f, sep=";", check.names=FALSE, na.strings=c("", "NA", "na"))
	}
	read_yield <- function(f) {
		raw <- readChar(f, file.info(f)$size, useBytes=TRUE)
		raw <- iconv(raw, from="latin1", to="UTF-8")
		lines <- strsplit(raw, "\r\n|\n")[[1]]
		lines <- lines[nzchar(lines)]
		lines <- sub("^\"", "", lines)
		lines <- sub("\"$", "", lines)
		lines <- gsub("\"\"", "\"", lines, fixed=TRUE)
		read.csv(text=paste(lines, collapse="\n"), check.names=FALSE, na.strings=c("", "NA", "na"))
	}
	sum3 <- function(a, b, c) {
		a <- as.numeric(a); a[is.na(a)] <- 0
		b <- as.numeric(b); b[is.na(b)] <- 0
		c <- as.numeric(c); c[is.na(c)] <- 0
		a + b + c
	}
	mdy_iso <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		as.character(as.Date(x, "%m/%d/%Y"))
	}

	f1 <- ff[basename(ff) == "Trials_yield_observation.csv"]
	f2 <- ff[basename(ff) == "crop_sequence.csv"]
	f3 <- ff[basename(ff) == "planting.csv"]
	f4 <- ff[basename(ff) == "obs_phenology.csv"]
	f5 <- ff[basename(ff) == "mineral_fertilisation.csv"]
	f6 <- ff[basename(ff) == "organic_amendment.csv"]
	f7 <- ff[basename(ff) == "soil_preparation.csv"]
	f8 <- ff[basename(ff) == "site_general_info.csv"]
	f9 <- ff[basename(ff) == "soil_general_info.csv"]
	f10 <- ff[basename(ff) == "soil_layers_characterisctics_corrected.xlsx"]
	f11 <- ff[basename(ff) == "Trials_SOC_observations.csv"]

	r1 <- read_yield(f1)
	r2 <- read.csv(f2, check.names=FALSE, na.strings=c("", "NA", "na"))
	r3 <- read_semi(f3)
	r4 <- read_semi(f4)
	r5 <- read_semi(f5)
	r6 <- read_semi(f6)
	r7 <- read.csv(f7, check.names=FALSE, na.strings=c("", "NA", "na"))
	r8 <- read.csv(f8, check.names=FALSE, na.strings=c("", "NA", "na"))
	r9 <- read_semi(f9)
	r10 <- carobiner::read.excel(f10, sheet="Feuil1")

	# Yield table
	fert_code <- r1$id_treatment
	fert_code <- gsub("+C", "C1", fert_code, fixed=TRUE)
	fert_code <- gsub("-C", "C0", fert_code, fixed=TRUE)
	fert_code <- gsub("+N", "N1", fert_code, fixed=TRUE)
	fert_code <- gsub("-N", "N0", fert_code, fixed=TRUE)

	d1 <- data.frame(
		trial_id = r1$id_season,
		site_code = substr(r1$id_season, 1, 4),
		fert_code = fert_code,
		treatment = r1$id_treatment,
		yield = as.numeric(r1$Yield) * 1000
	)

	# Crop sequence and previous crop per site
	prev_crop <- rep("fallow", nrow(r2))
	for (i in seq_len(nrow(r2))) {
		if (i > 1 && r2$id_site[i] == r2$id_site[i - 1]) {
			prev_crop[i] <- r2$crop[i - 1]
		}
	}
	prev_crop[prev_crop == "fallow"] <- "none"
	d2 <- data.frame(
		trial_id = r2$id_season,
		crop = ifelse(r2$crop == "fallow", "none", r2$crop),
		previous_crop = prev_crop
	)

	# Planting details
	d3 <- data.frame(
		trial_id = r3$id_season,
		variety = as.character(r3$cultivar),
		variety_type = as.character(r3$cul_note),
		planting_date = mdy_iso(r3$pdate),
		plant_density = as.numeric(r3$plant_population) * 10000,
		row_spacing = as.numeric(r3$row_spacing),
		planting_depth = as.numeric(r3$planting_depth)
	)

	# Observed phenology (emergence, anthesis -> flowering, maturity, harvest)
	d4 <- data.frame(
		trial_id = r4$id_season,
		fert_code = r4$id_tratnt,
		emergence_date = mdy_iso(r4$emergence_date),
		flowering_date = mdy_iso(r4$anthesis_date),
		maturity_date = mdy_iso(r4$physio_maturity_date),
		harvest_date = mdy_iso(r4$harvest_date)
	)

	# Mineral fertilization
	ft_map <- c(
		"urea" = "urea",
		"ammonium nitrate" = "AN",
		"CaNH4NO3" = "CAN",
		# CaNH4NO2 at Machanga is a typo for CaNH4NO3 (CAN)
		"CaNH4NO2" = "CAN",
		"super-phosphate" = "SSP",
		"PK fertiliser" = "PK"
	)
	ft_vec <- mapply(function(a, b, c) {
		v <- unique(unname(ft_map[trimws(c(a, b, c))]))
		v <- v[!is.na(v)]
		if (length(v) == 0) "none" else paste(v, collapse="; ")
	}, as.character(r5$fe_type1), as.character(r5$fe_type2), as.character(r5$fe_type3), USE.NAMES=FALSE)

	n1 <- as.numeric(r5$fe_n1); n2 <- as.numeric(r5$fe_n2); n3 <- as.numeric(r5$fe_n3)
	n_splits <- as.integer((!is.na(n1) & n1 > 0) + (!is.na(n2) & n2 > 0) + (!is.na(n3) & n3 > 0))
	Nf <- sum3(r5$fe_n1, r5$fe_n2, r5$fe_n3)
	Pf <- sum3(r5$fe_p1, r5$fe_p2, r5$fe_p3)
	Kf <- sum3(r5$fe_k1, r5$fe_k2, r5$fe_k3)

	d5 <- data.frame(
		trial_id = r5$id_season,
		fert_code = r5$id_treatment,
		N_fertilizer = Nf,
		P_fertilizer = Pf,
		K_fertilizer = Kf,
		N_splits = n_splits,
		fertilizer_used = (Nf + Pf + Kf) > 0,
		fertilizer_type = ft_vec
	)

	# Organic amendments (dry-weight basis in t/ha -> kg/ha; nutrient conc in % DW)
	om_amt <- as.numeric(r6$om_amount_DW) * 1000
	om_amt[is.na(om_amt)] <- 0
	om_map <- c(compost = "compost", manure = "farmyard manure")
	om_t <- unname(om_map[trimws(as.character(r6$om_type))])
	om_t[is.na(om_t) | om_amt == 0] <- "none"
	d6 <- data.frame(
		trial_id = r6$id_season,
		fert_code = r6$id_treatment,
		OM_amount = om_amt,
		OM_used = om_amt > 0,
		OM_type = om_t,
		N_organic = ifelse(om_amt > 0, om_amt * as.numeric(r6$om_n_conc) / 100, 0),
		P_organic = ifelse(om_amt > 0, om_amt * as.numeric(r6$om_p_conc) / 100, 0),
		K_organic = ifelse(om_amt > 0, om_amt * as.numeric(r6$om_k_conc) / 100, 0)
	)

	# Soil preparation
	imp_map <- function(x) {
		x <- trimws(as.character(x))
		out <- rep(NA_character_, length(x))
		out[grepl("cultivator", x, ignore.case=TRUE)] <- "cultivator"
		out[grepl("hand hoe", x, ignore.case=TRUE)] <- "hoe"
		out[grepl("mouldboard plough", x, ignore.case=TRUE)] <- "mouldboard plough"
		out
	}
	imp1 <- imp_map(r7$sprep_implement)
	imp2 <- imp_map(r7$sprep_implement2)
	imp <- mapply(function(a, b) {
		v <- unique(c(a, b))
		v <- v[!is.na(v)]
		if (length(v) == 0) NA_character_ else paste(v, collapse="; ")
	}, imp1, imp2, USE.NAMES=FALSE)
	d7 <- data.frame(
		trial_id = r7$id_season,
		land_prep_method = ifelse(!is.na(r7$sprep_type), "tillage", NA_character_),
		land_prep_implement = imp,
		tillage_depth = as.numeric(r7$sprep_depth),
		land_prep_date = mdy_iso(r7$sprep)
	)

	# Site & topsoil properties (first layer per site)
	country_map <- c(ICGA = "Côte d'Ivoire", KEEM = "Kenya", KEMA = "Kenya", ZIMU = "Zimbabwe")
	tex_map <- c("sandy clay loam" = "sandy clay loam", "sandy soil" = "sand", "clay soil" = "clay")
	top <- r10[!duplicated(r10$id_soil), ]
	d_site <- merge(
		data.frame(
			site_code = r8$id_site,
			country = unname(country_map[r8$id_site]),
			location = trimws(as.character(r8$site_name)),
			latitude = as.numeric(r8$latitude),
			longitude = as.numeric(r8$longitude),
			elevation = as.numeric(r8$elevation),
			geo_from_source = TRUE
		),
		data.frame(
			site_code = r9$id_site,
			soil_type = trimws(as.character(r9$soil_type)),
			soil_texture = unname(tex_map[trimws(as.character(r9$texture))])
		),
		by="site_code", all.x=TRUE
	)
	d_soil <- data.frame(
		site_code = top$id_soil,
		depth_top = 0,
		depth_bottom = as.numeric(top$layer_base_depth),
		soil_bd = as.numeric(top$bulk_density),
		soil_clay = as.numeric(top$clay),
		soil_silt = as.numeric(top$silt),
		soil_sand = as.numeric(top$sand),
		soil_CEC = as.numeric(top$cec),
		# summary_data_definition.xlsx: P_extractable is mg/kg, K/Ca_extractable are me/100g = cmol(+)/kg
		soil_P = as.numeric(top[["P_extractable%"]]),
		soil_K_exch = as.numeric(top[["K_extractable%"]]),
		soil_Ca_exch = as.numeric(top[["Ca_extractable%"]]),
		soil_pH = as.numeric(top$pH_water),
		soil_SOC = as.numeric(top[["organic_C%"]]),
		# % -> mg/kg (* 10000)
		soil_N_total = as.numeric(top[["organic_N%"]]) * 10000,
		soil_FC = as.numeric(top$drained_upper_limit),
		soil_PWP = as.numeric(top$lower_limit)
	)
	d_site <- merge(d_site, d_soil, by="site_code", all.x=TRUE)

	d <- merge(d1, d2, by="trial_id", all.x=TRUE)
	d <- merge(d, d3, by="trial_id", all.x=TRUE)
	d <- merge(d, d4, by=c("trial_id", "fert_code"), all.x=TRUE)
	d <- merge(d, d5, by=c("trial_id", "fert_code"), all.x=TRUE)
	d <- merge(d, d6, by=c("trial_id", "fert_code"), all.x=TRUE)
	d <- merge(d, d7, by="trial_id", all.x=TRUE)
	d <- merge(d, d_site, by="site_code", all.x=TRUE)

	part_map <- c(maize = "grain", soybean = "seed", none = "none")
	d$yield_part <- unname(part_map[d$crop])
	d$yield_moisture <- 0
	d$yield_isfresh <- FALSE
	# Rainfed long-term experiments (Couëdel et al. 2026)
	d$irrigated <- FALSE
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$record_id <- seq_len(nrow(d))

	# Observed SOC time series (t C/ha -> g/m2: * 100), matched to nearest season record_id per site x treatment
	r11 <- read.csv(f11, fileEncoding="latin1")
	soc_site <- c("Côte d'Ivoire" = "ICGA", "Kenya-Embu" = "KEEM", "Kenya-Machanga" = "KEMA", "Zimbabwe" = "ZIMU")
	sc <- unname(soc_site[r11$id_site])
	d_pdate <- as.Date(d$planting_date)
	soc_date <- as.Date(r11$Date)
	rec_id <- integer(nrow(r11))
	for (i in seq_len(nrow(r11))) {
		cand <- which(d$site_code == sc[i] & d$treatment == r11$id_treatment[i] & !is.na(d_pdate))
		rec_id[i] <- d$record_id[cand[which.min(abs(d_pdate[cand] - soc_date[i]))]]
	}
	long <- data.frame(
		record_id = rec_id,
		date = as.character(r11$Date),
		# t C/ha * 100 = g C/m2 (topsoil: 0-20 cm Gagnoa/Murewa, 0-15 cm Embu/Machanga)
		soil_C_stock = as.numeric(r11$SOC) * 100
	)

	d$site_code <- NULL
	d$fert_code <- NULL

	# Daily weather files (Models_inputs/weather/*.txt)
	wfiles <- ff[basename(ff) %in% c("ICGA.txt", "KEEM.txt", "KEMA.txt", "ZIMU.txt")]
	wlist <- lapply(wfiles, function(wf) {
		sid <- sub("\\.txt$", "", basename(wf))
		rw <- read.table(wf, skip=4, header=FALSE,
			col.names=c("year", "doy", "srad", "tmax", "tmin", "rhum", "prec", "wspd"))
		data.frame(
			station_id = sid,
			country = unname(country_map[sid]),
			location = d_site$location[match(sid, d_site$site_code)],
			longitude = d_site$longitude[match(sid, d_site$site_code)],
			latitude = d_site$latitude[match(sid, d_site$site_code)],
			elevation = d_site$elevation[match(sid, d_site$site_code)],
			geo_from_source = TRUE,
			date = as.character(as.Date(paste0(rw$year - 1L, "-12-31")) + rw$doy),
			# MJ/m2 -> kJ/m2
			srad = as.numeric(rw$srad) * 1000,
			tmax = as.numeric(rw$tmax),
			tmin = as.numeric(rw$tmin),
			rhum = as.numeric(rw$rhum),
			prec = as.numeric(rw$prec),
			wspd = as.numeric(rw$wspd)
		)
	})
	wth <- do.call(rbind, wlist)

	carobiner::write_files(path, meta, d, long=long, wth=wth)
}
