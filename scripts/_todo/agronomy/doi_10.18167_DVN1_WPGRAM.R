# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two 4-month cover-crop diversity and weed-control trials (austral summer 2019-2020 = wet season; austral winter 2020 = dry season) at the CIRAD Bassin Plat experimental station, Saint-Pierre, Réunion Island (Négrier et al. 2023, doi:10.25518/1780-4507.20220).
# Complete randomized design: 13 cover-crop treatments (1 to 4 species in substitutive mixtures, 3 replicates except the 4-species mixture with 4 replicates; 2 m x 4 m = 8 m2 plots) plus adjacent bare weed-control plots (0sp; 2 m x 1.5 m = 3 m2 plots).
# Wide table d has one row per component cover-crop species per plot (88 rows per season, sharing plot_id with intercrops set to partner species) plus observed bare weed-control plots (20 in summer, 30 in winter).
# Cover-crop aboveground fresh mass (kg/m2 * 10000 -> kg/ha) is yield (yield_part = "aboveground biomass", yield_isfresh = TRUE), with yield_moisture = 100 - DM%, dmy_total = fresh * DM%/100, leaves_moisture, stems_moisture, leaf_mass_fraction_, dmy_leaves, dmy_stems, and specific_leaf_area_.
# Plot-level weed biomass (from _weed units on cover-crop plots and L#_T# units on bare control plots) is stored as fresh weight in weed_biomass (kg/ha) and dry weight in weed_dmy_ (kg/ha); in mixtures, plot-level weed_biomass/weed_dmy_ are repeated on each component species row sharing plot_id.
# Long table stores repeated per-species plant_height and ground_cover (linked by record_id and date), plot-level weed_cover and total cover_crop_cover_ (attached to the first record_id of each plot_id x date), and site-level soil chemistry at 0-15 cm and 15-30 cm depths (attached to the first record_id of each trial_id with plot_id = NA).
# Soil S_cmol_kg_dry_soil equals Ca + Mg + K + Na (French "somme des bases échangeables"), used to compute soil_BS = 100 * S / CEC; Norg_g_kg_dry_soil (* 1000 -> mg/kg) is stored as soil_N_total; Corg_g_kg_dry_soil (/ 10 -> %) as soil_SOC.
# Daily weather from Meteo.csv (Bassin Plat station) is passed via wth. RAD (values 578-3152, mean 2013, with ETP 1.3-7.0 mm) is in J/cm2/day (confirmed against sibling Bassin Plat dataset doi:10.18167/DVN1/HA0V2G Rg in MJ/m2) and converted * 10 to kJ/m2 for srad.

## ISSUES
# P_fertilizer and K_fertilizer are not reported in Summer_itk.xlsx, Winter_itk.xlsx, or the publication (only 45 kg N/ha is reported).
# Winter_itk.xlsx accidentally repeats the 88 summer Seed_sowing_density_kgha rows (summer species codes GUIAB/PHSAU/CNAEN/PESGL and summer sowing date 2019-11-12); therefore winter sown cover-crop seed_rate is left NA.
# Bare weed-control plots (0sp; 20 in summer, 30 in winter) and 5 sown summer species-plot units without harvest biomass (L3_P10_PHSAU, L3_P13_PHSAU, L2_P6_PESGL, L7_P6_PESGL, L2_P12_PESGL) have yield = NA (triggers NA detected: yield). Ten unmeasured summer bare plots (L1_T3-L10_T3) have no records in Summer_observation.csv and are omitted.
# Raw Summer_observation.csv has two data-entry bugs fixed here: (1) in the CNAEN Plant_height_cm block (rows 54-163), every 3rd row (32 rows) was mislabeled _GUIAB instead of _CNAEN (including 10 rows on P7/P12 plots where GUIAB was never sown, and 22 colliding with the true GUIAB block); these are reassigned to _CNAEN; (2) L6_P5_Cover_crop appears twice across all 11 dates (rows 2880-2920 repeat L6_P5_GUIAB inside the P1 monoculture block), so the duplicate monoculture-block copy is removed.
# Raw Winter_observation.csv stores Leaves_sample_dry_matter_content_% for GUIAB as a 0-1 fraction (0.204-0.675) instead of % (12.1-52.5% for all other species); these 22 rows are multiplied by 100.
# Fresh aboveground biomass of pearl millet (up to 51,625 kg/ha) and mung bean (up to 23,663 kg/ha) exceeds terminag grain max_yield checks, and early cover-crop plant_height values include readings down to 1 cm; all are valid field measurements and kept as reported.
# Soil observations in Summer_observation.csv (12/11/2019) and Winter_observation.csv (12/08/2020) have identical site-level values for 0-15 cm and 15-30 cm layers.

## NEW VARIABLES
# species_number_: number of sown cover-crop species in the plot (0 to 4)
# leaf_mass_fraction_: ratio of leaf dry mass to total aboveground dry mass in the harvested sample (0 to 1)
# specific_leaf_area_: specific leaf area of the cover-crop species at harvest (cm2/g)
# weed_dmy_: dry weight aboveground biomass of weeds per plot (kg/ha)
# cover_crop_cover_: total visual ground cover of all cover-crop species combined in a plot (%)
# soil_N_min_potential_: potential annual soil nitrogen mineralization (kg/ha/year)

## NOT PROCESSED
# NEARPLOT.csv: spatial adjacency table listing the 3 nearest bare weed-control plots (L#_T#) for each cover-crop plot (L#_P#); spatial neighbour metadata, not agronomic observations.
# Summer_observation.csv / Winter_observation.csv: C_to_N_ratio (derived from Corg_g_kg_dry_soil / Norg_g_kg_dry_soil) and S_cmol_kg_dry_soil (sum of exchangeable bases Ca + Mg + K + Na, used to compute soil_BS).
# Readme.xlsx, Summer_design.csv, Winter_design.csv: dataset documentation and hierarchical plot-design lookup tables.

carob_script <- function(path) {

"
Experimental data set of cover crop mixture and weed control in tropical summer and winter in Reunion Island.

Weed pressure is a main biotic constraint in tropical agriculture. Cover crop mixtures have increased in popularity to limit weed growth through competition for shared resources but the relation between cover crop diversity and weed suppression is still under debate. This study aimed to assess the impact of increasing cover crops diversity (one to four species) on weed control during two 4-month growing seasons (tropical summer and winter) in Reunion Island. Weed control was expressed in terms of ground cover by weeds and weed aboveground dry mass and linked to cover crop traits in the mixtures.
"

	uri <- "doi:10.18167/DVN1/WPGRAM"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.25518/1780-4507.20220",
		project = "Siaam",
		design = "CRD",
		data_type = "experiment",
		treatment_vars = "crop;intercrops;species_number_",
		response_vars = "yield;dmy_total;dmy_leaves;dmy_stems;weed_biomass;ground_cover;weed_cover;plant_height",
		carob_LLM = "Gemini 4 Neon",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_completion = 92,
		carob_effort = 0
	)

	code_crop <- c(
		GUIAB = "niger",
		PHSAU = "mung bean",
		CNAEN = "jack bean",
		PESGL = "pearl millet",
		AVESG = "black oats",
		BRSCA = "carinata",
		VICVI = "hairy vetch"
	)

	sp_re <- "_(GUIAB|PHSAU|CNAEN|PESGL|AVESG|BRSCA|VICVI)$"
	as_excel_date <- function(x) as.character(as.Date(as.numeric(x), origin="1899-12-30"))

	build_season <- function(season, season_std, trt_bn, obs_bn, sdl_bn, itk_bn) {
		r1 <- read.csv(ff[basename(ff) == trt_bn], sep=";", check.names=FALSE)
		r2 <- read.csv(ff[basename(ff) == obs_bn], sep=";", check.names=FALSE)
		r3 <- read.csv(ff[basename(ff) == sdl_bn], sep=";", check.names=FALSE)
		r4 <- carobiner::read.excel(ff[basename(ff) == itk_bn], col_types="text")

		# Fix two raw data-entry errors in Summer_observation.csv
		if (season == "summer") {
			# Rows 54-163 are the CNAEN Plant_height_cm block (22 plots x 5 dates); every 3rd row from row 69 was mislabeled _GUIAB
			idx_cnaen <- seq_len(nrow(r2)) >= 54L & seq_len(nrow(r2)) <= 163L &
				r2$Variable == "Plant_height_cm" & grepl("_GUIAB$", r2$UNIT_CODE)
			r2$UNIT_CODE[idx_cnaen] <- sub("_GUIAB$", "_CNAEN", r2$UNIT_CODE[idx_cnaen])
			# Remove duplicate L6_P5_Cover_crop rows in the P1 monoculture block (copies of L6_P5_GUIAB)
			dup_cov <- r2$UNIT_CODE == "L6_P5_Cover_crop" & r2$Variable == "Plant_coverage_%" &
				duplicated(paste(r2$UNIT_CODE, r2$OBS_DATE, r2$Variable))
			r2 <- r2[!dup_cov, ]
		}
		if (season == "winter") {
			# Winter GUIAB leaf dry matter content was entered as a 0-1 fraction rather than %
			idx_frac <- r2$Variable == "Leaves_sample_dry_matter_content_%" & as.numeric(r2$Value) < 1
			r2$Value[idx_frac] <- as.numeric(r2$Value[idx_frac]) * 100
		}

		# Trial-level management from *_itk.xlsx
		itk_t <- r4[r4$unit_code == "Trial", ]
		sow_date <- as_excel_date(itk_t$date[itk_t$Variable == "Seed_sowing_method"][1])
		har_date <- as_excel_date(itk_t$date[itk_t$Variable == "Harvest_method"][1])
		fert_date <- as_excel_date(itk_t$date[itk_t$Variable == "Fertilizer_N_amount_kg_ha"][1])
		n_rate <- as.numeric(itk_t$Value[itk_t$Variable == "Fertilizer_N_amount_kg_ha"][1])
		irr_int <- as.integer(itk_t$Value[itk_t$Variable == "Irrigation_frequency_days"][1])
		till_dates <- as_excel_date(itk_t$date[itk_t$Variable == "Soil_tillage_method"])
		till_date_str <- paste(till_dates, collapse=";")
		till_times <- length(till_dates)

		# Treatment metadata per plot
		mix <- r1[r1$factor == "cc_mixture", c("unit_code", "factor_level")]
		nsp <- r1[r1$factor == "nb_cc_species", c("unit_code", "factor_level")]
		names(mix) <- c("plot_id", "mixture")
		names(nsp) <- c("plot_id", "nsp")
		trt <- merge(nsp, mix, by="plot_id", all=TRUE)
		trt$species_number_ <- as.integer(sub("sp", "", trt$nsp))
		trt$treatment <- ifelse(is.na(trt$mixture), trt$nsp, trt$mixture)

		# Build wide base rows: 1 row per sown component species (from seedlot) + 1 row per observed bare control plot
		sp_rows <- r3[r3$lot_code %in% names(code_crop), c("unit_code", "lot_code")]
		sp_df <- data.frame(
			unit_code = sp_rows$unit_code,
			plot_id = sub(sp_re, "", sp_rows$unit_code),
			sp_code = sp_rows$lot_code,
			crop = unname(code_crop[sp_rows$lot_code])
		)
		obs_bare <- sort(unique(r2$UNIT_CODE[grepl("^L[0-9]+_T[0-9]+$", r2$UNIT_CODE)]))
		bare_df <- data.frame(
			unit_code = obs_bare,
			plot_id = obs_bare,
			sp_code = NA_character_,
			crop = "none"
		)
		base <- rbind(sp_df, bare_df)
		base <- merge(base, trt[, c("plot_id", "species_number_", "treatment")], by="plot_id", all.x=TRUE)

		# Partner crops (intercrops) for mixture plots
		base$intercrops <- "none"
		mix_plots <- unique(base$plot_id[base$species_number_ > 1L])
		for (pid in mix_plots) {
			idx <- which(base$plot_id == pid)
			crops_p <- base$crop[idx]
			for (k in idx) {
				other <- crops_p[crops_p != base$crop[k]]
				base$intercrops[k] <- paste(other, collapse=";")
			}
		}

		# Seed rate (kg/ha): only valid in Summer_itk.xlsx (Winter_itk.xlsx repeats Summer rows)
		base$seed_rate <- ifelse(base$species_number_ == 0L, 0, NA_real_)
		if (season == "summer") {
			sr <- r4[r4$Variable == "Seed_sowing_density_kgha", c("unit_code", "Value")]
			m_sr <- match(base$unit_code, sr$unit_code)
			base$seed_rate[!is.na(m_sr)] <- as.numeric(sr$Value[m_sr[!is.na(m_sr)]])
		}

		# Extract single-date harvest observations by UNIT_CODE
		get_var <- function(vname) {
			sub <- r2[r2$Variable == vname, c("UNIT_CODE", "Value")]
			m <- match(base$unit_code, sub$UNIT_CODE)
			as.numeric(sub$Value[m])
		}
		fm <- get_var("Aboveground_fresh_mass_kg_m2") * 10000
		dm_pct <- get_var("Aboveground_sample_dry_matter_content_%")
		ldm_pct <- get_var("Leaves_sample_dry_matter_content_%")
		sdm_pct <- get_var("Stem_sample_dry_matter_content_%")
		leaf_frac <- get_var("Leaves_to_aboveground_dry_mass_fraction_sample")
		sla <- get_var("Specific_leaf_area_cm2_g")

		is_bare <- base$species_number_ == 0L
		dmy_tot <- ifelse(is_bare, NA_real_, fm * dm_pct / 100)

		# Plot-level weed biomass (fresh and dry kg/ha) from _weed units on cover-crop plots and L#_T# on bare plots
		w_fm_raw <- r2[r2$Variable == "Aboveground_fresh_mass_kg_m2" &
			(grepl("_weed$", r2$UNIT_CODE) | grepl("^L[0-9]+_T[0-9]+$", r2$UNIT_CODE)), c("UNIT_CODE", "Value")]
		w_dm_raw <- r2[r2$Variable == "Aboveground_sample_dry_matter_content_%" &
			(grepl("_weed$", r2$UNIT_CODE) | grepl("^L[0-9]+_T[0-9]+$", r2$UNIT_CODE)), c("UNIT_CODE", "Value")]
		w_bio <- merge(
			data.frame(plot_id = sub("_weed$", "", w_fm_raw$UNIT_CODE), weed_biomass = as.numeric(w_fm_raw$Value) * 10000),
			data.frame(plot_id = sub("_weed$", "", w_dm_raw$UNIT_CODE), w_dm_pct = as.numeric(w_dm_raw$Value)),
			by = "plot_id", all = TRUE
		)
		w_bio$weed_dmy_ <- w_bio$weed_biomass * w_bio$w_dm_pct / 100
		m_w <- match(base$plot_id, w_bio$plot_id)

		d <- data.frame(
			trial_id = season,
			season = season_std,
			plot_id = base$plot_id,
			block_id = sub("_.*", "", base$plot_id),
			unit_code_ = base$unit_code,
			# Négrier et al. 2023 Methods: cover-crop plots 2 m x 4 m (8 m2); bare weed-control plots 2 m x 1.5 m (3 m2)
			plot_area = ifelse(is_bare, 3, 8),
			plot_length = ifelse(is_bare, 2, 4),
			plot_width = ifelse(is_bare, 1.5, 2),
			treatment = base$treatment,
			species_number_ = base$species_number_,
			crop = base$crop,
			intercropped = base$species_number_ > 1L,
			intercrops = base$intercrops,
			intercrop_type = ifelse(base$species_number_ > 1L, "mixed", ifelse(base$species_number_ == 1L, "monocrop", "none")),
			intercrop_fraction = ifelse(is_bare, NA_real_, 1 / base$species_number_),
			planting_date = sow_date,
			harvest_date = har_date,
			planting_method = ifelse(is_bare, "none", "broadcasting"),
			seed_rate = base$seed_rate,
			land_prep_method = "rotovating",
			land_prep_times = as.integer(till_times),
			land_prep_date = till_date_str,
			fertilizer_used = TRUE,
			fertilizer_date = fert_date,
			N_fertilizer = n_rate,
			irrigated = TRUE,
			irrigation_interval = irr_int,
			weeding_done = FALSE,
			yield = ifelse(is_bare, NA_real_, fm),
			yield_part = "aboveground biomass",
			yield_isfresh = TRUE,
			yield_moisture = ifelse(is_bare, NA_real_, 100 - dm_pct),
			dmy_total = dmy_tot,
			dmy_leaves = dmy_tot * leaf_frac,
			dmy_stems = dmy_tot * (1 - leaf_frac),
			leaves_moisture = 100 - ldm_pct,
			stems_moisture = 100 - sdm_pct,
			leaf_mass_fraction_ = leaf_frac,
			specific_leaf_area_ = sla,
			# Plot-level weed fresh weight (weed_biomass) and dry weight (weed_dmy_) shared across component species in mixtures
			weed_biomass = w_bio$weed_biomass[m_w],
			weed_dmy_ = w_bio$weed_dmy_[m_w]
		)
		d <- d[order(d$plot_id, d$crop), ]

		# Long table: per-species height and cover, plot-level weed and total cover-crop cover, and site-level soil layers
		ph <- r2[r2$Variable == "Plant_height_cm", ]
		l_ph <- data.frame(
			unit_code_ = ph$UNIT_CODE,
			date = as.character(as.Date(ph$OBS_DATE, "%d/%m/%Y")),
			plant_height = as.numeric(ph$Value)
		)
		cv <- r2[r2$Variable == "Plant_coverage_%", ]
		cv$date <- as.character(as.Date(cv$OBS_DATE, "%d/%m/%Y"))
		cv$val <- as.numeric(cv$Value)

		# Per-species ground cover
		cv_sp <- cv[grepl(sp_re, cv$UNIT_CODE), ]
		l_sp <- data.frame(
			unit_code_ = cv_sp$UNIT_CODE,
			date = cv_sp$date,
			ground_cover = cv_sp$val
		)
		l_crop <- merge(l_ph, l_sp, by=c("unit_code_", "date"), all=TRUE)

		# Plot-level weed cover (_weed on cover-crop plots and L#_T# on bare plots) and total cover-crop cover (_Cover_crop)
		cv_w <- cv[grepl("_weed$", cv$UNIT_CODE) | grepl("^L[0-9]+_T[0-9]+$", cv$UNIT_CODE), ]
		l_w <- data.frame(
			plot_id = sub("_weed$", "", cv_w$UNIT_CODE),
			date = cv_w$date,
			weed_cover = cv_w$val
		)
		cv_cc <- cv[grepl("_Cover_crop$", cv$UNIT_CODE), ]
		l_cc <- data.frame(
			plot_id = sub("_Cover_crop$", "", cv_cc$UNIT_CODE),
			date = cv_cc$date,
			cover_crop_cover_ = cv_cc$val
		)
		l_plot <- merge(l_w, l_cc, by=c("plot_id", "date"), all=TRUE)
		# Attach plot-level cover observations to the first unit_code_ of each plot_id (avoids repeating across mixture rows)
		first_u <- d[!duplicated(d$plot_id), c("plot_id", "unit_code_")]
		l_plot <- merge(l_plot, first_u, by="plot_id", all.x=TRUE)
		l_plot$plot_id <- NULL

		l_obs <- merge(l_crop, l_plot, by=c("unit_code_", "date"), all=TRUE)
		l_obs$trial_id <- season
		l_obs$plot_id <- d$plot_id[match(l_obs$unit_code_, d$unit_code_)]

		# Site-level soil chemistry (0-15 cm and 15-30 cm)
		sr2 <- r2[grepl("^Soil_layer_", r2$UNIT_CODE), ]
		s_date <- as.character(as.Date(sr2$OBS_DATE[1], "%d/%m/%Y"))
		get_soil <- function(layer, vname) {
			as.numeric(sr2$Value[sr2$UNIT_CODE == layer & sr2$Variable == vname][1])
		}
		layers <- c("Soil_layer_0_15_cm", "Soil_layer_15_30_cm")
		s_exch <- vapply(layers, get_soil, numeric(1), vname="S_cmol_kg_dry_soil")
		s_cec <- vapply(layers, get_soil, numeric(1), vname="CEC_cmol_kg_dry_soil")
		l_soil <- data.frame(
			trial_id = season,
			unit_code_ = d$unit_code_[1],
			plot_id = NA_character_,
			date = s_date,
			depth_top = c(0, 15),
			depth_bottom = c(15, 30),
			soil_pH = vapply(layers, get_soil, numeric(1), vname="pH_H20"),
			soil_pH_KCl = vapply(layers, get_soil, numeric(1), vname="pH_HKCL"),
			# Corg_g_kg_dry_soil -> % (/ 10); Norg_g_kg_dry_soil -> mg/kg (* 1000)
			soil_SOC = vapply(layers, get_soil, numeric(1), vname="Corg_g_kg_dry_soil") / 10,
			soil_N_total = vapply(layers, get_soil, numeric(1), vname="Norg_g_kg_dry_soil") * 1000,
			soil_N_min_potential_ = vapply(layers, get_soil, numeric(1), vname="Nmin_kg_ha_year"),
			soil_P = vapply(layers, get_soil, numeric(1), vname="P_mg_kg_dry_soil"),
			soil_K_exch = vapply(layers, get_soil, numeric(1), vname="K_cmol_kg_dry_soil"),
			soil_Ca_exch = vapply(layers, get_soil, numeric(1), vname="Ca_cmol_kg_dry_soil"),
			soil_Mg_exch = vapply(layers, get_soil, numeric(1), vname="Mg_cmol_kg_dry_soil"),
			soil_Na_exch = vapply(layers, get_soil, numeric(1), vname="Na_cmol_kg_dry_soil"),
			soil_CEC = s_cec,
			# S_cmol_kg_dry_soil = Ca + Mg + K + Na (sum of exchangeable bases) -> base saturation (%)
			soil_BS = 100 * s_exch / s_cec
		)

		list(d=d, l_obs=l_obs, l_soil=l_soil)
	}

	s1 <- build_season("summer", "wet", "Summer_treatment.csv", "Summer_observation.csv", "Summer_seedlot.csv", "Summer_itk.xlsx")
	s2 <- build_season("winter", "dry", "Winter_treatment.csv", "Winter_observation.csv", "Winter_seedlot.csv", "Winter_itk.xlsx")

	d <- rbind(s1$d, s2$d)
	d$record_id <- seq_len(nrow(d))

	# Map (trial_id, unit_code_) to record_id for long table, then drop helper column
	key <- paste(d$trial_id, d$unit_code_)
	l_obs <- rbind(s1$l_obs, s2$l_obs)
	l_obs$record_id <- d$record_id[match(paste(l_obs$trial_id, l_obs$unit_code_), key)]
	l_obs$unit_code_ <- NULL

	l_soil <- rbind(s1$l_soil, s2$l_soil)
	l_soil$record_id <- d$record_id[match(paste(l_soil$trial_id, l_soil$unit_code_), key)]
	l_soil$unit_code_ <- NULL

	d$unit_code_ <- NULL
	long <- carobiner::bindr(l_obs, l_soil)

	# Daily weather (Meteo.csv) and station coordinates
	r4 <- read.csv(ff[basename(ff) == "Meteo.csv"], sep=";", check.names=FALSE)
	pos <- gsub("[()]", "", r4$Position)
	sp <- strsplit(pos, ",")
	wth <- data.frame(
		country = "Réunion",
		location = "Saint-Pierre",
		station_name = "Bassin Plat",
		date = as.character(as.Date(r4$Date, "%d/%m/%Y")),
		prec = as.numeric(r4$PPT),
		# RAD is in J/cm2/day -> * 10 for kJ/m2/day
		srad = as.numeric(r4$RAD) * 10,
		tmin = as.numeric(r4$Tmin),
		temp = as.numeric(r4$Tmean),
		tmax = as.numeric(r4$Tmax),
		ETP = as.numeric(r4$ETP),
		longitude = as.numeric(vapply(sp, `[`, character(1), 1)),
		latitude = as.numeric(vapply(sp, `[`, character(1), 2)),
		elevation = as.numeric(vapply(sp, `[`, character(1), 3)),
		geo_from_source = TRUE
	)

	d$country <- "Réunion"
	d$location <- "Saint-Pierre"
	# Négrier et al. 2023 Methods: Bassin Plat CIRAD experimental station, Andic Cambisol (WRB)
	d$site <- "Bassin Plat"
	d$soil_type <- "Andic Cambisol"
	d$longitude <- wth$longitude[1]
	d$latitude <- wth$latitude[1]
	d$elevation <- wth$elevation[1]
	d$geo_from_source <- TRUE
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d, long=long, wth=wth)
}
