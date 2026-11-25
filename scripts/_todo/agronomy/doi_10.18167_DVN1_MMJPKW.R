# R script for "carob"
# license: GPL (>=3)

## NOTES
# Multi-site cereal-legume intercropping dataset across 4 West African research stations (Saria, Burkina Faso; N'Tarla, Mali; Bambey and Sob, Senegal; 2017-2021; De Freitas et al., 2026, doi:10.1016/j.fcr.2026.110468).
# Saria and Sob experimental units are 3-replicate treatment means (with SDs where measured); Bambey and N'Tarla are single replicates. All values in Crop_and_soil_measurements are field observations (no simulated yields in this workbook).
# Crop cultivar "millet_souna3" is pearl millet (Souna 3). Each intercropping Experimental_unit has two rows in wide d (plant_1 and plant_2) sharing plot_id and linked to long records via record_id.
# Wide yield and dmy_total are taken from final/maximum measured mafruit and masec_n (t/ha * 1000 -> kg/ha); phenology dates/days (emergence, flowering, maturity, harvest) and final grain N / stem / leaf dry matter are extracted from Crop_and_soil_measurements (read with col_types="text" so readxl does not coerce sparse numeric columns such as ilevs, irecs, CNgrain, and vapour_pressure to logical).
# Crop management sheet provides N_fertilizer (sum of absolute_value kg N/ha; 0 when no N applied), N_splits, fertilizer_date, fertilizer_type (1=AN, 3=urea), planting_date (iplt0 DOY), planting_depth (profsem), plant_density (densitesem * 10000), row_spacing (interrang * 100), tillage_depth (proftrav), land_prep_date (jultrav), irrigated and irrigation_amount (sum of amount mm), and organic residue additions (qres t/ha fresh sorghum straw * 1000 -> OM_amount). P and K rates are not in the management sheet.
# Topsoil properties (Layer 1 from Soil sheet: soil_clay = argi %, soil_N_total = norg % * 10000 -> mg/kg, soil_pH, soil_bd = DAF g/cm3, soil_PWP = HMINF %, soil_FC = HCCF %) are joined to wide d; time-series canopy height (m * 100 -> cm), LAI, aboveground/grain/stem/leaf biomass, plant/grain N uptake and concentration, leaf number, rooting depth, and measured profile soil water (resmes, mm) are stored in long linked via record_id.
# Daily weather (Weather sheet) includes tmin, tmax, prec, srad (global_radiation MJ/m2 * 1000 -> kJ/m2), wspd (m/s), and vapr (vapour_pressure mbars = hPa), with site coordinates from Site_location.

## ISSUES
# P_fertilizer and K_fertilizer are not recorded in the Crop management sheet (missing variables).
# Yield moisture basis (fresh vs dry) is not explicitly specified for mafruit; yield_moisture is omitted.
# Some experimental units (e.g., biomass/soil-water-only units) have no grain yield measurement (NA detected: yield).
# Early-season canopy heights in the long table are below 10 cm (down to 4.8 cm); kept as measured.
# Only topsoil Layer 1 of the 5-layer STICS Soil sheet is joined to wide d (argi, norg, and pH are constant across layers for each soil profile; layer-varying HCCF/HMINF/epc below Layer 1 are not expanded to avoid duplicating soil columns across wide and long).

## NEW VARIABLES
# grain_number_: number of grains per square meter (grains/m2)
# grain_N_uptake_: nitrogen accumulated in harvested grain (kg N/ha)
# plant_N_uptake_: total nitrogen taken up by the aboveground plant (kg N/ha)
# plant_N_pct_: nitrogen concentration in aboveground plant (% dry weight)
# leaf_number_: number of leaves on main stem
# rooting_depth_: maximum rooting depth (cm)
# soil_water_: measured profile soil water reserve (mm)
# grain_biomass_: time-series grain biomass (kg/ha)
# aboveground_biomass_: time-series aboveground dry biomass (kg/ha)
# grain_biomass_sd_: standard deviation of grain biomass across replicates (kg/ha)
# aboveground_biomass_sd_: standard deviation of aboveground biomass across replicates (kg/ha)
# LAI_sd_: standard deviation of LAI across replicates
# plant_N_uptake_sd_: standard deviation of plant N uptake (kg N/ha)

## NOT PROCESSED
# Soil sheet: albedo, CsurNsol, and subsoil layers 2-5 thickness/HCCF/HMINF/DAF.
# Crop management sheet: orientrang (row orientation in radians), profmes (soil water measurement depth), Crespc, CsurNres, Nminres, eaures, profres.
# Crop_and_soil_measurements: ilaxs (DOY of max LAI), idrps (DOY of start of grain filling), QNrac, QNrac_sd, CNgrain_sd.
# Weather sheet: CO2_content (atmospheric CO2 ppm).
# Table_description sheet and readme.txt: metadata/codebooks, not observation tables.


carob_script <- function(path) {

"
Data for 'The STICS intercrop model accurately reproduces interspecific interactions and yield in  semi-arid West African cereal-legume intercropping systems'

The dataset contains experimental data on cereal-legume intercropping from four sites in West Africa (two sites in Senegal, Mali and Burkina Faso) for crop modelling calibration and use. The dataset contains observed data on sorghum, millet, and cowpea in sole crop and intercrops, soil properties and observed soil water content, management data and weather data.
"

	uri <- "doi:10.18167/DVN1/MMJPKW"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.fcr.2026.110468",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "crop;variety;intercrops;N_fertilizer;irrigated",
		response_vars = "yield;dmy_total;LAI;plant_height;grain_N;soil_water_",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "Database_DeFreitas_et_al_2026.xlsx"]
	r_eu   <- carobiner::read.excel(f, sheet = "Experimental_unit", col_types = "text")
	r_loc  <- carobiner::read.excel(f, sheet = "Site_location", col_types = "text")
	r_soil <- carobiner::read.excel(f, sheet = "Soil", col_types = "text")
	r_cm   <- carobiner::read.excel(f, sheet = "Crop management", col_types = "text")
	r_obs  <- carobiner::read.excel(f, sheet = "Crop_and_soil_measurements", col_types = "text")
	r_wth  <- carobiner::read.excel(f, sheet = "Weather", col_types = "text")

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "na")] <- NA_character_
		as.numeric(x)
	}
	as_obs_date <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "na")] <- NA_character_
		is_ser <- grepl("^[0-9]+(\\.[0-9]+)?$", x)
		out <- x
		if (any(is_ser)) {
			out[is_ser] <- as.character(as.Date(as.numeric(x[is_ser]), origin = "1899-12-30"))
		}
		out
	}
	doy_to_date <- function(yr, doy) {
		ok <- !is.na(yr) & !is.na(doy) & doy >= 1 & doy <= 366
		out <- rep(NA_character_, length(yr))
		out[ok] <- as.character(as.Date(paste(yr[ok], as.integer(doy[ok])), format = "%Y %j"))
		out
	}

	# Experimental units
	r_eu$crop_raw <- sub("_.*", "", trimws(r_eu$crop_cultivar))
	r_eu$crop <- ifelse(r_eu$crop_raw == "millet", "pearl millet", r_eu$crop_raw)
	r_eu$variety <- sub("^[^_]+_", "", trimws(r_eu$crop_cultivar))
	r_eu$plant <- ifelse(r_eu$System == "Intercropping" & r_eu$crop == "cowpea", "plant_2", "plant_1")
	r_eu$year <- as.integer(sub(".*_", "", trimws(r_eu$weather)))
	r_eu$trial_id <- paste(r_eu$site, r_eu$year, sep = "_")
	r_eu$plot_id <- paste(r_eu$site, r_eu$Experimental_unit, sep = "_")

	partners <- split(r_eu$crop, paste(r_eu$site, r_eu$Experimental_unit))
	r_eu$intercrops <- mapply(function(key, crop) {
		x <- unique(partners[[key]])
		x <- x[x != crop]
		if (length(x) == 0) "none" else paste(x, collapse = ";")
	}, paste(r_eu$site, r_eu$Experimental_unit), r_eu$crop, USE.NAMES = FALSE)
	r_eu$intercropped <- r_eu$System == "Intercropping"

	# Crop management aggregation per crop_management key (case-insensitive to match "n1_PL_F1_2019" vs "n1_Pl_F1_2019")
	r_eu$cm_key <- tolower(trimws(r_eu$crop_management))
	r_cm$cm_key <- tolower(trimws(r_cm$crop_management))
	n_col <- grep("^absolute_value", names(r_cm), value = TRUE)[1]
	r_cm$Nrate <- num(r_cm[[n_col]])
	r_cm$irr_amt <- num(r_cm$amount)
	r_cm$qres_num <- num(r_cm$qres)
	# Sheet comment: 1 AN, 2 UAN, 3 urea, 4 anhydrous ammonia, 5 DAS, 6 DAP/MAP, 7 CN
	eng_map <- c("1" = "AN", "2" = "UAN", "3" = "urea", "4" = "NH3", "5" = "DAS", "6" = "DAP", "7" = "CN")
	r_cm$ftype <- unname(eng_map[trimws(as.character(r_cm$engrais))])

	cm_keys <- unique(r_cm$cm_key)
	cm_list <- lapply(cm_keys, function(k) {
		sub <- r_cm[r_cm$cm_key == k, ]
		one_n <- function(v) {
			u <- unique(num(v))
			u <- u[!is.na(u)]
			if (length(u) >= 1) u[1] else NA_real_
		}
		n_vals <- sub$Nrate[!is.na(sub$Nrate)]
		n_tot <- if (length(n_vals) == 0) 0 else sum(n_vals)
		n_spl <- as.integer(sum(!is.na(sub$Nrate) & sub$Nrate > 0))
		ft <- unique(sub$ftype[!is.na(sub$ftype) & nzchar(sub$ftype)])
		irr_vals <- sub$irr_amt[!is.na(sub$irr_amt)]
		irr_tot <- if (length(irr_vals) == 0) 0 else sum(irr_vals)
		qres_vals <- sub$qres_num[!is.na(sub$qres_num)]
		om_amt <- if (length(qres_vals) == 0) 0 else sum(qres_vals) * 1000
		res_tp <- unique(trimws(sub[["Residual type"]][!is.na(sub[["Residual type"]]) & nzchar(trimws(sub[["Residual type"]]))]))
		jn <- sort(unique(as.integer(num(sub$julapN)[!is.na(num(sub$julapN))])))
		data.frame(
			cm_key = k,
			iplt0 = one_n(sub$iplt0),
			planting_depth = one_n(sub$profsem),
			plant_density = one_n(sub$densitesem) * 10000,
			row_spacing = one_n(sub$interrang) * 100,
			jultrav = one_n(sub$jultrav),
			tillage_depth = one_n(sub$proftrav),
			N_fertilizer = n_tot,
			N_splits = n_spl,
			fertilizer_used = n_tot > 0,
			fertilizer_type = if (n_tot == 0) "none" else if (length(ft) > 0) paste(ft, collapse = ";") else "unknown",
			julapN_str = if (length(jn) > 0) paste(jn, collapse = ";") else NA_character_,
			irrigated = irr_tot > 0,
			irrigation_amount = irr_tot,
			OM_used = om_amt > 0,
			OM_type = if (om_amt > 0 && length(res_tp) > 0) "sorghum straw" else "none",
			OM_amount = om_amt
		)
	})
	cm_df <- do.call(rbind, cm_list)

	# Topsoil properties (Soil_Layers == 1)
	s1 <- r_soil[trimws(r_soil$Soil_Layers) == "1", ]
	s1 <- s1[!duplicated(s1$soil_name), ]
	soil_df <- data.frame(
		soil_name = s1$soil_name,
		depth_top = 0,
		depth_bottom = num(s1$epc),
		soil_clay = num(s1$argi),
		# norg is % dry soil -> * 10000 for mg/kg
		soil_N_total = num(s1$norg) * 10000,
		soil_pH = num(s1$pH),
		soil_bd = num(s1$DAF),
		soil_PWP = num(s1$HMINF),
		soil_FC = num(s1$HCCF)
	)

	r_eu <- merge(r_eu, cm_df, by = "cm_key", all.x = TRUE)
	r_eu <- merge(r_eu, r_loc, by = "site", all.x = TRUE)
	r_eu <- merge(r_eu, soil_df, by = "soil_name", all.x = TRUE)

	# Parse Crop_and_soil_measurements
	r_obs$date <- as_obs_date(r_obs$Date)
	r_obs$mafruit <- num(r_obs$mafruit)
	r_obs$masec_n <- num(r_obs$masec_n)
	r_obs$hauteur <- num(r_obs$hauteur)
	r_obs$lai_n   <- num(r_obs$lai_n)
	r_obs$resmes  <- num(r_obs$resmes)
	r_obs$ilevs   <- num(r_obs$ilevs)
	r_obs$iflos   <- num(r_obs$iflos)
	r_obs$imats   <- num(r_obs$imats)
	r_obs$irecs   <- num(r_obs$irecs)
	r_obs$chargefruit <- num(r_obs$chargefruit)
	r_obs$matigestruc_kg_ha <- num(r_obs$matigestruc_kg_ha)
	r_obs$mafeuil_kg_ha <- num(r_obs$mafeuil_kg_ha)
	r_obs$nbfeuille <- num(r_obs$nbfeuille)
	r_obs$QNplante <- num(r_obs$QNplante)
	r_obs$QNgrain  <- num(r_obs$QNgrain)
	r_obs$CNplante <- num(r_obs$CNplante)
	r_obs$CNgrain  <- num(r_obs$CNgrain)
	r_obs$zrac     <- num(r_obs$zrac)
	r_obs$masec_n_sd <- num(r_obs$masec_n_sd)
	r_obs$mafruit_sd <- num(r_obs$mafruit_sd)
	r_obs$QNplante_sd <- num(r_obs$QNplante_sd)
	r_obs$lai_n_sd <- num(r_obs$lai_n_sd)

	# Extract summary/final values per experimental unit x plant
	n_eu <- nrow(r_eu)
	r_eu$yield <- NA_real_
	r_eu$dmy_total <- NA_real_
	r_eu$grain_number_ <- NA_real_
	r_eu$grain_N <- NA_real_
	r_eu$grain_N_uptake_ <- NA_real_
	r_eu$emergence_date <- NA_character_
	r_eu$flowering_date <- NA_character_
	r_eu$maturity_date <- NA_character_
	r_eu$harvest_date <- NA_character_
	r_eu$emergence_days <- NA_real_
	r_eu$flowering_days <- NA_real_
	r_eu$maturity_days <- NA_real_
	r_eu$harvest_days <- NA_real_
	r_eu$keep <- FALSE

	for (i in seq_len(n_eu)) {
		m <- r_obs[r_obs$site == r_eu$site[i] & r_obs$Experimental_unit == r_eu$Experimental_unit[i] & r_obs$Plant == r_eu$plant[i], ]
		if (nrow(m) == 0) next
		r_eu$keep[i] <- TRUE
		yr_i <- r_eu$year[i]
		plt_doy <- r_eu$iplt0[i]

		# Phenology DOYs (ilevs, iflos, imats, irecs)
		ev <- m$ilevs[!is.na(m$ilevs)]
		if (length(ev) > 0) {
			r_eu$emergence_date[i] <- doy_to_date(yr_i, ev[1])
			if (!is.na(plt_doy)) r_eu$emergence_days[i] <- ev[1] - plt_doy
		}
		fl <- m$iflos[!is.na(m$iflos)]
		if (length(fl) > 0) {
			r_eu$flowering_date[i] <- doy_to_date(yr_i, fl[1])
			if (!is.na(plt_doy)) r_eu$flowering_days[i] <- fl[1] - plt_doy
		}
		mt <- m$imats[!is.na(m$imats)]
		if (length(mt) > 0) {
			r_eu$maturity_date[i] <- doy_to_date(yr_i, mt[1])
			if (!is.na(plt_doy)) r_eu$maturity_days[i] <- mt[1] - plt_doy
		}
		hv <- m$irecs[!is.na(m$irecs)]
		if (length(hv) > 0) {
			r_eu$harvest_date[i] <- doy_to_date(yr_i, hv[1])
			if (!is.na(plt_doy)) r_eu$harvest_days[i] <- hv[1] - plt_doy
		}

		# Grain yield & aboveground biomass at harvest (max mafruit, latest date on ties)
		ok <- which(!is.na(m$mafruit))
		if (length(ok) > 0) {
			best <- ok[m$mafruit[ok] == max(m$mafruit[ok])]
			best <- best[which.max(as.Date(m$date[best]))]
			r_eu$yield[i] <- m$mafruit[best] * 1000
			if (!is.na(m$masec_n[best])) {
				r_eu$dmy_total[i] <- m$masec_n[best] * 1000
			}
			if (is.na(r_eu$harvest_date[i])) r_eu$harvest_date[i] <- m$date[best]
		}
		if (is.na(r_eu$dmy_total[i]) && any(!is.na(m$masec_n))) {
			r_eu$dmy_total[i] <- max(m$masec_n, na.rm = TRUE) * 1000
		}
		if (any(!is.na(m$chargefruit))) r_eu$grain_number_[i] <- max(m$chargefruit, na.rm = TRUE)
		if (any(!is.na(m$QNgrain))) r_eu$grain_N_uptake_[i] <- max(m$QNgrain, na.rm = TRUE)
		if (any(!is.na(m$CNgrain))) {
			# CNgrain is % dry weight -> * 10 for mg/g
			r_eu$grain_N[i] <- max(m$CNgrain, na.rm = TRUE) * 10
		} else if (!is.na(r_eu$grain_N_uptake_[i]) && !is.na(r_eu$yield[i]) && r_eu$yield[i] > 0) {
			r_eu$grain_N[i] <- r_eu$grain_N_uptake_[i] / r_eu$yield[i] * 1000
		}
	}
	r_eu <- r_eu[r_eu$keep, ]
	r_eu <- r_eu[order(r_eu$site, r_eu$year, r_eu$Experimental_unit, r_eu$plant), ]

	cty <- ifelse(grepl("^Burkina", r_eu$site), "Burkina Faso",
		ifelse(grepl("^Mali", r_eu$site), "Mali",
		ifelse(grepl("^Senegal", r_eu$site), "Senegal", NA_character_)))

	# Convert fertilizer DOY strings to semicolon-separated dates
	fdate <- vapply(seq_len(nrow(r_eu)), function(i) {
		s <- r_eu$julapN_str[i]
		if (is.na(s) || !nzchar(s)) return(NA_character_)
		doys <- as.integer(strsplit(s, ";")[[1]])
		paste(doy_to_date(rep(r_eu$year[i], length(doys)), doys), collapse = ";")
	}, character(1))

	d <- data.frame(
		record_id = seq_len(nrow(r_eu)),
		trial_id = r_eu$trial_id,
		plot_id = r_eu$plot_id,
		crop = r_eu$crop,
		variety = r_eu$variety,
		treatment = r_eu$Experimental_unit,
		country = cty,
		location = sub("^[^_]+_", "", r_eu$site),
		latitude = num(r_eu$latitude),
		longitude = num(r_eu$longitude),
		geo_from_source = TRUE,
		station_id = r_eu$site,
		land_prep_date = doy_to_date(r_eu$year, r_eu$jultrav),
		tillage_depth = r_eu$tillage_depth,
		planting_date = doy_to_date(r_eu$year, r_eu$iplt0),
		planting_depth = r_eu$planting_depth,
		emergence_date = r_eu$emergence_date,
		emergence_days = r_eu$emergence_days,
		flowering_date = r_eu$flowering_date,
		flowering_days = r_eu$flowering_days,
		maturity_date = r_eu$maturity_date,
		maturity_days = r_eu$maturity_days,
		harvest_date = r_eu$harvest_date,
		harvest_days = r_eu$harvest_days,
		plant_density = r_eu$plant_density,
		row_spacing = r_eu$row_spacing,
		yield = r_eu$yield,
		yield_part = ifelse(r_eu$crop == "cowpea", "seed", "grain"),
		dmy_total = r_eu$dmy_total,
		grain_number_ = r_eu$grain_number_,
		grain_N = r_eu$grain_N,
		grain_N_uptake_ = r_eu$grain_N_uptake_,
		fertilizer_used = r_eu$fertilizer_used,
		fertilizer_type = r_eu$fertilizer_type,
		fertilizer_date = fdate,
		N_fertilizer = r_eu$N_fertilizer,
		N_splits = r_eu$N_splits,
		OM_used = r_eu$OM_used,
		OM_type = r_eu$OM_type,
		OM_amount = r_eu$OM_amount,
		intercrops = r_eu$intercrops,
		intercropped = r_eu$intercropped,
		irrigated = r_eu$irrigated,
		irrigation_amount = r_eu$irrigation_amount,
		depth_top = r_eu$depth_top,
		depth_bottom = r_eu$depth_bottom,
		soil_clay = r_eu$soil_clay,
		soil_N_total = r_eu$soil_N_total,
		soil_pH = r_eu$soil_pH,
		soil_bd = r_eu$soil_bd,
		soil_PWP = r_eu$soil_PWP,
		soil_FC = r_eu$soil_FC,
		on_farm = FALSE,
		is_survey = FALSE
	)

	# Long table: time-series measurements linked via record_id
	key_d <- data.frame(
		record_id = d$record_id,
		trial_id = d$trial_id,
		plot_id = d$plot_id,
		site = r_eu$site,
		Experimental_unit = r_eu$Experimental_unit,
		Plant = r_eu$plant
	)
	obs_m <- merge(r_obs, key_d, by = c("site", "Experimental_unit", "Plant"))
	dlong <- data.frame(
		record_id = obs_m$record_id,
		trial_id = obs_m$trial_id,
		plot_id = obs_m$plot_id,
		date = obs_m$date,
		plant_height = obs_m$hauteur * 100,
		LAI = obs_m$lai_n,
		LAI_sd_ = obs_m$lai_n_sd,
		grain_biomass_ = obs_m$mafruit * 1000,
		grain_biomass_sd_ = obs_m$mafruit_sd * 1000,
		aboveground_biomass_ = obs_m$masec_n * 1000,
		aboveground_biomass_sd_ = obs_m$masec_n_sd * 1000,
		dmy_stems = obs_m$matigestruc_kg_ha,
		dmy_leaves = obs_m$mafeuil_kg_ha,
		leaf_number_ = obs_m$nbfeuille,
		plant_N_uptake_ = obs_m$QNplante,
		plant_N_uptake_sd_ = obs_m$QNplante_sd,
		plant_N_pct_ = obs_m$CNplante,
		rooting_depth_ = obs_m$zrac,
		soil_water_ = obs_m$resmes
	)
	vcols <- setdiff(names(dlong), c("record_id", "trial_id", "plot_id", "date"))
	dlong <- dlong[!is.na(dlong$date) & rowSums(!is.na(dlong[, vcols])) > 0, ]

	# Daily weather with all confirmed units from Table_description
	r_loc$country <- ifelse(grepl("^Burkina", r_loc$site), "Burkina Faso",
		ifelse(grepl("^Mali", r_loc$site), "Mali",
		ifelse(grepl("^Senegal", r_loc$site), "Senegal", NA_character_)))
	wth <- data.frame(
		station_id = r_wth$site,
		station_name = r_wth$name,
		date = as.character(as.Date(paste(as.integer(num(r_wth$year)), as.integer(num(r_wth$month)), as.integer(num(r_wth$day)), sep = "-"))),
		tmin = num(r_wth$minimum_temperature),
		tmax = num(r_wth$maximum_temperature),
		# global_radiation is MJ/m2/day -> * 1000 for kJ/m2
		srad = num(r_wth$global_radiation) * 1000,
		prec = num(r_wth$rainfall),
		wspd = num(r_wth$wind),
		# vapour_pressure is mbars (= hPa)
		vapr = num(r_wth$vapour_pressure)
	)
	wth <- wth[wth$station_id %in% d$station_id & !is.na(wth$date), ]
	wth <- merge(wth, data.frame(station_id = r_loc$site, country = r_loc$country,
		latitude = num(r_loc$latitude), longitude = num(r_loc$longitude), geo_from_source = TRUE),
		by = "station_id", all.x = TRUE)
	wth <- wth[!duplicated(wth[, c("station_id", "date")]), ]

	carobiner::write_files(path, meta, d, long = dlong, wth = wth)
}
