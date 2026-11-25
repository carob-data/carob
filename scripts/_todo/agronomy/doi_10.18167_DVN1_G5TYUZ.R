# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm maize-cowpea intercropping trials across 12 smallholder farms in semi-arid Zimbabwe over two rainfed seasons (Season 1: 2021-2022; Season 2: 2022-2023; Kwenda et al., 2025, doi:10.1016/j.fcr.2025.109890).
# Each farmer x season x treatment x crop record is built from Crop_management (including sole maize, sole cowpea CPA_PD1/CPA_PD2, unfertilized sole SC403 CONTR, and intercrops QPM_CPA, SC403_CPA, ZS500_CPA). Intercrops have one row per component crop sharing plot_id and linked to long records via record_id.
# CRITICAL SOURCE FIX: In the pLER_LER sheet, the four "aboveground biomass" column headers (cols 5-8) and four "grain yield" column headers (cols 10-13) are swapped relative to both the values and the pLER columns (cols 31-34): e.g., col12/col13 equals pLER for maize aboveground biomass (max diff 5e-15), and col7/col8 equals pLER for maize grain yield (max diff 1.4e-14). The script maps cols 5-8 to yield and cols 10-13 to dmy_total (Mg DM/ha * 1000 -> kg/ha, yield_moisture = 0). Sole cowpea in pLER_LER matches CPA_PD2 (same sowing date as intercropped cowpea).
# Fertilization is joined from the Fertilization sheet (Compound D 7-14-7 basal: 12 N, 10 P, 10 K kg/ha; ammonium nitrate top-dressing: 34.5 kg N/ha per split, 1 split for Season 1 intercrops and 2 splits for sole maize and Season 2 intercrops; 0 for CONTR). Note: prose notes in Fertilization state 200 kg/ha Compound D and 2 x 50 kg N/ha AN; the numeric application columns are used. No organic fertilizer was applied (Treatments sheet).
# Multi-depth soil profiles (0-120 cm in 20 cm increments from Soil_results; Ntotal g/kg * 1000 -> mg/kg; organic_Carbon g/kg / 10 -> %), periodic LAI and cumulative soil water (Soil_water_LAI), and biological N2 fixation / N uptake (N2-fixation) are stored in the long table linked via trial_id and plot_id (with measured_crop_ distinguishing maize vs cowpea in intercrops).
# Daily weather for all 9 stations in Climate_data is passed via wth (coordinates from Sites). Solar radiation is labeled "J/m2" in the header but values are 3.3-29.0, clearly MJ/m2/day, so converted (* 1000) to kJ/m2 for srad.

## ISSUES
# Swapped grain yield vs aboveground biomass headers in pLER_LER were corrected based on exact agreement with the pLER columns and biological plausibility.
# Crop_management had 7 Season 2 harvest_date values stored as Excel serial numbers (45036, 45045 -> 2023-04-20, 2023-04-29) and 5 Season 1 harvest_date rows (CHIU x 4, SMAN x 1) with an obvious year typo (2023 instead of 2022; CHIU was only in Season 1); both were corrected.
# Crop_management has one duplicate key (SCHI, Season 1, ZS500_CPA, maize: row 83 planted 2021-12-30 at 5 cm depth vs row 85 planted 2022-01-15 at 3 cm depth, matching cowpea sowing); row 83 is kept.
# CONTR and CPA_PD1 plots are not in pLER_LER, so yield and dmy_total are NA for those rows (they carry management, LAI, soil water, and N2-fixation observations).
# Model-estimated water-stress and nitrogen-stress indices (STICS outputs) and derived LER/pLER columns in pLER_LER are omitted (measured observations only).

## NEW VARIABLES
# measured_crop_: component crop (maize or cowpea) measured in the long table (distinguishes crops sharing plot_id in intercrops)
# soil_Ca_total_: total soil calcium content (mg/kg)
# soil_water_: cumulative profile soil moisture to the farmer's maximum measurement depth (mm)
# ndfa_: percentage of plant nitrogen derived from the atmosphere (%)
# N_uptake_: total crop nitrogen uptake (kg N/ha)

## NOT PROCESSED
# pLER_LER: STICS-simulated water stress and nitrogen stress indices (16 cols, model outputs); derived pLER and LER ratios (7 cols); redundant DOY/days-between-planting columns.
# Treatments: max_depth_for_soil_water_measurements per each farmer (160-170 cm metadata for soil_water_).
# Legend: codebook sheet, not an observation table.


carob_script <- function(path) {

"
Data for \"Intercrop overyielding is maintained under estimated water and nitrogen stress in maize-cowpea on-farm trials in semi-arid Zimbabwe\"

These are the raw data of the paper \"Intercrop overyielding is maintained under estimated water and nitrogen stress in maize-cowpea on-farm trials in semi-arid Zimbabwe\" authored by Illiana W. Kwenda, Gatien N. Falconnier, Rémi Cardinael, François Affholder, Antoine Couëdel, Frederic Baudron, Angelinus C. Franke, Isaiah Nyagumbo, Stanford Mabasa, Mathilde de Freitas, Valentin Pret, Souleymane Diop, Eleanor F. Magwaza, Regis Chikowo
"

	uri <- "doi:10.18167/DVN1/G5TYUZ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.fcr.2025.109890",
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;intercrops;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield;dmy_total;LAI;N_fixation;N_uptake_",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Kwenda_et_al_data_vf.xlsx"]
	r_sites <- carobiner::read.excel(f, sheet = "Sites", na = c("", "NA"))
	r_cm    <- carobiner::read.excel(f, sheet = "Crop_management", na = c("", "NA"))
	r_fert  <- carobiner::read.excel(f, sheet = "Fertilization", na = c("", "NA"))
	r_pler  <- carobiner::read.excel(f, sheet = "pLER_LER", na = c("", "NA"))
	r_soil  <- carobiner::read.excel(f, sheet = "Soil_results", na = c("", "NA"))
	r_n2    <- carobiner::read.excel(f, sheet = "N2-fixation", na = c("", "NA"))
	r_sw    <- carobiner::read.excel(f, sheet = "Soil_water_LAI", na = c("", "NA"))
	r_clim  <- carobiner::read.excel(f, sheet = "Climate_data", na = c("", "NA"))

	as_date <- function(x) {
		if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d"))
		if (inherits(x, "Date")) return(as.character(x))
		s <- trimws(as.character(x))
		is_ser <- grepl("^[0-9]{5}$", s)
		if (any(is_ser)) {
			s[is_ser] <- as.character(as.Date(as.numeric(s[is_ser]), origin = "1899-12-30"))
		}
		s
	}

	# Base table from Crop_management (one row per farmer x season x treatment x crop)
	r_cm$farmer <- trimws(as.character(r_cm$farmer))
	r_cm$season <- trimws(as.character(r_cm$Growing_season))
	r_cm$treatment <- gsub("\\s+", "", trimws(as.character(r_cm$treatment)))
	r_cm$crop <- tolower(trimws(as.character(r_cm$plant)))
	# SCHI Season 1 ZS500_CPA maize is duplicated (row 83: 5 cm depth, 2021-12-30; row 85: 3 cm depth, 2022-01-15 matching cowpea); keep first
	r_cm <- r_cm[!duplicated(r_cm[, c("farmer", "season", "treatment", "crop")]), ]

	# Normalize cultivar names (CONTR row has cultivar="CONTR" -> SC403 per Treatments sheet; QPM -> QPM 623)
	cult <- trimws(as.character(r_cm$cultivar))
	cult[cult == "CONTR"] <- "SC403"
	cult[cult == "QPM"] <- "QPM 623"

	is_ic <- grepl("_CPA$", r_cm$treatment)
	ic_partner <- ifelse(!is_ic, "none", ifelse(r_cm$crop == "maize", "cowpea", "maize"))

	# Fertilization joined on farmer x season x treatment
	r_fert$farmer <- trimws(as.character(r_fert$farmer))
	r_fert$treatment <- gsub("\\s+", "", trimws(as.character(r_fert$treatment)))
	d1_f <- as.Date(as_date(r_fert[["fertiliser application_date_ymd1"]]))
	r_fert$season <- ifelse(is.na(d1_f), NA_character_, ifelse(d1_f < as.Date("2022-07-01"), "Season 1", "Season 2"))

	n1 <- as.numeric(r_fert[["fertiliser_Nitrogen content in first application_1"]])
	p1 <- as.numeric(r_fert[["fertiliser_phosphorous content in first application_1"]])
	k1 <- as.numeric(r_fert[["fertiliser_ potassium content in first application_K1"]])
	n2 <- as.numeric(r_fert[["fertiliser nitrogen amount_N2"]])
	p2 <- as.numeric(r_fert[["fertiliser_phoshorous_amount2"]])
	k2 <- as.numeric(r_fert[["fertiliser_potassium content in second application_K2"]])
	n3 <- as.numeric(r_fert[["fertiliser_application_N amount_3"]])
	p3 <- as.numeric(r_fert[["fertiliser_phosphorous_amount_p3"]])
	k3 <- as.numeric(r_fert[["fertiliser_potasiumcontentent in third application_K3"]])

	fsum <- function(a, b, c) {
		ifelse(is.na(a) & is.na(b) & is.na(c), 0,
			ifelse(is.na(a), 0, a) + ifelse(is.na(b), 0, b) + ifelse(is.na(c), 0, c))
	}
	f_df <- data.frame(
		farmer = r_fert$farmer,
		season = r_fert$season,
		treatment = r_fert$treatment,
		N_fertilizer = fsum(n1, n2, n3),
		P_fertilizer = fsum(p1, p2, p3),
		K_fertilizer = fsum(k1, k2, k3),
		fertilizer_type = ifelse(is.na(r_fert$fertiliser_type1), "none",
			ifelse(is.na(r_fert$fertiliser_type2), "NPK", "NPK;AN")),
		fertilizer_used = !is.na(r_fert$fertiliser_type1)
	)
	# CONTR has no application dates (season NA); set 0 NPK directly for CONTR after merge
	f_df <- f_df[!is.na(f_df$season), ]
	f_df <- f_df[!duplicated(f_df[, c("farmer", "season", "treatment")]), ]

	# Extract yield and aboveground biomass from pLER_LER
	# NOTE: cols 5-8 ("aboveground biomass") are grain yields and cols 10-13 ("grain yield") are aboveground biomass (verified against pLER ratios)
	sn_p <- ifelse(tolower(trimws(r_pler[["growing season"]])) == "season 1", "Season 1", "Season 2")
	mv_p <- trimws(as.character(r_pler[["maize variety"]]))
	y_rows <- rbind(
		data.frame(
			farmer = trimws(as.character(r_pler$farmer)), season = sn_p,
			treatment = paste0(mv_p, "_CPA"), crop = "maize",
			yield = as.numeric(r_pler[[7]]) * 1000,
			dmy_total = as.numeric(r_pler[[12]]) * 1000
		),
		data.frame(
			farmer = trimws(as.character(r_pler$farmer)), season = sn_p,
			treatment = mv_p, crop = "maize",
			yield = as.numeric(r_pler[[8]]) * 1000,
			dmy_total = as.numeric(r_pler[[13]]) * 1000
		),
		data.frame(
			farmer = trimws(as.character(r_pler$farmer)), season = sn_p,
			treatment = paste0(mv_p, "_CPA"), crop = "cowpea",
			yield = as.numeric(r_pler[[5]]) * 1000,
			dmy_total = as.numeric(r_pler[[10]]) * 1000
		),
		data.frame(
			farmer = trimws(as.character(r_pler$farmer)), season = sn_p,
			treatment = "CPA_PD2", crop = "cowpea",
			yield = as.numeric(r_pler[[6]]) * 1000,
			dmy_total = as.numeric(r_pler[[11]]) * 1000
		)
	)
	# Sole cowpea (CPA_PD2) is repeated once per maize variety in pLER_LER; deduplicate
	y_rows <- y_rows[!duplicated(y_rows[, c("farmer", "season", "treatment", "crop")]), ]

	# Multi-depth soil profile depths from Soil_results (stored in long table to avoid wide/long duplicate columns)
	r_soil$farmer <- trimws(as.character(r_soil$farmer))
	r_soil$depth_bottom <- as.numeric(r_soil$layer_base_depth)
	r_soil$depth_top <- r_soil$depth_bottom - 20

	# Sites & weather stations
	r_sites$id_site <- trimws(as.character(r_sites$id_site))
	r_sites$id_soil <- trimws(as.character(r_sites$id_soil))
	wst_avail <- unique(trimws(as.character(r_clim$id_wst)))

	hd_cm <- as_date(r_cm$harvest_date)
	# Fix obvious year typo (2023 -> 2022) in 5 Season 1 harvest_date rows (CHIU x 4, SMAN x 1)
	fix_s1 <- r_cm$season == "Season 1" & grepl("^2023-", hd_cm)
	hd_cm[fix_s1] <- sub("^2023-", "2022-", hd_cm[fix_s1])

	m <- data.frame(
		farmer = r_cm$farmer,
		season = r_cm$season,
		treatment = r_cm$treatment,
		crop = r_cm$crop,
		variety = cult,
		variety_type = tolower(trimws(as.character(r_cm$cultivar_note))),
		intercrops = ic_partner,
		intercropped = is_ic,
		planting_date = as_date(r_cm$planting_date),
		harvest_date = hd_cm,
		# plants/m2 * 10000 -> plants/ha
		plant_density = as.numeric(r_cm$plant_population) * 10000,
		row_spacing = as.numeric(r_cm$interrow_spacing),
		plant_spacing = as.numeric(r_cm$inrow_spacing),
		planting_depth = as.numeric(r_cm$planting_depth)
	)
	m <- merge(m, y_rows, by = c("farmer", "season", "treatment", "crop"), all.x = TRUE)
	m <- merge(m, f_df, by = c("farmer", "season", "treatment"), all.x = TRUE)
	# Unfertilized control (CONTR) received 0 mineral fertilizer
	is_ctrl <- m$treatment == "CONTR"
	m$N_fertilizer[is_ctrl] <- 0
	m$P_fertilizer[is_ctrl] <- 0
	m$K_fertilizer[is_ctrl] <- 0
	m$fertilizer_type[is_ctrl] <- "none"
	m$fertilizer_used[is_ctrl] <- FALSE

	m <- merge(m, r_sites[, c("id_site", "country", "longitude", "latitude", "elevation")],
		by.x = "farmer", by.y = "id_site", all.x = TRUE)
	m <- m[order(m$farmer, m$season, m$treatment, m$crop), ]

	d <- data.frame(
		record_id = seq_len(nrow(m)),
		trial_id = paste(m$farmer, gsub(" ", "", m$season), sep = "_"),
		plot_id = paste(m$farmer, gsub(" ", "", m$season), m$treatment, sep = "_"),
		location = m$farmer,
		country = m$country,
		longitude = as.numeric(m$longitude),
		latitude = as.numeric(m$latitude),
		elevation = as.numeric(m$elevation),
		geo_from_source = TRUE,
		crop = m$crop,
		variety = m$variety,
		variety_type = m$variety_type,
		treatment = m$treatment,
		intercrops = m$intercrops,
		intercropped = m$intercropped,
		planting_date = m$planting_date,
		harvest_date = m$harvest_date,
		plant_density = m$plant_density,
		row_spacing = m$row_spacing,
		plant_spacing = m$plant_spacing,
		planting_depth = m$planting_depth,
		fertilizer_used = m$fertilizer_used,
		fertilizer_type = m$fertilizer_type,
		N_fertilizer = m$N_fertilizer,
		P_fertilizer = m$P_fertilizer,
		K_fertilizer = m$K_fertilizer,
		OM_used = FALSE,
		yield = m$yield,
		dmy_total = m$dmy_total,
		yield_part = ifelse(m$crop == "cowpea", "seed", "grain"),
		yield_moisture = 0,
		station_id = ifelse(m$farmer %in% wst_avail, m$farmer, "KWERE"),
		irrigated = FALSE,
		on_farm = TRUE,
		is_survey = FALSE
	)

	# Long table: periodic LAI, cumulative soil moisture, N2-fixation, N uptake, and multi-depth soil profiles
	key_d <- data.frame(
		trial_id = d$trial_id,
		plot_id = d$plot_id,
		farmer = m$farmer,
		season = m$season,
		treatment = m$treatment,
		crop = m$crop
	)

	sw_clean <- data.frame(
		date = as_date(r_sw$Date),
		farmer = trimws(as.character(r_sw$farmer)),
		season = trimws(as.character(r_sw$growing_season)),
		treatment = gsub("\\s+", "", trimws(as.character(r_sw$treatment))),
		crop = tolower(trimws(as.character(r_sw$Plant))),
		LAI = as.numeric(r_sw$leaf_area_index),
		soil_water_ = as.numeric(r_sw$cumulative_soil_moisture_mm_per_each_site)
	)
	sw_clean <- sw_clean[!(is.na(sw_clean$LAI) & is.na(sw_clean$soil_water_)), ]
	sw_clean <- merge(sw_clean, key_d, by = c("farmer", "season", "treatment", "crop"), all.x = TRUE)
	sw_clean <- sw_clean[!is.na(sw_clean$trial_id), ]

	n2_clean <- data.frame(
		date = as_date(r_n2$Date),
		farmer = trimws(as.character(r_n2$farmer)),
		season = trimws(as.character(r_n2$growing_season)),
		treatment = gsub("\\s+", "", trimws(as.character(r_n2$treatment))),
		crop = tolower(trimws(as.character(r_n2$Plant))),
		ndfa_ = as.numeric(r_n2[["%Ndfa"]]),
		N_fixation = as.numeric(r_n2$N2_fixation),
		N_uptake_ = as.numeric(r_n2$N_uptake)
	)
	n2_clean <- n2_clean[!(is.na(n2_clean$ndfa_) & is.na(n2_clean$N_fixation) & is.na(n2_clean$N_uptake_)), ]
	n2_clean <- merge(n2_clean, key_d, by = c("farmer", "season", "treatment", "crop"), all.x = TRUE)
	n2_clean <- n2_clean[!is.na(n2_clean$trial_id), ]

	# Multi-depth soil profile per trial (0-120 cm in 20 cm increments)
	trials_f <- unique(data.frame(trial_id = d$trial_id, farmer = m$farmer))
	soil_long <- merge(trials_f, r_soil, by = "farmer")

	long_sw <- data.frame(
		trial_id = sw_clean$trial_id,
		plot_id = sw_clean$plot_id,
		measured_crop_ = sw_clean$crop,
		date = sw_clean$date,
		LAI = sw_clean$LAI,
		soil_water_ = sw_clean$soil_water_
	)
	long_n2 <- data.frame(
		trial_id = n2_clean$trial_id,
		plot_id = n2_clean$plot_id,
		measured_crop_ = n2_clean$crop,
		date = n2_clean$date,
		ndfa_ = n2_clean$ndfa_,
		N_fixation = n2_clean$N_fixation,
		N_uptake_ = n2_clean$N_uptake_
	)
	long_soil <- data.frame(
		trial_id = soil_long$trial_id,
		depth_top = soil_long$depth_top,
		depth_bottom = soil_long$depth_bottom,
		soil_clay = as.numeric(soil_long[["clay (%)"]]),
		soil_silt = as.numeric(soil_long[["silt (%)"]]),
		soil_sand = as.numeric(soil_long[["sand (%)"]]),
		soil_pH = as.numeric(soil_long$pH_water),
		# g/kg * 1000 -> mg/kg
		soil_N_total = as.numeric(soil_long[["Ntotal_g_kg of soil"]]) * 1000,
		soil_P = as.numeric(soil_long[["Available_P_mg_Kg of soil"]]),
		soil_K = as.numeric(soil_long[["Available _K_mg_kg_soil"]]),
		soil_Ca_total_ = as.numeric(soil_long[["Total Ca__mg_kg of soil"]]),
		soil_Mg_total = as.numeric(soil_long[["Total Mg_mg_kg of soil"]]),
		# g C/kg / 10 -> %
		soil_SOC = as.numeric(soil_long[["organic_Carbon_g_kg_soil"]]) / 10
	)
	dlong <- carobiner::bindr(long_sw, long_n2, long_soil)

	# Daily weather for all 9 stations
	wth <- data.frame(
		station_id = trimws(as.character(r_clim$id_wst)),
		date = as_date(r_clim$Date),
		# Header says "J/m2" but values are 3.3-29.0 (MJ/m2/day); * 1000 -> kJ/m2
		srad = as.numeric(r_clim[[grep("^solar_radiation", names(r_clim))]]) * 1000,
		tmax = as.numeric(r_clim[[grep("^max_temperature", names(r_clim))]]),
		tmin = as.numeric(r_clim[[grep("^min_temperture", names(r_clim))]]),
		prec = as.numeric(r_clim[[grep("^rainfall", names(r_clim))]]),
		wspd = as.numeric(r_clim[[grep("^wind_speed", names(r_clim))]]),
		rhum = as.numeric(r_clim[[grep("^relative_humidity", names(r_clim))]])
	)
	wth <- wth[!is.na(wth$date), ]
	wth <- merge(wth, r_sites[, c("id_soil", "country", "longitude", "latitude", "elevation")],
		by.x = "station_id", by.y = "id_soil", all.x = TRUE)

	carobiner::write_files(path, meta, d, long = dlong, wth = wth)
}
