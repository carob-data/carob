# R script for "carob"
# license: GPL (>=3)

## NOTES
# Potato CWSI drip-irrigation experiment at CIP La Molina, Lima, Peru (12 plots of 3.6 x 5 m = 18 m2; 60 plants/plot sown at 0.3 m plant spacing and 1.0 m furrow spacing -> plant_density = 33333.33 plants/ha).
# Dictionary treatments: T1 = full irrigation, T2 = gs_max 0.15 mol H2O m-2 s-1, T3 = gs_max 0.05 mol H2O m-2 s-1 (note: abstract labels the thresholds T1/T2 vs control; Data_dictionary.xls definitions matching the raw `trat` codes T1/T2/T3 are used).
# Plot-level yield (kg/ha fresh tubers) and tuber_density (/ha) are computed from plot mean per-plant PF (g/plant) and nT (tubers/plant) in Harvest.xlsx multiplied by plant_density (33333.33 plants/ha). Plot-level yield_moisture (%) and yield_dry (kg/ha) are computed from mean tuber dry-matter concentration (DBC = DW/FW * 100) in Dry_biomass.xlsx.
# Measured drip irrigation (Lr, mm) from Irrigation.xlsx (starting 39 DAP) is summed per plot into irrigation_amount (mm). Note from abstract: 7 uniform establishment furrow irrigations of ~67 mm each (~469 mm total) were applied at 0, 4, 7, 11, 17, 22, and 30 DAP prior to drip setup and are not in Irrigation.xlsx.
# All 6 observation workbooks (Harvest, Dry_biomass, CWSI, Irrigation, Eco-physiological, Tuber_isotopes) are preserved in full in the long table linked via record_id.

## ISSUES
# No calendar planting or harvest dates and no fertilizer rates in the dataset.
# irrigation_amount reflects measured drip irrigation from Irrigation.xlsx (39 DAP onward) and excludes the ~469 mm approximate pre-drip furrow irrigations described in the abstract.
# write_files: missing carob_contributor, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date; unknown variables: irrigation_regime_, tubers_per_plant_, tuber_dm_pct_, cwsi_, foliage_temperature_, wet_surface_temp_, air_temp_, foliage_air_dt_, irrigation_event_mm_, gs_max_, leaf_water_potential_, leaf_d13C_, leaf_D13C_, tuber_d13C_, tuber_D13C_.

## NOT PROCESSED
# Data_dictionary.xls: variable definitions and units.
# ldw and tdw (mg): analytical sample weights for IRMS isotope analysis.

## NEW VARIABLES
# irrigation_regime_: water-restriction treatment (full irrigation, gs_max 0.15, gs_max 0.05).
# tubers_per_plant_: harvested tubers per plant (count).
# tuber_dm_pct_: tuber dry biomass concentration (%).
# cwsi_: crop water stress index from thermography.
# foliage_temperature_, wet_surface_temp_, air_temp_, foliage_air_dt_: thermography temperatures (degrees C).
# irrigation_event_mm_: per-event drip irrigation depth (mm).
# gs_max_: maximum stomatal conductance (mol H2O m-2 s-1).
# leaf_water_potential_: leaf water potential (MPa).
# leaf_d13C_, leaf_D13C_, tuber_d13C_, tuber_D13C_: carbon-13 isotopic composition and discrimination (per mil).


carob_script <- function(path) {

"
Replication Data for: Analyze Crop Water Stress Index (CWSI) values for an optimum tuber yield in potatoes under humid conditions.

It developed a field experiment, with the aim to analyze Crop Water Stress Index (CWSI) under humid conditions with testing two irrigation timing treatments with pre-established gs_max threshold (0.15 [T1] and 0.050 [T2] mol H2O m−2 s−1) against a control (frequently irrigated). The experiment was conducted in a 416 m2 total area, divided in 12 plots (3.6×5 m2 each) and sixty plants per plot were sown in five rows with a plant and furrow distance of 0.3 and 1 m, respectively. Seven furrow irrigations were provided at 0, 4, 7, 11, 17, 22 and 30 days after planting watering every plot with approximately 67 mm per irrigation; after that, a drip irrigation system was set up independently for each plot.
"

	uri <- "doi:10.21223/P3_PNJBH6"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "irrigation_regime_",
		response_vars = "yield;yield_dry;tuber_density;soil_GWC;soil_VWC",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.25
	)

	nas <- c("", "NA")
	read_clean <- function(fname) {
		x <- carobiner::read.excel(ff[basename(ff) == fname], na=nas)
		names(x) <- trimws(names(x))
		x
	}
	r1 <- read_clean("Harvest.xlsx")
	r2 <- read_clean("CWSI.xlsx")
	r3 <- read_clean("Dry_biomass.xlsx")
	r4 <- read_clean("Irrigation.xlsx")
	r5 <- read_clean("Eco-physiological.xlsx")
	r6 <- read_clean("Tuber_isotopes.xlsx")

	lab <- c(T1="full irrigation", T2="gs_max 0.15", T3="gs_max 0.05")
	trat <- trimws(as.character(r1$trat))
	regime <- unname(lab[trat])
	regime[is.na(regime)] <- trat[is.na(regime)]
	plots <- unique(data.frame(plot = as.integer(r1$plot), regime = regime))
	plots <- plots[order(plots$plot), ]

	# 0.3 m plant spacing x 1.0 m row spacing -> 33333.33 plants/ha (60 plants per 18 m2 plot)
	pdens <- 10000 / (0.3 * 1.0)
	pf_mean <- tapply(as.numeric(r1$PF), as.integer(r1$plot), mean, na.rm=TRUE)
	nt_mean <- tapply(as.numeric(r1$nT), as.integer(r1$plot), mean, na.rm=TRUE)
	dbc_mean <- tapply(as.numeric(r3$DBC), as.integer(r3$plot), mean, na.rm=TRUE)
	lr_sum <- tapply(as.numeric(r4$Lr), as.integer(r4$plot), sum, na.rm=TRUE)
	lr_cnt <- tapply(as.numeric(r4$Lr), as.integer(r4$plot), function(z) sum(!is.na(z) & z > 0))

	pchar <- as.character(plots$plot)
	y_fresh <- unname(pf_mean[pchar]) * pdens / 1000
	dm_pct <- unname(dbc_mean[pchar])

	d <- data.frame(
		record_id = seq_len(nrow(plots)),
		trial_id = "LaMolina_CWSI",
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		# Dataverse geographic coverage
		latitude = -12.08,
		longitude = -76.95,
		geo_from_source = TRUE,
		crop = "potato",
		plot_id = pchar,
		treatment = plots$regime,
		irrigation_regime_ = plots$regime,
		plot_area = 3.6 * 5,
		plant_spacing = 30,
		row_spacing = 100,
		plant_density = pdens,
		tuber_density = unname(nt_mean[pchar]) * pdens,
		yield = y_fresh,
		yield_dry = y_fresh * dm_pct / 100,
		yield_moisture = 100 - dm_pct,
		yield_isfresh = TRUE,
		yield_part = "tubers",
		irrigated = TRUE,
		irrigation_method = "drip",
		irrigation_amount = unname(lr_sum[pchar]),
		irrigation_number = as.integer(unname(lr_cnt[pchar])),
		on_farm = FALSE,
		is_survey = FALSE
	)

	rid <- function(p) d$record_id[match(as.character(p), d$plot_id)]

	hh <- r2$hour
	if (inherits(hh, "POSIXt")) {
		hh <- format(hh, "%H:%M:%S", tz="UTC")
	} else {
		hh <- trimws(as.character(hh))
	}

	l_harv <- data.frame(
		record_id = rid(r1$plot),
		order = as.integer(r1$plant),
		fw_tubers = as.numeric(r1$PF),
		tubers_per_plant_ = as.numeric(r1$nT)
	)
	l_dry <- data.frame(
		record_id = rid(r3$plot),
		order = as.integer(r3$plant),
		subsample_fw_tubers = as.numeric(r3$FW),
		subsample_dw_tubers = as.numeric(r3$DW),
		tuber_dm_pct_ = as.numeric(r3$DBC)
	)
	l_cwsi <- data.frame(
		record_id = rid(r2$pot),
		DAP = as.integer(r2$dap),
		time = hh,
		cwsi_ = as.numeric(r2$CWSI),
		foliage_temperature_ = as.numeric(r2$Tf),
		wet_surface_temp_ = as.numeric(r2$Tw),
		air_temp_ = as.numeric(r2$Ta),
		foliage_air_dt_ = as.numeric(r2$dT)
	)
	l_irr <- data.frame(
		record_id = rid(r4$plot),
		DAP = as.integer(r4$dds),
		# Hg (%) -> g/g (soil_GWC); Hv (%) -> cm3/cm3 (soil_VWC)
		soil_GWC = as.numeric(r4$Hg) / 100,
		soil_VWC = as.numeric(r4$Hv) / 100,
		irrigation_event_mm_ = as.numeric(r4$Lr)
	)
	l_eco <- data.frame(
		record_id = rid(r5$plot),
		DAP = as.integer(r5$dap),
		gs_max_ = as.numeric(r5$gs_max),
		leaf_water_potential_ = as.numeric(r5$LWP),
		leaf_d13C_ = as.numeric(r5$dleaf),
		leaf_D13C_ = as.numeric(r5$Dleaf)
	)
	l_iso <- data.frame(
		record_id = rid(r6$plot),
		order = as.integer(r6$plant),
		tuber_d13C_ = as.numeric(r6$dtuber),
		tuber_D13C_ = as.numeric(r6$Dtuber)
	)

	dlong <- carobiner::bindr(l_harv, l_dry, l_cwsi, l_irr, l_eco, l_iso)

	carobiner::write_files(path, meta, d, long=dlong)
}
