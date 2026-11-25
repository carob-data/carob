# R script for "carob"
# license: GPL (>=3)

## NOTES
# UNICA potato under drip irrigation at National Agrarian University La Molina (UNALM), Lima, Peru (June-September 2018). Five blocks; T1 = full irrigation, T2 = CWSI < 0.4, T3 = CWSI < 0.7.
# Dictionary.xls states units explicitly: FL, FS, FR, FT, DL, DS, DR, DT are in g per plant (mapped to terminag fw_leaves..dw_tubers); CC is % ground cover (0.1 to 100%, mapped to ground_cover); Hv is gravimetric soil humidity in % (divided by 100 to g/g for soil_GWC); V is irrigation volume in m3 per plot; Tf, Tw, Ta, dT are degrees C; CWSI is dimensionless.
# Coordinates (-12.076289, -76.948417) come from the CIP Dataverse geographic metadata.
# Biomass, canopy cover, soil humidity, irrigation volumes, and CWSI thermography are all repeated by DAP and stored in the long table linked via record_id.

## ISSUES
# Plot area and plant density are not stated in Data.xls or Dictionary.xls, so per-plant tuber fresh weight (g/plant) and per-plot irrigation volume (m3) cannot be converted to kg/ha and mm.
# No fertilizer rates or exact calendar planting/harvest dates (only June-September 2018).
# write_files: missing carob_contributor, yield, yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer; unknown variables: cwsi_rule_, harvest_, tubers_per_plant_, irrigation_event_, irrigation_volume_m3_, cwsi_, foliage_temperature_, foliage_air_dt_.

## NOT PROCESSED
# CWSI sheet meteorological columns (Tw, Ta, RH, es, VPD, PAR, SR, ws, wd): repeated across all plots at measurement hours without calendar dates for a daily weather table.
# Irrigation daTI (days after treatment initiation): DAP is used as the common time variable.
# Dictionary.xls: variable definitions and units.

## NEW VARIABLES
# cwsi_rule_: irrigation threshold rule (full, 0.4, or 0.7 CWSI).
# harvest_: sequential harvest number (1 to 18).
# tubers_per_plant_: number of tubers per plant.
# irrigation_event_: sequential irrigation event number per plot.
# irrigation_volume_m3_: irrigation volume applied per plot (m3).
# cwsi_: crop water stress index from thermography (dimensionless).
# foliage_temperature_: canopy foliage temperature (degrees C).
# foliage_air_dt_: foliage minus air temperature difference (degrees C).


carob_script <- function(path) {

"
Dataset for: Analyze the use of thresholds based on crop water stress index calculus in potato  'Unica' under drip irrigation

The objective of this study was to evaluate the UNICA potato crop variety's yield relate with the threshold of Crop Water Stress Index subjected to different water stress levels and under drip of irrigation. The trial was carried out in the experimental field of National Agrarian University La Molina during the months of June-2018 to September-2018. Three treatments of irrigation moments based on the Crop Water Stress Index (CWSI) were applied for this trial: Full irrigated (T1), CWSI less than 0.4 (T2), CWSI less than 0.7 (T3).
"

	uri <- "doi:10.21223/P3_2VCYRQ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "cwsi_rule_",
		response_vars = "fw_tubers;dw_tubers;ground_cover;soil_GWC",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 80,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "Data.xls"]
	nas <- c("", "NA", "NULL")
	r1 <- carobiner::read.excel(f, sheet="Biomass", na=nas)
	r2 <- carobiner::read.excel(f, sheet="Canopy cover", na=nas)
	r3 <- carobiner::read.excel(f, sheet="Soil humidity", na=nas)
	r4 <- carobiner::read.excel(f, sheet="Irrigation", na=nas)
	r5 <- carobiner::read.excel(f, sheet="CWSI", na=nas)

	lab <- c(T1="T1 full irrigation", T2="T2 CWSI < 0.4", T3="T3 CWSI < 0.7")
	rule <- c(T1="full", T2="0.4", T3="0.7")

	plots <- unique(data.frame(
		plot = as.integer(r1$plot),
		block = as.integer(r1$block),
		code = trimws(as.character(r1$treatment))
	))
	plots <- plots[order(plots$plot), ]

	irr_n <- tapply(as.numeric(r4$V), as.integer(r4$plot), function(z) sum(!is.na(z) & z > 0))

	d <- data.frame(
		record_id = seq_len(nrow(plots)),
		trial_id = "LaMolina_2018",
		plot_id = as.character(plots$plot),
		block_id = as.character(plots$block),
		rep = plots$block,
		treatment = unname(lab[plots$code]),
		cwsi_rule_ = unname(rule[plots$code]),
		crop = "potato",
		variety = "Unica",
		yield_part = "tubers",
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = TRUE,
		planting_date = "2018-06",
		harvest_date = "2018-09",
		irrigated = TRUE,
		irrigation_method = "drip",
		irrigation_number = as.integer(unname(irr_n[as.character(plots$plot)])),
		on_farm = FALSE,
		is_survey = FALSE
	)

	rid <- function(p) d$record_id[match(as.character(p), d$plot_id)]

	hh <- r5$hour
	if (inherits(hh, "POSIXt")) {
		hh <- format(hh, "%H:%M:%S", tz="UTC")
	} else {
		hh <- trimws(as.character(hh))
	}

	b <- data.frame(
		record_id = rid(r1$plot),
		DAP = as.integer(r1$dap),
		harvest_ = as.integer(r1$harvest),
		fw_leaves = as.numeric(r1$FL),
		fw_stems = as.numeric(r1$FS),
		fw_roots = as.numeric(r1$FR),
		fw_tubers = as.numeric(r1$FT),
		tubers_per_plant_ = as.numeric(r1$nT),
		dw_leaves = as.numeric(r1$DL),
		dw_stems = as.numeric(r1$DS),
		dw_roots = as.numeric(r1$DR),
		dw_tubers = as.numeric(r1$DT)
	)
	cc <- data.frame(
		record_id = rid(r2$plot),
		DAP = as.integer(r2$dap),
		ground_cover = as.numeric(r2$CC)
	)
	sh <- data.frame(
		record_id = rid(r3$plot),
		DAP = as.integer(r3$dap),
		# Hv is gravimetric moisture in %; terminag soil_GWC is g/g
		soil_GWC = as.numeric(r3$Hv) / 100
	)
	ir <- data.frame(
		record_id = rid(r4$plot),
		DAP = as.integer(r4$dap),
		irrigation_event_ = as.integer(r4$nIrri),
		irrigation_volume_m3_ = as.numeric(r4$V)
	)
	cw <- data.frame(
		record_id = rid(r5$plot),
		DAP = as.integer(r5$dap),
		time = hh,
		cwsi_ = as.numeric(r5$CWSI),
		foliage_temperature_ = as.numeric(r5$Tf),
		foliage_air_dt_ = as.numeric(r5$dT)
	)
	dlong <- carobiner::bindr(b, cc, sh, ir, cw)

	carobiner::write_files(path, meta, d, long=dlong)
}
