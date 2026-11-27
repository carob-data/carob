# R script for "carob"
# license: GPL (>=3)

## ISSUES
# No fertilizer rates, irrigation status, or exact planting/harvest dates are reported.
# SPAD150Days/SPAD250days and CT150DAY/CT250days have 150-day and 250-day labels that exceed a groundnut season; kept as timing labels rather than DAP.
# Drought score (1-3) scale is not defined in the description.
# Description states 100-seed weight is in cm; values (27-48) are clearly grams and were multiplied by 10 for 1000-seed weight.

## NEW
# plant_density_emerged_: emerged plant density (plants/ha), computed from NPLE / 6 m2 plot area.
# sound_mature_kernel_: sound mature kernels (SMK, %).
# disease: late leaf spot

## MISSING
# irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date

## NOTES
# Multi-location elite groundnut trial at Minjibir (Kano State, Nigeria), 2016: 18 ICRISAT breeding lines + 2 check varieties (SAMNUT 24, SAMNUT 25) in 3 replications x 4 blocks.
# Both sheets cover the same 60 plots and are matched by Plot No. Harvested plot area is exactly 6 m2 (10 * DPWg / DPWkgha == 6 and 10000 * DFWkg / DFWkgha == 6 on all rows).
# DPWkgha is dry pod yield (kg/ha, yield_moisture = 0, yield_isfresh = FALSE) and DFWkgha is dry fodder yield (dmy_residue).
# ELS, LLS, and Rust (1-9 visual scores), SPAD, canopy_temperature, and NDVI are stored in the long table.
# Coordinates are the midpoint of the ICRISAT Dataverse bounding box (northLongitude/southLongitude hold the latitudes).

## NOT PROCESSED
# Sheet 1, DPWkgha80, Pod adjusted, DFWkgha80, Fodder adjusted: stand-adjusted yields (DPWkgha80 == DPWkgha * 64 / NPH), not a separate 80-day harvest despite the description text.
# Sheet 1, HPWg: likely 100-pod weight (64-122 g), not defined in description.
# Sheet 1, NMP, NSP: pod/seed count traits, not defined in description (NMP missing for rep 3).
# Sheet 2, SW: shelled seed weight (g) from a 200 g pod sample (redundant with SH_Calc_pct == 100 * SW / 200).

carob_script <- function(path) {

"
Evaluation of elite lines for productivity diseases and drought resistance in the location Minjibir

Multilocation evaluation of elite breeding lines conducted at three locations viz; BUK, with 20 entries (18 elite lines from ICRISAT and 2 check varieties) was aimed to identify genotypes with high yield potential, dorught resistant and resistant to foliar diseases.        Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/TEFCSJ"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_residue;disease_severity;drought_stress;emergence_days;flowering_days;seed_weight;shelling_percentage;SPAD;NDVI;canopy_temperature",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 80,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Data file of Elite groundnut trial in Minjibir 2016.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Elite groundnut trial-2016", na="NA")
	r2 <- carobiner::read.excel(f, sheet="Groundnut trial-2016", na="NA")

	plot_id <- as.character(r1[["Plot No"]])
	ib <- match(plot_id, as.character(r2[["Plot No"]]))
	variety <- trimws(r1$Genotypes)

	# Harvested plot area is 6 m2 (verified from 10 * DPWg / DPWkgha == 6 and 10000 * DFWkg / DFWkgha == 6)
	plot_area <- 6

	d <- data.frame(
		country = "Nigeria",
		adm1 = "Kano",
		adm2 = "Minjibir",
		location = "Minjibir",
		crop = "groundnut",
		variety = variety,
		# Abstract notes 18 ICRISAT elite lines and 2 check varieties (SAMNUT 24, SAMNUT 25)
		variety_type = ifelse(grepl("^SAMNUT", variety), "check", NA_character_),
		treatment = variety,
		rep = as.integer(r1[["Replication number"]]),
		block_id = as.character(r1$Block),
		plot_id = plot_id,
		plot_area = plot_area,
		emergence_days = as.numeric(r1$DEM),
		flowering_days = as.numeric(r1$DF50),
		plant_density_emerged_ = as.numeric(r1$NPLE) * 10000 / plot_area,
		plant_density = as.numeric(r1$NPH) * 10000 / plot_area,
		yield = as.numeric(r1$DPWkgha),
		dmy_residue = as.numeric(r1$DFWkgha),
		# 100-seed weight in g (description mistakenly says cm), converted to 1000-seed weight
		seed_weight = as.numeric(r1[["SW_ M_g100sd"]]) * 10,
		shelling_percentage = as.numeric(r1$SH_Calc_pct),
		sound_mature_kernel_ = as.numeric(r1$SMK),
		drought_stress = as.character(r2$DROUGHT[ib])
	)

	d$yield_part <- "pod"
	d$yield_moisture <- 0
	d$yield_isfresh <- FALSE
	d$stress <- "disease;drought"
	d$planting_date <- "2016"
	d$trial_id <- "Minjibir_2016"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	d$longitude <- (8.625937 + 8.630400) / 2
	d$latitude <- (12.192154 + 12.191986) / 2
	d$geo_uncertainty <- 243
	d$geo_source <- "ICRISAT Dataverse bounding box"
	d$record_id <- as.integer(seq_len(nrow(d)))

	n <- nrow(d)
	dis <- data.frame(
		record_id = rep(d$record_id, 3),
		disease = rep(c("early leaf spot", "late leaf spot", "rust"), each=n),
		disease_severity = as.character(c(r2$ELS[ib], r2$LLS[ib], r2$Rust[ib])),
		severity_scale = "1-9",
		timing = NA_character_,
		canopy_temperature = NA_real_,
		SPAD = NA_real_,
		NDVI = NA_real_
	)
	ph <- data.frame(
		record_id = rep(d$record_id, 4),
		disease = NA_character_,
		disease_severity = NA_character_,
		severity_scale = NA_character_,
		timing = rep(c("150days", "250days", "NDVI1", "NDVI2"), each=n),
		canopy_temperature = as.numeric(c(r2$CT150DAY[ib], r2$CT250days[ib], rep(NA_real_, n), rep(NA_real_, n))),
		SPAD = as.numeric(c(r2$SPAD150Days[ib], r2$SPAD250days[ib], rep(NA_real_, n), rep(NA_real_, n))),
		NDVI = as.numeric(c(rep(NA_real_, n), rep(NA_real_, n), r1$NDVI1, r1$NDVI2))
	)
	long <- rbind(dis, ph)
	long$order <- as.integer(ave(seq_len(nrow(long)), long$record_id, FUN=seq_along))

	carobiner::write_files(path, meta, d, long=long)
}
