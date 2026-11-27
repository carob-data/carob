# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Plot area (6 m2) is not stated in the description docx, but is exact from DPWg / DPWkgha and DFWg (kg) / DFWkgha on every row; used to derive plant_density and harvested_plant_density_.
# DPWkgha80 and DFWkgha80 are described as yields "at 80 days" in the docx, but equal DPWkgha * 64 / NPH and DFWkgha * 64 / NPH on every row (stand-adjusted to 64 plants/plot); excluded in favor of unadjusted DPWkgha and DFWkgha.
# Sheet 2 column "SW_ M_g100sd" (46-186 g) differs from Sheet 1 100-seed weight (30-50 g) and its sample basis is undefined.
# Drought visual score (3-5) has no scale definition. CT150DAYS/SPAD150days and CT250days/SPAD250Days time labels exceed a normal groundnut cycle, so they are stored in timing rather than DAP.

## NEW
# harvested_plant_density_: harvested plant stand per hectare (plants/ha; from NPH / 6 m2)
# sound_mature_kernel_: sound mature kernel score/count (SMK; docx says "Number", values 75-97)
# disease: "late leaf spot" not yet in values_disease.csv

## MISSING
# irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date

## NOTES
# Multi-location elite groundnut trial at Bayero University Kano (BUK), Kano State, Nigeria, 2016 (Dataverse collection window 2016-07-14 to 2016-12-25).
# Two sheets of the same 60 plots (20 entries = 18 ICRISAT elite lines + 2 checks SAMNUT 24 and SAMNUT 25, 3 reps, 4 blocks) joined by Plot No.
# DPWkgha is dry pod yield (yield_part = "pod", yield_moisture = 0, yield_isfresh = FALSE); DFWkgha is dry fodder (dmy_residue). DEM (6-12) and DFF (24-29) are days, not calendar dates.
# Foliar diseases (early leaf spot, late leaf spot, rust; 1-9 scale) and repeated physiological readings (canopy_temperature, SPAD, NDVI) are stored in the long table.
# Coordinates are the midpoint of the Dataverse bounding box (northLongitude/southLongitude fields hold latitudes ~11.984 N).

## NOT PROCESSED
# Groundnut trial-2016: DPWkgha80, podadjusted, DFWkgha80, fodderadjusted (stand-adjusted yields; see ISSUES); HPWg (66-159 g), NSP (12-25), NMP (27-300) (not defined in description docx).
# Elite trial in Buk-2016: SW_ M_g100sd (46-186 g; sample undefined, differs from Sheet 1 100-seed weight).

carob_script <- function(path) {

"
Evaluation of elite lines for productivity diseases and drought resistance in the location Buk

Multilocation evaluation of elite breeding lines conducted at three locations viz; BUK, with 20 entries (18 elite lines from ICRISAT and 2 check varieties) was aimed to identify genotypes with high yield potential, dorught resistant and resistant to foliar diseases.        Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/NC420W"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = "3 replications in blocks",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_residue;emergence_days;flowering_days;seed_weight;shelling_percentage;disease_severity;drought_stress;canopy_temperature;SPAD;NDVI",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 80,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Data file of Elite groundnut trial in Buk 2016.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Groundnut trial-2016", na = "NA")
	r2 <- carobiner::read.excel(f, sheet = "Elite trial in Buk-2016", na = "NA")

	variety <- trimws(r1$Genotype)
	vtype <- ifelse(variety %in% c("SAMNUT 24", "SAMNUT 25"), "check", "breeding line")

	d <- data.frame(
		variety = variety,
		variety_type = vtype,
		treatment = variety,
		rep = as.integer(r1[["Replication number"]]),
		block_id = as.character(r1$Block),
		plot_id = as.character(r1[["Plot No"]]),
		emergence_days = as.numeric(r1$DEM),
		flowering_days = as.numeric(r1$DFF),
		# Plot area = 6 m2 (exact from DPWg / 1000 / DPWkgha * 10000 on all 60 rows)
		plant_density = as.numeric(r1$NPLE) * 10000 / 6,
		harvested_plant_density_ = as.numeric(r1$NPH) * 10000 / 6,
		plot_area = 6,
		yield = as.numeric(r1$DPWkgha),
		dmy_residue = as.numeric(r1$DFWkgha),
		seed_weight = as.numeric(r1[["SW_ M_g100sd"]]) * 10,
		shelling_percentage = as.numeric(r1$SH_Calc_pct),
		sound_mature_kernel_ = as.numeric(r1$SMK)
	)
	ib <- match(d$plot_id, as.character(r2[["Plot No"]]))
	d$drought_stress <- as.character(r2$DROUGHT[ib])

	d$crop <- "groundnut"
	d$yield_part <- "pod"
	d$yield_moisture <- 0
	d$yield_isfresh <- FALSE
	d$stress <- "disease;drought"
	d$country <- "Nigeria"
	d$adm1 <- "Kano"
	d$location <- "Bayero University Kano"
	d$planting_date <- "2016"
	d$trial_id <- "Buk_2016"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	d$longitude <- (8.474923 + 8.475408) / 2
	d$latitude <- (11.983994 + 11.983964) / 2
	d$geo_uncertainty <- 27
	d$geo_source <- "ICRISAT Dataverse bounding box"
	d$record_id <- as.integer(seq_len(nrow(d)))

	n <- nrow(d)
	dis <- data.frame(
		record_id = rep(d$record_id, 3),
		disease = rep(c("early leaf spot", "late leaf spot", "rust"), each = n),
		disease_severity = as.character(c(r1$ELS_E_1to9, r1$LLS_E_1to9, r2$Rust_E_1to9[ib])),
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
		timing = rep(c("150days", "250days", "NDVI1", "NDVI2"), each = n),
		canopy_temperature = c(r2$CT150DAYS[ib], r2$CT250days[ib], rep(NA_real_, n), rep(NA_real_, n)),
		SPAD = c(r2$SPAD150days[ib], r2$SPAD250Days[ib], rep(NA_real_, n), rep(NA_real_, n)),
		NDVI = c(rep(NA_real_, n), rep(NA_real_, n), r2$NDVI1[ib], r2$NDVI2[ib])
	)
	long <- rbind(dis, ph)
	long$order <- as.integer(ave(seq_len(nrow(long)), long$record_id, FUN = seq_along))

	carobiner::write_files(path, meta, d, long = long)
}
