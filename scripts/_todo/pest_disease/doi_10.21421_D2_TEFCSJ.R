# R script for "carob"
# license: GPL (>=3)

## NOTES
# Elite groundnut lines at Minjibir, 2016. Two sheets of the same 60 plots, joined on plot number.
# DPWkgha is dry pod yield (kg/ha). DFWkgha is dry fodder (kg/ha). yield_moisture is 0. yield_part is pod.
# SW_ M_g100sd is grams per 100 seeds, multiplied by 10 to seed_weight (grams per 1000 seeds).
# DEM is emergence_days (description says "date"; values are 7-9). DF50 is not defined in the description; values are 25-32 and are stored as flowering_days.
# ELS, LLS and Rust on the second sheet are 1-9 scores in the long table.
# SPAD150Days is paired with CT150DAY, and SPAD250days with CT250days. NDVI1 and NDVI2 are on the first sheet and are not paired with those days.
# Coordinates are the midpoint of the Dataverse bounding box. northLongitude and southLongitude in that box are the latitudes (about 12.19).

## ISSUES
# No fertilizer rates or plot area.
# DPWkgha80 and DFWkgha80 are described as "at 80 days" but they sit with the adjusted yield columns, so they were not used as a second harvest.
# Sheet 2 SW is 70-150 g and is not the 100-seed weight. The sample is not defined.
# Drought scores are 1-3. The scale is not defined. The 150-day and 250-day labels were not treated as days after planting.
# The abstract says two checks and does not name them.
# Dataverse puts "Minjibir" in the state field. It is the location, not adm1.
# The description says 100-seed weight is in centimeters. The values are 27-48, so they were read as grams, as in the Buk sister file.
# write_files: missing carob_contributor, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown plant_population_, plants_harvested_, sound_mature_kernel_, reading_, canopy_temperature_; invalid disease early leaf spot, late leaf spot.

## NOT PROCESSED
# S.no, Entry number.
# DPWg (grams) and DFWkg (kg on the plot). The kg/ha columns were used.
# DPWkgha80, Pod adjusted, DFWkgha80, Fodder adjusted.
# HPWg: not defined.
# NMP, NSP: named in the description with no definition.
# Sheet 2 SW.

## NEW VARIABLES
# plant_population_: NPLE, plants per plot.
# plants_harvested_: NPH, plants per plot.
# sound_mature_kernel_: SMK, unit not stated.
# canopy_temperature_: CT columns, degrees C.
# reading_: source label of the long row (150days, 250days, NDVI1, NDVI2).

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
		design = "3 replications in blocks",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_residue;disease_severity;drought_stress",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Data file of Elite groundnut trial in Minjibir 2016.xlsx"]
	a <- carobiner::read.excel(f, sheet="Elite groundnut trial-2016", na="NA")
	b <- carobiner::read.excel(f, sheet="Groundnut trial-2016", na="NA")

	d <- data.frame(
		variety = trimws(a$Genotypes),
		treatment = trimws(a$Genotypes),
		rep = as.integer(a[["Replication number"]]),
		block_id = as.character(a$Block),
		plot_id = as.character(a[["Plot No"]]),
		emergence_days = as.numeric(a$DEM),
		flowering_days = as.numeric(a$DF50),
		plant_population_ = as.numeric(a$NPLE),
		plants_harvested_ = as.numeric(a$NPH),
		yield = as.numeric(a$DPWkgha),
		dmy_residue = as.numeric(a$DFWkgha),
		seed_weight = as.numeric(a[["SW_ M_g100sd"]]) * 10,
		shelling_percentage = as.numeric(a$SH_Calc_pct),
		sound_mature_kernel_ = as.numeric(a$SMK)
	)
	b$plot_id <- as.character(b[["Plot No"]])
	d <- merge(d, b[, c("plot_id", "DROUGHT")], by="plot_id", all.x=TRUE, sort=FALSE)
	d$drought_stress <- as.character(d$DROUGHT)
	d$DROUGHT <- NULL

	d$crop <- "groundnut"
	d$yield_part <- "pod"
	d$yield_moisture <- 0
	d$yield_isfresh <- FALSE
	d$stress <- "disease;drought"
	d$country <- "Nigeria"
	d$location <- "Minjibir"
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
	ia <- match(d$plot_id, as.character(a[["Plot No"]]))
	ib <- match(d$plot_id, as.character(b[["Plot No"]]))
	dis <- data.frame(
		record_id = rep(d$record_id, 3),
		disease = rep(c("early leaf spot", "late leaf spot", "rust"), each=n),
		disease_severity = as.character(c(b$ELS[ib], b$LLS[ib], b$Rust[ib])),
		reading_ = NA_character_,
		canopy_temperature_ = NA_real_,
		SPAD = NA_real_,
		NDVI = NA_real_
	)
	ph <- data.frame(
		record_id = rep(d$record_id, 4),
		disease = NA_character_,
		disease_severity = NA_character_,
		reading_ = rep(c("150days", "250days", "NDVI1", "NDVI2"), each=n),
		canopy_temperature_ = c(b$CT150DAY[ib], b$CT250days[ib], rep(NA_real_, n), rep(NA_real_, n)),
		SPAD = c(b$SPAD150Days[ib], b$SPAD250days[ib], rep(NA_real_, n), rep(NA_real_, n)),
		NDVI = c(rep(NA_real_, n), rep(NA_real_, n), a$NDVI1[ia], a$NDVI2[ia])
	)
	long <- rbind(dis, ph)
	long$order <- as.integer(ave(seq_len(nrow(long)), long$record_id, FUN=seq_along))

	carobiner::write_files(path, meta, d, long=long)
}
