# R script for "carob"
# license: GPL (>=3)

## NOTES
# Elite groundnut lines at Bayero University Kano, 2016. Two sheets of the same 60 plots (20 entries x 3 reps), joined on plot number.
# DPWkgha is dry pod yield (kg/ha), so yield_moisture is 0 and yield_part is pod. DFWkgha is dry fodder (kg/ha) and is dmy_residue.
# SW_ M_g100sd is grams per 100 seeds. seed_weight is grams per 1000 seeds, so the value is multiplied by 10.
# DEM and DFF are stored as emergence_days and flowering_days. The description says "date" but the values are 6-12 and 24-29.
# ELS_E_1to9, LLS_E_1to9 and Rust_E_1to9 are 1-9 scores in the long table. The description's "SPAD = single-photon avalanche diode" is not how the numbers behave; they are stored as SPAD.
# CT150DAYS and SPAD150days share one long row, and the 250-day pair another. NDVI1 and NDVI2 are separate rows because the file does not say they were taken on those days.
# Coordinates are the midpoint of the Dataverse bounding box. The box labels northLongitude and southLongitude are the latitudes (about 11.98).

## ISSUES
# No fertilizer rates or plot area. NPLE and NPH stay as per-plot counts.
# The description calls DPWkgha80 and DFWkgha80 yields "at 80 days". Those columns match the adjusted-yield columns, not a second harvest, so they were not used.
# Sheet 2 SW_ M_g100sd is not the 100-seed weight (sheet 1 is 30-50 g; sheet 2 is 46-186 g) and the sample is not defined.
# Drought is a visual score (3-5 here). The scale is not defined. 150 and 250 days are longer than a normal groundnut season, so that time origin was not treated as days after planting.
# The abstract says two checks and does not name them.
# State is not a separate metadata field. Minjibir's metadata puts the town in the state field; this file's city string is "BUk(Bayero University Kano)".
# write_files: missing carob_contributor, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown plant_population_, plants_harvested_, sound_mature_kernel_, reading_, canopy_temperature_; invalid disease early leaf spot, late leaf spot.

## NOT PROCESSED
# S.No, Entry number: layout indexes.
# DPWg and DFWg: plot weights in grams. The kg/ha columns were used. Plot area is not given.
# DPWkgha80, podadjusted, DFWkgha80, fodderadjusted: adjusted yields. See ISSUES.
# HPWg: not defined in the description.
# NSP, NMP: not defined.
# Sheet 2 SW_ M_g100sd: grams, sample not defined.

## NEW VARIABLES
# plant_population_: NPLE, plants per plot.
# plants_harvested_: NPH, plants per plot.
# sound_mature_kernel_: SMK. The description says "Number".
# canopy_temperature_: CT columns, degrees C.
# reading_: which repeated measurement the long row came from (150days, 250days, NDVI1, NDVI2).

carob_script <- function(path) {

"
Evaluation of elite lines for productivity diseases and drought resistance in the location Buk

Multilocation evaluation of elite breeding lines conducted at three locations viz; BUK, with 20 entries (18 elite lines from ICRISAT and 2 check varieties) was aimed to identify genotypes with high yield potential, dorught resistant and resistant to foliar diseases.        Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/NC420W"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "Data file of Elite groundnut trial in Buk 2016.xlsx"]
	a <- carobiner::read.excel(f, sheet="Groundnut trial-2016", na="NA")
	b <- carobiner::read.excel(f, sheet="Elite trial in Buk-2016", na="NA")

	d <- data.frame(
		variety = trimws(a$Genotype),
		treatment = trimws(a$Genotype),
		rep = as.integer(a[["Replication number"]]),
		block_id = as.character(a$Block),
		plot_id = as.character(a[["Plot No"]]),
		emergence_days = as.numeric(a$DEM),
		flowering_days = as.numeric(a$DFF),
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
	ia <- match(d$plot_id, as.character(a[["Plot No"]]))
	ib <- match(d$plot_id, as.character(b[["Plot No"]]))
	score <- as.character(c(a$ELS_E_1to9[ia], a$LLS_E_1to9[ia], b$Rust_E_1to9[ib]))
	dis <- data.frame(
		record_id = rep(d$record_id, 3),
		disease = rep(c("early leaf spot", "late leaf spot", "rust"), each=n),
		disease_severity = score,
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
		canopy_temperature_ = c(b$CT150DAYS[ib], b$CT250days[ib], rep(NA_real_, n), rep(NA_real_, n)),
		SPAD = c(b$SPAD150days[ib], b$SPAD250Days[ib], rep(NA_real_, n), rep(NA_real_, n)),
		NDVI = c(rep(NA_real_, n), rep(NA_real_, n), b$NDVI1[ib], b$NDVI2[ib])
	)
	long <- rbind(dis, ph)
	long$order <- as.integer(ave(seq_len(nrow(long)), long$record_id, FUN=seq_along))

	carobiner::write_files(path, meta, d, long=long)
}
