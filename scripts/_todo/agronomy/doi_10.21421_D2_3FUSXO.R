# R script for "carob"
# license: GPL (>=3)

## NOTES
# Groundnut, polythene mulch crossed with four varieties, two dry seasons (2014 and 2015), Sudan savanna of Nigeria. The description says a split-plot design with four replications.
# The file codes mulch as M and WM. M is polythene mulch and WM is without mulch, from the description.
# The file name says the trial was irrigated. The description says the dry season. irrigated is TRUE from the file name.
# HSW is a hundred-seed weight (g). seed_weight is the thousand-seed weight, so HSW is multiplied by 10.
# DF is days to flowering. DFF is earlier (days to first flower) and is not used.
# Germination, leaf area index, and the SPAD reading (SCMR) were taken on several days. They are in the long table with DAP. Pod yield is kg/ha (about 1400 to 4300).
# Country is Nigeria from the description. The file has no coordinates.

## ISSUES
# carob_contributor is unset until someone has read this script.
# write_files: missing on_farm, yield_moisture, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variable spad_ (SPAD?).

## NOT PROCESSED
# Haulm.Yield: the unit is not stated, and fresh and dry weight are not distinguished.
# Shelling.ratio: values run from about 9 to 51, which is neither a 0-1 ratio nor a usual groundnut shelling percentage.
# DMW columns: dry matter weight at 30, 45, 60, 75, and 90 days. The unit is not stated.
# DFF: days to first flower. flowering_days is DF.
# Germination, LAI, and SCMR at a single date were not copied onto the wide table. The long table keeps every date.

## NEW VARIABLES
# spad_: SPAD chlorophyll meter reading (file column SCMR). Unitless. On the long table, with DAP.

carob_script <- function(path) {

"The objective of this work conceived an idea of using Polythene mulch on broad bed furrows to improve groundnut production and productivity during the low temperature months in Sudan savanna of Nigeria. The experiments were conducted in 2 seasons (2014 and 2015 dry seasons). The treatments used in the experiment consisted of Polythene mulch (Mulch and Without mulch) and Groundnut varieties ( Samnut 23, Samnut 24, Samnut 26 and Ex-Dakar) laid out in Split plot design with 4 replications"

	uri <- "doi:10.21421/D2/3FUSXO"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety;mulch_type",
		response_vars = "yield",
		carob_effort = 0.45,
		carob_completion = 60,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		design = "split-plot",
		notes = "No coordinates in the file. Haulm yield and shelling ratio were not used."
	)

	f <- ff[basename(ff) == "Data file of Irrigated groundnut to polythene mulching on broad-bed and furrows .xlsx"]
	r <- carobiner::read.excel(f, fix_names=TRUE)

	mulch <- r$Mulch
	mulch[mulch == "M"] <- "polythene"
	mulch[mulch == "WM"] <- "none"

	d <- data.frame(
		trial_id = as.character(r$Year),
		plot_id = as.character(r$Plot.no),
		country = "Nigeria",
		crop = "groundnut",
		variety = r$Variety,
		mulch_type = mulch,
		rep = as.integer(r$Replication.number),
		yield = r$Pod.Yield,
		yield_part = "pod",
		flowering_days = r$DF,
		maturity_days = r$DM,
		seed_weight = as.numeric(r$HSW) * 10,
		planting_date = as.character(r$Year),
		season = "dry",
		irrigated = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$treatment <- paste(d$mulch_type, d$variety)
	d$record_id <- as.integer(seq_len(nrow(d)))

	add_time <- function(dap, germ, lai, spad) {
		data.frame(
			record_id = d$record_id,
			DAP = as.integer(dap),
			germination_rate = as.numeric(germ),
			LAI = as.numeric(lai),
			spad_ = as.numeric(spad)
		)
	}
	n <- nrow(d)
	na <- rep(NA_real_, n)
	long <- rbind(
		add_time(8, r$Germination_8.DAS, na, na),
		add_time(15, r$Germination_15.DAS, na, na),
		add_time(30, r$Germination_30.DAS, na, na),
		add_time(40, na, r$LAI_40.DAS, r$SCMR_40.DAS),
		add_time(60, na, r$LAI_60.DAS, r$SCMR_60.DAS),
		add_time(80, na, r$LAI_80.DAS, r$SCMR_80.DAS)
	)

	carobiner::write_files(path, meta, d, long)
}
