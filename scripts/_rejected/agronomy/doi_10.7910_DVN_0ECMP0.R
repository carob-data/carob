# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 0. Reject as no location data is available.
# 1. Only Summary sheet is used; other sheets hold other-country data or
#    redundant/derived duplicates (simulated columns, goodness-of-fit stats).
# 2. Summary rows 27-34 are 8 distinct real trial records (verified against
#    APSIM/DSSAT sheets).
# 3. Source doesn't say which row belongs to which season - planting_date NA.
# 4. No data beyond country, coordinate harcoded added for Kenya only.

carob_script <- function(path) {

"
APSIM and DSSAT Calibration

Calibration and validation of crop simulation models for 3 maize cultivars in Embu county Kenya
"

	uri <- "doi:10.7910/DVN/0ECMP0"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = "AgMIP",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_total;flowering_days;maturity_days",
		notes = NA,
		carob_contributor = "Oscar Bautista",
		carob_LLM = "Claude Sonnet 5",
		carob_date = "2026-08-25",
		carob_completion = 85,
		carob_effort = 1
	)

	f1 <- ff[basename(ff) == "APSIM-DSSAT-Calibration.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="All") # Tanzania/Ethiopia/Uganda obs+sim, no Kenya
	r2 <- carobiner::read.excel(f1, sheet="Sheet1") # Kenya obs+sim plus derived error stats (MBE/RMSE/NRMSE/NSE)
	r3 <- carobiner::read.excel(f1, sheet="APSIM") # all-country APSIM obs+sim, same values as Summary
	r4 <- carobiner::read.excel(f1, sheet="DSSAT") # all-country DSSAT obs+sim, same values as Summary
	r5 <- carobiner::read.excel(f1, sheet="ETH-DSSAT") # Ethiopia DSSAT obs+sim, subset of "All", no Year
	r6 <- carobiner::read.excel(f1, sheet="ETH-APSIM") # Ethiopia APSIM obs+sim, subset of "All", no Year
	r <- carobiner::read.excel(f1, sheet="Summary", skip=26, col_names=FALSE)[1:8,]

	d <- data.frame(
		country = r[[1]],
		adm1 = "Embu",
		location = "KARI Embu Research Farm",
		longitude = 37.4578,
		latitude = -0.5027,
		geo_uncertainty = as.numeric(NA),
		geo_source = "KARI Embu research farm, google maps",
		geo_from_source = FALSE,
		crop = "maize",
		variety = r[[2]],
		treatment = r[[2]],
		flowering_days = as.numeric(r[[3]]),
		maturity_days = as.numeric(r[[5]]),
		dmy_total = as.numeric(r[[7]]),
		yield = as.numeric(r[[9]]),
		planting_date = as.character(NA),
		harvest_date = as.character(NA),
		on_farm = FALSE
	)
	d$trial_id <- paste0("Kenya_", seq_len(nrow(d)))

	d$is_survey <- FALSE
	d$irrigated <- as.logical(NA)

	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)
	d$N_fertilizer <- as.numeric(NA)
	d$S_fertilizer <- as.numeric(NA)
	d$lime <- as.numeric(NA)

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}
