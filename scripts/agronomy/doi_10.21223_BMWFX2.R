# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"
Advanced yield trial of potato clones from the CIP late blight and heat tolerant
population (LBHT cycle 2), with check varieties and parents, evaluated at San Ramon,
Junin, Peru, in 2021. Data include tuber number and weight per plot, marketable and
total tuber yield, tuber appearance, uniformity and size scores, and dry matter.
"

	uri <- "doi:10.21223/BMWFX2"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		publication = NA,
		carob_contributor = "Preeti Bharti",
		carob_date = "2026-11-05",
		data_type = "experiment",
		data_organization = "CIP",
		project = NA,
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = "Yields in the source are in t/ha, adjusted for plot stand (TTYA, MTYA). Tuber scores, tuber counts and dry matter not yet processed.",
		design = NA,
		carob_completion = 60,
		carob_LLM = "Claude Opus 5.5"
	)

	f <- ff[basename(ff) == "01_data_advanced_trial_LBHTC2_SanRamon2021.xlsx"]
	r <- carobiner::read.excel(f, fix_names = TRUE, sheet = "SRA21-02")

	d <- data.frame(
		plot_id = as.character(r$Plot),
		rep = as.integer(r$Rep),
		variety = r$CIPN,
		treatment = r$CIPN,
		yield = r$TTYA * 1000,
		yield_marketable = r$MTYA * 1000
	)

	d$trial_id <- "SRA21-02"
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	d$planting_date <- "2021"
	d$harvest_date <- as.character(NA)
	d$yield_moisture <- 100 - r$DM.pct

	d$country <- "Peru"
	d$adm1 <- "Junin"
	d$adm2 <- "Chanchamayo"
	d$location <- "San Ramon"
	d$longitude <- -75.357
	d$latitude <- -11.124
	d$geo_from_source <- FALSE

	d$N_fertilizer <- as.numeric(NA)
	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA

	carobiner::write_files(path, meta, d)
}
