# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer survey of potato seed degeneration in the Andes of Ecuador. median_yield is bags of 45.45 kg harvested per bag of 45.45 kg planted, not kg/ha.
# Virus columns are the percentage of the seed lot (DAS-ELISA). farmtype is a cluster code (1, 2, 3) with no legend.

## ISSUES
# No coordinates or place names below the country.
# No kg/ha yield.
# The source has 184 rows, 181 IDs, two fully repeated rows, and one ID that occurs twice with different values. Each row keeps its own trial_id.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: farm_type_, seed_age_, yield_ratio_, and the virus percentage columns.

## NOT PROCESSED
# None. Combination columns are kept.

## NEW VARIABLES
# farm_type_: farmer typology code.
# seed_age_: how long the seed lot was kept without renewal.
# yield_ratio_: bags harvested per bag planted (45.45 kg bags).
# virus percentage columns (pvs_pct_ and the combination names): percent of the seed lot.


carob_script <- function(path) {

"
Dataset for: Seed degeneration in a Andean highland landscape

Potato seed degeneration threatens productivity of small-scale potato producers in Ecuador. For this reason, two studies were carried out a socio-economical survey and a exploratory survey to identify the presence of viruses in the seed lots of small-scale potato farmers. This relates with internal report to understand potato seed degeneration in the Andes of Ecuador.
"

	uri <- "doi:10.21223/KTWFGH"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Seed_degeneration_in_a_Andean_highland_landscape.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")
	d <- data.frame(
		hhid = as.character(r$ID),
		farm_type_ = as.character(r$farmtype),
		seed_age_ = trimws(as.character(r$timeclass)),
		yield_ratio_ = as.numeric(r$median_yield),
		pvs_pvx_pvy_plrv_pct_ = as.numeric(r$perfour),
		pvs_pvx_pvy_pct_ = as.numeric(r$persxy),
		pvs_pvx_plrv_pct_ = as.numeric(r$persxl),
		pvx_pvy_plrv_pct_ = as.numeric(r$perxyl),
		pvs_pvy_plrv_pct_ = as.numeric(r$persyl),
		pvs_pvx_pct_ = as.numeric(r$persx),
		pvs_pvy_pct_ = as.numeric(r$persy),
		pvx_pvy_pct_ = as.numeric(r$perxy),
		pvx_plrv_pct_ = as.numeric(r$perxl),
		pvs_plrv_pct_ = as.numeric(r$persl),
		pvy_plrv_pct_ = as.numeric(r$peryl),
		pvs_pct_ = as.numeric(r$pers),
		pvx_pct_ = as.numeric(r$perx),
		pvy_pct_ = as.numeric(r$pery),
		plrv_pct_ = as.numeric(r$perl),
		aplv_pct_ = as.numeric(r$peraplv),
		apmov_pct_ = as.numeric(r$perapmov),
		clean_seed_pct_ = as.numeric(r$perno_virus)
	)
	d$crop <- "potato"
	d$country <- "Ecuador"
	d$trial_id <- paste0("Ecuador_seed_", d$hhid, "_", seq_len(nrow(d)))
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$stress <- "disease"

	carobiner::write_files(path, meta, d)
}
