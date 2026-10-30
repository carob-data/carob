# R script for "carob"
# license: GPL (>=3)

## NOTES
# Yield evaluation of clones at Musanze station, Rwanda, season 2016A. The abstract names the site and the season.
# The dictionary states TWTG in g, TWTKG in kg, and TWTH in tonnes. Those are the same total tuber weight. tuber_weight_kg_ is TWTKG.
# NT_I, NT_II, and NT_III are tuber counts in the size classes the dictionary writes as <30 cm, 30-60 cm, and >60 cm. The matching NMT columns are weights in g.

## ISSUES
# The dictionary describes YIELD only as "yield", with no unit. It is not the same number as the kilogram or tonne columns, and it is not a constant plot area times TWTKG. It is stored as yield_recorded_ and is not mapped to yield.
# No coordinates. Musanze station still needs georeferencing.
# No fertilizer rate, irrigation statement, or a planting or harvest date. A map sheet has a column title "PD: 22.3.16", which is not used as a planting date.
# write_files: missing variables: carob_contributor, yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: season_ (season?). unknown variable: yield_recorded_ (yield?). unknown variables: tuber_n_lt_30cm_, tuber_wt_g_lt_30cm_, tuber_n_30_60cm_, tuber_wt_g_30_60cm_, tuber_n_gt_60cm_, tuber_wt_g_gt_60cm_, tuber_n_total_, tuber_weight_kg_.

## NOT PROCESSED
# 03_Summary: a summary table.
# 05_Map: a field sketch.
# 02_Data_catalogue: disease and processing notes on the clones, not plot measurements.
# TWTG and TWTH: the same weight as TWTKG in g and tonnes.
# NTP and NPH: tubers planted and tubers at harvest, with no plot area, so they are not a density.


carob_script <- function(path) {

"
Dataset for: 5 advanced clones introduced and bulked for evaluation in Rwanda

Evaluation of yield parameters of 6 clones introduced to Rwanda as processing varieties and planted in 2016A  season in Musanze station.
"

	uri <- "doi:10.21223/ECLZGG"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "tuber_weight_kg_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "01_Data_Mel_4798.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	names(r) <- trimws(names(r))
	variety <- trimws(as.character(r$INSTN))

	d <- data.frame(
		trial_id = "Musanze_2016A",
		country = "Rwanda",
		location = "Musanze",
		season_ = "2016A",
		crop = "potato",
		variety = variety,
		variety_code = variety,
		treatment = variety,
		rep = as.integer(r$REP),
		block_id = as.character(r$BLOCK),
		plot_id = as.character(r$PLOT),
		tuber_n_lt_30cm_ = as.numeric(r$NT_I),
		tuber_wt_g_lt_30cm_ = as.numeric(r$NMT_I),
		tuber_n_30_60cm_ = as.numeric(r$NT_II),
		tuber_wt_g_30_60cm_ = as.numeric(r$NMT_II),
		tuber_n_gt_60cm_ = as.numeric(r$NT_III),
		tuber_wt_g_gt_60cm_ = as.numeric(r$NMT_III),
		tuber_n_total_ = as.numeric(r$NTT),
		tuber_weight_kg_ = as.numeric(r$TWTKG),
		yield_recorded_ = as.numeric(r$YIELD),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
