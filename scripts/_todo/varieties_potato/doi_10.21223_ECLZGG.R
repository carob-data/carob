# R script for "carob"
# license: GPL (>=3)

## NOTES
# Yield evaluation of clones at Musanze station, Rwanda (-1.500, 29.633, geo_from_source = FALSE), season 2016A.
# 05_Map_Mel_4798.xlsx gives planting date "PD: 22.3.16" (stored as planting_date = "2016-03-22").
# The dictionary states TWTG in g, TWTKG in kg, and TWTH in tonnes. Those are the same total tuber weight; stored as tuber_weight_ (kg/plot).
# NT_I, NT_II, and NT_III are tuber counts in the three size classes (<30, 30-60, >60 mm, written cm in dictionary), and NMT_I..III are weights (g).

## ISSUES
# The dictionary describes YIELD only as "yield", with no unit, and it is not a constant plot area times TWTKG; stored as yield_recorded_ rather than yield (kg/ha).
# No fertilizer rate, irrigation statement, or harvest date.

## NEW VARIABLES
# tuber_n_class1_, tuber_n_class2_, tuber_n_class3_: tuber count per plot in size classes I (<30), II (30-60), and III (>60).
# tuber_wt_class1_, tuber_wt_class2_, tuber_wt_class3_: tuber weight per plot (g) in size classes I, II, and III.
# tuber_n_total_: total tuber count per plot (NTT).
# tuber_weight_: total tuber weight per plot (kg, TWTKG).
# yield_recorded_: YIELD column as recorded (unit not stated).

## NOT PROCESSED
# 03_Summary: a summary table.
# 05_Map: a field sketch.
# 02_Data_catalogue: disease and processing notes on the clones, not plot measurements.
# TWTG and TWTH: the same weight as TWTKG in g and tonnes.
# NTP and NPH: tubers planted and tubers at harvest, with no plot area, so they are not a density.
# 02_Data_catalogue_Mel_4798.xlsx: not read. sheet Catalogue description: Number, Clone ID, Population, Processing potential, LB, PVX, PVY, PLRV, BW, NOTE. Observation records are missing from d until this file is standardized.
# 03_Summary_Mel_4798.xlsx: not read. sheet Summary table 6 clones: INSTN, NTP, YIELD, NT_III, NMT_III, NT_II, NT_I. Observation records are missing from d until this file is standardized.
# 04_Data_dictionary_Mel_4798.xlsx: documentation or a codebook, not an observation table.
# 05_Map_Mel_4798.xlsx: not read. sheet Map 2016 A: PD: 22.3.16. Observation records are missing from d until this file is standardized.


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
		response_vars = "tuber_weight_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Data_Mel_4798.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	names(r) <- trimws(names(r))
	variety <- trimws(as.character(r$INSTN))

	d <- data.frame(
		trial_id = "Musanze_2016A",
		country = "Rwanda",
		adm1 = "Northern Province",
		adm2 = "Musanze",
		location = "Musanze",
		latitude = -1.500,
		longitude = 29.633,
		planting_date = "2016-03-22",
		season = "first",
		crop = "potato",
		variety = variety,
		variety_code = variety,
		treatment = variety,
		rep = as.integer(r$REP),
		block_id = as.character(r$BLOCK),
		plot_id = as.character(r$PLOT),
		tuber_n_class1_ = as.numeric(r$NT_I),
		tuber_wt_class1_ = as.numeric(r$NMT_I),
		tuber_n_class2_ = as.numeric(r$NT_II),
		tuber_wt_class2_ = as.numeric(r$NMT_II),
		tuber_n_class3_ = as.numeric(r$NT_III),
		tuber_wt_class3_ = as.numeric(r$NMT_III),
		tuber_n_total_ = as.numeric(r$NTT),
		tuber_weight_ = as.numeric(r$TWTKG),
		yield_recorded_ = as.numeric(r$YIELD),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
