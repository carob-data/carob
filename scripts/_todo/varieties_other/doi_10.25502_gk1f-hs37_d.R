# R script for "carob"
# license: GPL (>=3)

## NOTES
# Advanced soybean variety trials at SEEDCO, Arcturus, Zimbabwe, 2018 (three experiments: Combined 2018AVT1, AVT2, AVT3).
# Dictionary: YIELD is grain yield after drying, kg/ha; SWT100 is grams per 100 seeds (x10 -> 1000-seed weight); PLHT is cm; DFFL and DM are days.
# yield_moisture set to 0 because the yield method is "Dry and weigh". MC (harvest seed moisture) is empty in every row.
# EXPT_DESIGN in the dictionary is RCBD with 3 replications.
# CROSS and SOURCE are identical to DESIGNATION on every row, so they were not used as pedigree or seed source.
# LOC is SEEDCO and City is Arcturus. Treated as a station trial (on_farm FALSE).

## ISSUES
# No coordinates. Georeference SEEDCO, Arcturus, Zimbabwe.
# Pod clearance (POD_CL) is 5, 10, 15 or 20 and the dictionary unit is only "Number"; kept as pod_clearance_ .
# Lodging scores include decimals (0.3 to 5); stored as stalk_lodging text.
# Disease, vigor, flower color, harvest index, plot yield, biomass, shattering, moisture and pods-per-plant columns are entirely empty.
# write_files will still report missing N/P/K fertilizer, irrigated, and harvest_date. Rates and those dates are not in the files.

## NOT PROCESSED
# data.csv GID: germplasm id, all missing.
# data.csv PL_VIGOR, FLW_COLOR, DF_P, RUST_R3, RUST_R6, BROWN_SPOT, CERPOSPORA _LS, RED_LB, BP_R3, BB, FROGEYE, SMV, HARVEST, POD_3PLANTS, PODS_PLT, PLOT_YLD, SHATTERING, MC, BIOM, Num_s_pod: all missing.
# data.csv CROSS, SOURCE: identical to DESIGNATION.
# data.csv ENTRY_NO, ENTRY_CODE: within-trial entry numbers 1-25, not a stable germplasm id (DESIGNATION is the variety name).
# abbr.csv: generic unit abbreviations, not plot data.
# Dictionary descriptions for BROWN_SPOT through BB look shifted relative to the column names; irrelevant here because those columns are empty.

## NEW VARIABLES
# pod_clearance_: dictionary "pod clearance", measured, unit not stated (values 5-20).


carob_script <- function(path) {

"
Advanced Variety Trials (AVT), Zimbabwe - 2018

Soybean (Glycine max (L.) Merrill.) is one of the most important oil crops of the world which also has tremendous importance as a food legume. The work on soybean aims at providing farmers, both commercial and subsistence, varieties with their preferred attributes to increase yield and income. These include high yield, resistance to deadly diseases, such as soybean rust, and insect pests, early maturity, good seed quality, and resistance to other stresses such as drought and soil acidity.The International Institute of Tropical Agriculture (IITA) is a key player in tropical soybean research and a partner of the Soybean Innovation Lab.
"

	uri <- "doi:10.25502/gk1f-hs37/d"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height;flowering_days;maturity_days;seed_weight",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.6,
		notes = NA
	)

	f <- ff[basename(ff) == "data.csv"]
	r <- read.csv(f, stringsAsFactors=FALSE, na.strings=c("", "NA"))

	d <- data.frame(
		trial_id = r$FILE_NAME,
		location = "Arcturus",
		country = "Zimbabwe",
		planting_date = "2018",
		rep = as.integer(r$REP_NO),
		block_id = as.character(r$BLOCK_NO),
		plot_id = as.character(r$PLOT_NO),
		variety = r$DESIGNATION,
		treatment = r$DESIGNATION,
		flowering_days = r$DFFL,
		maturity_days = r$DM,
		plant_height = r$PLHT,
		# dictionary: 100-seed weight in grams
		seed_weight = r$SWT100 * 10,
		# dictionary: dry grain yield, kg/ha
		yield = r$YIELD,
		stalk_lodging = as.character(r$LODGING),
		pod_clearance_ = r$POD_CL,
		crop = "soybean",
		yield_part = "seed",
		yield_moisture = 0,
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
