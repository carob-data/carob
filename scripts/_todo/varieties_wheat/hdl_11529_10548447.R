# R script for "carob"
# license: GPL (>=3)

## NOTES
# International wheat nursery processed with proc_wheat (RawData, Loc_data, EnvData).
# Coordinates and country come from those files. geo_from_source is set inside proc_wheat.

## ISSUES
# Grain Zn is outside the vocabulary range. previous_crop "mash" is not in the crop vocabulary. Those values were not changed.
# write_files: out of bounds: grain_Zn (33.9, 55.8). invalid terms: previous_crop: mash. contributor:

carob_script <- function(path) {

"9th Harvest Plus Yield Trial

The Harvest Plus Yield Trial (HPYT) contains spring bread wheat (Triticum aestivum) germplasm adapted to ME1 (Optimally Irrigated, low rainfall environment) and ME5 (Warmer area environment) environments. It has total 50 entries with 2 replications, white grain color and distributed to more than 70 locations."

	uri <- "hdl:11529/10548447"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "9th Harvest Plus Yield Trial",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.2
	)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	carobiner::write_files(path, meta, d$wide, d$long)
}

