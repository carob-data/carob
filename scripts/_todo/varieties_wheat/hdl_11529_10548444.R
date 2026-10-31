# R script for "carob"
# license: GPL (>=3)

## NOTES
# International wheat nursery processed with proc_wheat (RawData, Loc_data, EnvData).

## ISSUES
# write_files: out of bounds: grain_Fe (0, 56.9), grain_Zn (0, 61.1). invalid terms: previous_crop: cotoon. NA detected: yield.

carob_script <- function(path) {

"6th Harvest Plus Yield Trial

The Harvest Plus Yield Trial (HPYT) contains spring bread wheat (Triticum aestivum) germplasm adapted to ME1 (Optimally Irrigated, low rainfall environment) and ME5 (Warmer area environment) environments. It has total 50 entries with 2 replications, white grain color and distributed to more than 70 locations."

	uri <- "hdl:11529/10548444"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "6th Harvest Plus Yield Trial",
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
