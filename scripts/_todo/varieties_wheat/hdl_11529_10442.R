# R script for "carob"
# license: GPL (>=3)

## NOTES
# International wheat nursery processed with proc_wheat (RawData, Loc_data, EnvData).

## ISSUES
# write_files: out of bounds: irrigation_amount (2.62, 1009.8). NA detected: yield.

carob_script <- function(path) {

"43rd International Bread Wheat Screening Nursery

The International Bread Wheat Screening Nursery (IBWSN) is designed to rapidly assess a large number of advanced generation (F3-F7) lines of spring bread wheat under Mega-environment 1 (ME1) which represents diversity for a wide range of latitudes, climates, daylengths, fertility conditions, water management, and (most importantly) disease conditions. The distribution of these nurseries is deliberately biased toward the major spring wheat regions of the world where the diseases of wheat are of high incidence. It is distributed to 180 locations and contains 300-450 entries."

	uri <- "hdl:11529/10442"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "43rd International Bread Wheat Screening Nursery",
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
