# R script for "carob"
# license: GPL (>=3)

## NOTES
# International wheat nursery processed with proc_wheat (RawData, Loc_data, EnvData).

## ISSUES
# The tables in this record are named 52ND IBWSN, not 14th Stem Rust Resistance Screening Nursery.
# write_files: out of bounds: grain_Zn (24.2, 43.8). NA detected: yield.

carob_script <- function(path) {

"14th Stem Rust Resistance Screening Nursery

The Stem Rust Resistance Screening Nursery is a single replicate nursery that contains diverse spring bread wheat (Triticum aestivum) germplasm adapted to all mega-environments with total 50-100 entries and white/red grain color."

	uri <- "hdl:11529/10548530"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "14th Stem Rust Resistance Screening Nursery",
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
