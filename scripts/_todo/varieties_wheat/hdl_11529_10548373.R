# R script for "carob"
# license: GPL (>=3)

## NOTES
# International wheat nursery processed with proc_wheat (RawData, Loc_data, EnvData).

## ISSUES
# write_files: out of bounds: irrigation_amount (2, 1800), plant_height (44, 870). NA detected: yield.

carob_script <- function(path) {

"36th International Durum Screening Nursery

International Durum Screening Nursery (IDSN) distributes diverse CIMMYT-bred spring durum wheat germplasm adapted to irrigated and variable moisture stressed environments. Disease resistance and high industrial pasta quality are essential traits possessed in this germplasm. It is distributed to 100 locations, and contains 150 entries."

	uri <- "hdl:11529/10548373"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIMMYT",
		publication = NA,
		project = "36th International Durum Screening Nursery",
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
