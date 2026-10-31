# R script for "carob"
# license: GPL (>=3)

## NOTES
# International wheat nursery processed with proc_wheat (RawData, Loc_data, EnvData).
# Coordinates and country come from those files. geo_from_source is set inside proc_wheat.

## ISSUES
# proc_wheat kept records with missing yield. Those values were not changed.
# write_files: NA detected: yield. contributor:

carob_script <- function(path) {

"40th International Durum Screening Nursery

International Durum Screening Nursery (IDSN) distributes diverse CIMMYT-bred spring durum wheat germplasm adapted to irrigated and variable moisture stressed environments. Disease resistance and high industrial pasta quality are essential traits possessed in this germplasm. It is distributed to 100 locations, and contains 150 entries. "

	uri <- "hdl:11529/10548377"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "40th International Durum Screening Nursery",
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

