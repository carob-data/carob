# R script for "carob"
# license: GPL (>=3)

## NOTES
# International wheat nursery processed with proc_wheat (RawData, Loc_data, EnvData).
# Coordinates and country come from those files. geo_from_source is set inside proc_wheat.

## ISSUES
# proc_wheat kept records with missing yield and some planting and harvest dates that are too close or more than a year apart. Those values were not changed.
# write_files: datespan: 141 records with harvest_date within 45 days of planting_date. datespan: 141 harvest_date more than 366 days after planting_date. NA detected: yield. contributor:

carob_script <- function(path) {

"32nd Semi-Arid Wheat Screening Nursery

The Semi-Arid Wheat Screening Nursery (SAWSN) is a single replicate trial that contains diverse spring bread wheat (Triticum aestivum) germplasm adapted to low rainfall, drought prone, semi-arid environments typically receiving less than 500 mm of water available during the cropping cycle. CIMMYT'
s breeding approach attempts to combine high yield potential with drought resistance for ME4.  The combination of water-use efficiency and water responsive traits plus yield potential is important in drought environments where rainfall is frequently erratic across years. When rains are significantly above average in certain years, the crop must respond appropriately (water responsive) with higher yields, while expressing resistance to the wider suite of diseases that appear under more favorable conditions.  Constrains including leaf, stem and yellow rusts, and Septoria spp., Fusarium spp., Pyrenophora tritici-repentis tan spot, nematodes and root rots must be considered.  It is distributed to 120 locations, and contains 150-250 entries."

	uri <- "hdl:11529/10813"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "32nd Semi-Arid Wheat Screening Nursery",
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

